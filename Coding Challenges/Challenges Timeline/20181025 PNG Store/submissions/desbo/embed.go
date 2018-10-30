package main

import (
	"encoding/binary"
	"hash/crc32"
	"io"
	"io/ioutil"
	"os"
)

//  Start of file looks like:
// |        |       IHDR chunk           |
// | header | length | type | data | CRC |
// |    8   |   4    |   4  |  13  |  4  |
// = 33 bytes
func embed(input, data, output string) {
	i := mustReadBytes(input)
	dataReader, err := os.Open(data)

	if err != nil {
		panic(err)
	}

	png := make([]byte, 33)
	copy(png, i)
	end := i[33:]
	dataChunks, err := chunk(dataReader)

	if err != nil {
		panic(err)
	}

	png = append(png, filenameChunk(data).encode()...)

	for _, c := range dataChunks {
		png = append(png, c.encode()...)
	}

	png = append(png, end...)

	if err := ioutil.WriteFile(output, png, os.ModePerm); err != nil {
		panic(err)
	}
}

func filenameChunk(filename string) *dataChunk {
	return &dataChunk{
		Type:   filenameChunkType,
		Length: uint32(len(filename)),
		Data:   []byte(filename),
	}
}

func (dc *dataChunk) checksum() uint32 {
	crc := crc32.NewIEEE()
	_, err := crc.Write([]byte(dc.Type))

	if err != nil {
		panic(err)
	}

	_, err = crc.Write(dc.Data)

	if err != nil {
		panic(err)
	}

	return crc.Sum32()
}

func (dc *dataChunk) encode() []byte {
	chunk := make([]byte, 0)

	length := make([]byte, 4)
	binary.BigEndian.PutUint32(length, dc.Length)

	typ := []byte(dc.Type[:4])

	data := dc.Data

	crc := make([]byte, 4)
	binary.BigEndian.PutUint32(crc, dc.checksum())

	chunk = append(chunk, length...)
	chunk = append(chunk, typ...)
	chunk = append(chunk, data...)
	chunk = append(chunk, crc...)

	return chunk
}

func chunk(data io.Reader) ([]*dataChunk, error) {
	chunks := make([]*dataChunk, 0)
	chunkData := make([]byte, maxChunkLength)

	for {
		n, err := data.Read(chunkData)

		if err != nil {
			return nil, err
		}

		data := chunkData
		chunks = append(chunks, &dataChunk{
			Type:   dataChunkType,
			Length: uint32(n),
			Data:   data,
		})

		if n < maxChunkLength {
			return chunks, nil
		}
	}
}

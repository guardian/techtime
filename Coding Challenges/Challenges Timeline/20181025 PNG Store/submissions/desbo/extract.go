package main

import (
	"encoding/binary"
	"io/ioutil"
	"os"
)

func extract(src string) {
	var filename string
	data := make([]byte, 0)
	d := mustReadBytes(src)
	pos := 33

	for {
		length := binary.BigEndian.Uint32(d[pos : pos+4])
		pos += 4

		typ := string(d[pos : pos+4])
		pos += 4

		dat := d[pos : pos+int(length)]
		pos += int(length)

		// skip CRC
		pos += 4

		if typ == filenameChunkType {
			filename = string(dat)
			continue
		}

		if typ == dataChunkType {
			data = append(data, dat...)
			continue
		}

		break
	}

	if err := ioutil.WriteFile(filename, data, os.ModePerm); err != nil {
		panic(err)
	}
}

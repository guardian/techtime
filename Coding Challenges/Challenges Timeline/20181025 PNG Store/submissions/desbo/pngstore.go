package main

import (
	"io/ioutil"
	"log"
	"math"
	"os"
)

type dataChunk struct {
	Type   string
	Length uint32
	Data   []byte
}

const maxChunkLength = math.MaxUint32
const dataChunkType = "xdAt"
const filenameChunkType = "xnAm"

func main() {
	if len(os.Args) < 2 {
		log.Fatal("use embed or extract")
	}

	switch os.Args[1] {
	case "embed":
		if len(os.Args) < 5 {
			log.Fatal("usage: pngstore embed source data output")
		}

		embed(os.Args[2], os.Args[3], os.Args[4])
	case "extract":
		if len(os.Args) < 3 {
			log.Fatal("usage: pngstore extract source")
		}

		extract(os.Args[2])
	default:
		log.Fatal("invalid arguments")
	}
}

func mustReadBytes(filename string) []byte {
	f, err := os.Open(filename)

	if err != nil {
		panic(err)
	}

	b, err := ioutil.ReadAll(f)

	if err != nil {
		panic(err)
	}

	return b
}

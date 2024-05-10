package main

import (
	"fmt"
	"io"
	"os"
)

const (
	// Part one
	// uniqueChars = 4
	// Part two
	uniqueChars = 14
)

func main() {
	bytes, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	str := string(bytes)

outerLoop:
	for i := uniqueChars - 1; i < len(str); i++ {
		for a := 0; a < uniqueChars; a++ {
			for b := a + 1; b < uniqueChars; b++ {
				if str[i-a] == str[i-b] {
					continue outerLoop
				}
			}
		}
		fmt.Println("Solution:", i+1)
		return
	}
}

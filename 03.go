package main

import (
	"bufio"
	"fmt"
	"os"
)

func getPriority(r rune) int {
	if r >= 'a' {
		return 1 + int(r-'a')
	} else {
		return 27 + int(r-'A')
	}
}

func main() {
	score := 0
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		first := scanner.Text()
		scanner.Scan()
		second := scanner.Text()
		scanner.Scan()
		third := scanner.Text()

	outer:
		for _, a := range first {
			for _, b := range second {
				for _, c := range third {
					if a == b && b == c {
						score += getPriority(a)
						break outer
					}
				}
			}
		}
	}

	fmt.Println("Solution:", score)
}

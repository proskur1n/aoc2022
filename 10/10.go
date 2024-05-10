package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// Part two
func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)

	x := 1
	opCycles := 0
	add := 0
	horizontal := 0

	for {
		if opCycles == 0 {
			x += add
			if !scanner.Scan() {
				break
			}
			if scanner.Text() == "noop" {
				add = 0
				opCycles = 1
			} else {
				scanner.Scan()
				add, _ = strconv.Atoi(scanner.Text())
				opCycles = 2
			}
		}
		opCycles--

		if horizontal == x-1 || horizontal == x || horizontal == x+1 {
			fmt.Print("#")
		} else {
			fmt.Print(".")
		}
		horizontal = (horizontal + 1) % 40
		if horizontal == 0 {
			fmt.Println()
		}
	}
}

// Part one
// func main() {
// 	x := 1
// 	cycle := 1
// 	cycleSync := 20
// 	sum := 0
// 	scanner := bufio.NewScanner(os.Stdin)
// 	scanner.Split(bufio.ScanWords)
// 	for scanner.Scan() {
// 		cmd := scanner.Text()
// 		before := x
// 		if cmd == "noop" {
// 			cycle += 1
// 		} else {
// 			scanner.Scan()
// 			i, _ := strconv.Atoi(scanner.Text())
// 			x += i
// 			cycle += 2
// 		}
// 		if cycle == cycleSync {
// 			sum += x * cycleSync
// 			cycleSync += 40
// 		} else if cycle > cycleSync {
// 			sum += before * cycleSync
// 			cycleSync += 40
// 		}
// 	}
// 	fmt.Println("Solution:", sum)
// }

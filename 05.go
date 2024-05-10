package main

import "fmt"

func main() {
	stacks := [9]string{
		"QWPSZRHD",
		"VBRWQHF",
		"CVSH",
		"HFG",
		"PGJBZ",
		"QTJHWFL",
		"ZTWDLVJN",
		"DTZCJGHF",
		"WPVMBH",
	}
	for {
		var amount, from, to int
		if _, err := fmt.Scanf("move %d from %d to %d\n", &amount, &from, &to); err != nil {
			break
		}
		from--
		to--
		// Part one
		// for i := 0; i < amount; i++ {
		// 	stacks[to] = stacks[to] + string(stacks[from][len(stacks[from]) - 1])
		// 	stacks[from] = stacks[from][:len(stacks[from]) - 1]
		// }
		// Part two
		left := len(stacks[from]) - amount
		stacks[to] = stacks[to] + stacks[from][left:]
		stacks[from] = stacks[from][:left]
	}
	fmt.Print("Solution: ")
	for _, crates := range stacks {
		if len(crates) == 0 {
			fmt.Printf("%c", '_')
		} else {
			fmt.Printf("%c", crates[len(crates)-1])
		}
	}
	fmt.Println()
}

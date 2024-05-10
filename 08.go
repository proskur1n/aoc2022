package main

import (
	"bufio"
	"fmt"
	"os"
)

func solvePartOne(input []string) (solution int) {
	var visible [][]bool
	for _, line := range input {
		visible = append(visible, make([]bool, len(line)))
	}

	for y := 0; y < len(input); y++ {
		line := input[y]
		for x, max := 0, byte(0); x < len(line); x++ {
			if max < line[x] {
				visible[y][x] = true
				max = line[x]
			}
		}
		for x, max := len(line)-1, byte(0); x >= 0; x-- {
			if max < line[x] {
				visible[y][x] = true
				max = line[x]
			}
		}
	}

	for x := 0; x < len(input[0]); x++ {
		for y, max := 0, byte(0); y < len(input); y++ {
			if max < input[y][x] {
				visible[y][x] = true
				max = input[y][x]
			}
		}
		for y, max := len(input)-1, byte(0); y >= 0; y-- {
			if max < input[y][x] {
				visible[y][x] = true
				max = input[y][x]
			}
		}
	}

	for _, row := range visible {
		for _, v := range row {
			if v {
				solution++
			}
		}
	}
	return solution
}

func getScenicScore(input []string, x, y int) int {
	score := 1
	treeHouseHeight := input[y][x]
	for i := x - 1; ; i-- {
		if i == 0 || input[y][i] >= treeHouseHeight {
			score *= x - i
			break
		}
	}
	for i := x + 1; ; i++ {
		if i == len(input[0])-1 || input[y][i] >= treeHouseHeight {
			score *= i - x
			break
		}
	}
	for i := y - 1; ; i-- {
		if i == 0 || input[i][x] >= treeHouseHeight {
			score *= y - i
			break
		}
	}
	for i := y + 1; ; i++ {
		if i == len(input)-1 || input[i][x] >= treeHouseHeight {
			score *= i - y
			break
		}
	}
	return score
}

func solvePartTwo(input []string) (solution int) {
	for y := 1; y < len(input)-1; y++ {
		for x := 1; x < len(input[0])-1; x++ {
			score := getScenicScore(input, x, y)
			if score > solution {
				solution = score
			}
		}
	}
	return
}

func main() {
	var input []string
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}
	// fmt.Println("Solution:", solvePartOne(input))
	fmt.Println("Solution:", solvePartTwo(input))
}

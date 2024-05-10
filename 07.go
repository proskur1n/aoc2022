package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Directory struct {
	size     int // Size of all files (excluding directories)
	children map[string]*Directory
}

func parseInput() (root Directory) {
	root.children = make(map[string]*Directory)
	stack := []*Directory{&root}
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	scanner.Scan() // Skip first "$"

	for scanner.Scan() {
		switch scanner.Text() {
		case "cd":
			scanner.Scan()
			switch dir := scanner.Text(); dir {
			case "..":
				stack = stack[:len(stack)-1]
			case "/":
				stack = stack[:1]
			default:
				entry, ok := stack[len(stack)-1].children[dir]
				if !ok {
					entry = &Directory{0, make(map[string]*Directory)}
					stack[len(stack)-1].children[dir] = entry
				}
				stack = append(stack, entry)
			}
			scanner.Scan() // Skip next "$"
		case "ls":
			if stack[len(stack)-1].size > 0 {
				continue
			}
			for scanner.Scan() {
				txt := scanner.Text()
				if txt == "$" {
					break
				}
				if txt != "dir" {
					size, _ := strconv.Atoi(txt)
					stack[len(stack)-1].size += size
				}
				scanner.Scan() // Skip file or directory name
			}
		default:
			panic("Bad command")
		}
	}

	return
}

func solvePart1(root Directory) (solution, totalSize int) {
	for _, child := range root.children {
		sol, ts := solvePart1(*child)
		solution += sol
		totalSize += ts
	}
	totalSize += root.size
	if totalSize < 100000 {
		solution += totalSize
	}
	return
}

func solvePart2(root Directory, needed int) (solution, totalSize int) {
	solution = math.MaxInt
	totalSize = root.size
	for _, child := range root.children {
		sol, ts := solvePart2(*child, needed)
		if sol > 0 && sol < solution {
			solution = sol
		}
		totalSize += ts
	}

	if solution != math.MaxInt {
		return solution, totalSize
	}
	if totalSize >= needed {
		return totalSize, totalSize
	}
	return 0, totalSize
}

func getTotalSize(root Directory) (totalSize int) {
	for _, child := range root.children {
		totalSize += getTotalSize(*child)
	}
	return totalSize + root.size
}

// For debugging purposes
func printTree(root Directory, indent int) {
	fmt.Printf("%s- %d\n", strings.Repeat(" ", indent), root.size)
	for _, child := range root.children {
		printTree(*child, indent+2)
	}
}

func main() {
	root := parseInput()
	// Part one
	// solution, _ := solvePart1(root)
	// Part two
	solution, _ := solvePart2(root, getTotalSize(root)-40000000)
	fmt.Println("Solution:", solution)
}

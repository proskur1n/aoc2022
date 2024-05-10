package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type vec2 struct {
	x, y int
}

func sign(x int) int {
	if x < 0 {
		return -1
	}
	if x > 0 {
		return 1
	}
	return 0
}

func moveSegment(head, tail vec2) vec2 {
	x := tail.x + sign(head.x-tail.x)
	y := tail.y + sign(head.y-tail.y)
	if x == head.x && y == head.y {
		return tail
	}
	return vec2{x, y}
}

func main() {
	rope := [10]vec2{} // rope[0] -> head
	visited := map[vec2]bool{}
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)

	for scanner.Scan() {
		cmd := scanner.Text()
		scanner.Scan()
		amount, _ := strconv.Atoi(scanner.Text())

		for i := 0; i < amount; i++ {
			switch cmd {
			case "U":
				rope[0].y++
			case "R":
				rope[0].x++
			case "D":
				rope[0].y--
			case "L":
				rope[0].x--
			}

			for i := 1; i < len(rope); i++ {
				rope[i] = moveSegment(rope[i-1], rope[i])
			}
			visited[rope[len(rope)-1]] = true
		}
	}

	fmt.Println("Solution:", len(visited))
}

// For debugging purposes
func printRope(rope []vec2) {
	minX, maxX := 0, 0
	minY, maxY := 0, 0
	for _, s := range rope {
		if s.x < minX {
			minX = s.x
		} else if s.x > maxX {
			maxX = s.x
		}
		if s.y < minY {
			minY = s.y
		} else if s.y > maxY {
			maxY = s.y
		}
	}

	width := maxX - minX + 1
	height := maxY - minY + 1

	for y := height - 1; y >= 0; y-- {
		for x := 0; x < width; x++ {
			c := '.'
			for i, s := range rope {
				if s.x-minX == x && s.y-minY == y {
					c = rune('0' + i)
					break
				}
			}
			if c == '0' {
				c = 'H'
			}
			fmt.Printf("%c", c)
		}
		fmt.Println()
	}
	fmt.Println()
}

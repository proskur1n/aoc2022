package main

import (
	"fmt"
)

func modulo(a, b int) int {
	return (a%b + b) % b
}

func main() {
	score := 0

	for {
		var opponent, round rune
		if _, err := fmt.Scanf("%c %c\n", &opponent, &round); err != nil {
			break
		}

		score += int(round-'X') * 3 // Lose, Draw, Win
		score += modulo(int(opponent-'A'+(round-'Y')), 3) + 1
	}

	fmt.Println("Solution:", score)
}

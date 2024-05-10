package main

import (
	"fmt"
	"io"
)

func main() {
	var maxCalories [3]int
	calories := 0

	for {
		c := 0
		_, err := fmt.Scanln(&c)
		if err == io.EOF {
			break
		} else if err != nil {
			for i, val := range maxCalories {
				if calories > val {
					maxCalories[i] = calories
					calories = val
				}
			}
			calories = 0
		} else {
			calories += c
		}
	}

	fmt.Println("Solution:", maxCalories[0]+maxCalories[1]+maxCalories[2])
}

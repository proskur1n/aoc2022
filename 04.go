package main

import "fmt"

func main() {
	count := 0
	for {
		var a, b, x, y int
		if _, err := fmt.Scanf("%d-%d,%d-%d\n", &a, &b, &x, &y); err != nil {
			break
		}
		// Part one
		// if (a <= x && y <= b) || (x <= a && b <= y) {
		// 	count++
		// }
		// Part two
		if (a <= x && b >= x) || (x <= a && y >= a) {
			count++
		}
	}
	fmt.Println("Solution:", count)
}

package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type tile uint8

const (
	void tile = iota
	open
	solid
)

// A non-negative instruction represents the number of steps to go forward. Negative instructions
// are magic numbers to turn left and right.
type instruction int

const (
	turnLeft  instruction = -1
	turnRight instruction = -2
)

type monkeyMap struct {
	grid          [][]tile
	width, height int
}

func parseInput(filePath string) (monkeyMap, []instruction, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return monkeyMap{}, nil, err
	}
	defer file.Close()

	longestLine := 0
	var grid [][]tile

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}

		if len(line) > longestLine {
			longestLine = len(line)
		}

		row := make([]tile, longestLine)
		for i, c := range line {
			switch c {
			case ' ':
			case '.':
				row[i] = open
			case '#':
				row[i] = solid
			default:
				return monkeyMap{}, nil, fmt.Errorf("Invalid character in monkey map description '%c'", c)
			}
		}
		grid = append(grid, row)
	}

	if !scanner.Scan() {
		return monkeyMap{}, nil, fmt.Errorf("Unexpected end of file")
	}
	pathString := scanner.Text()
	if scanner.Scan() {
		return monkeyMap{}, nil, fmt.Errorf("File is too large")
	}

	err = scanner.Err()
	if err != nil {
		return monkeyMap{}, nil, err
	}

	for i, row := range grid {
		if len(grid) < longestLine {
			grid[i] = append(row, make([]tile, longestLine-len(grid))...)
		}
	}

	var path []instruction
	for _, match := range regexp.MustCompile(`\d+|L|R`).FindAllString(pathString, -1) {
		switch match {
		case "L":
			path = append(path, turnLeft)
		case "R":
			path = append(path, turnRight)
		default:
			steps, _ := strconv.Atoi(match)
			path = append(path, instruction(steps))
		}
	}

	return monkeyMap{
		grid:   grid,
		width:  longestLine,
		height: len(grid),
	}, path, nil
}

type walker interface {
	// turnLeft turns the walker 90 degrees to the left.
	turnLeft()
	// turnRight turns the walker 90 degrees to the right.
	turnRight()
	// moveForwardOnce tries to move the walker in the forward direction by one tile, wrapping
	// around the map as needed. Returns false if the new position would be a solid tile. Walker's
	// coordinates are not changed in this case.
	moveForwardOnce() bool
	// coordinates returns the (row, column, direction) triple for the monkey map. Row and column
	// are 1-based.
	coordinates() (int, int, direction)
}

type direction int

const (
	right direction = iota
	down
	left
	up
)

type password int

func followPath(walker walker, path []instruction) password {
	for _, inst := range path {
		switch inst {
		case turnLeft:
			walker.turnLeft()
		case turnRight:
			walker.turnRight()
		default:
			for i := 0; i < int(inst); i++ {
				if !walker.moveForwardOnce() {
					break
				}
			}
		}
	}
	row, column, direction := walker.coordinates()
	return password(1000*row + 4*column + int(direction))
}

type monkeyMapWalker struct {
	mmap   monkeyMap
	x, y   int // 0-based
	dx, dy int
}

func newMonkeyMapWalker(mmap monkeyMap) *monkeyMapWalker {
	var x, y int
loop:
	for ; y < mmap.height; y++ {
		for ; x < mmap.width; x++ {
			if mmap.grid[y][x] == open {
				break loop
			}
		}
	}
	if x >= mmap.width || y >= mmap.height {
		panic("Invalid starting position") // Sanity check
	}

	return &monkeyMapWalker{
		mmap: mmap,
		x:    x,
		y:    y,
		dx:   1,
		dy:   0,
	}
}

func (w *monkeyMapWalker) turnLeft() {
	w.dx, w.dy = w.dy, -w.dx
}

func (w *monkeyMapWalker) turnRight() {
	w.dx, w.dy = -w.dy, w.dx
}

func (w *monkeyMapWalker) moveForwardOnce() bool {
	x, y := w.x, w.y
	for {
		x = mod(x+w.dx, w.mmap.width)
		y = mod(y+w.dy, w.mmap.height)
		switch w.mmap.grid[y][x] {
		case void:
			continue
		case open:
			w.x = x
			w.y = y
			return true
		case solid:
			return false
		default:
			panic("Unreachable")
		}
	}
}

func (w *monkeyMapWalker) coordinates() (int, int, direction) {
	return w.y + 1, w.x + 1, dxDyToDirection(w.dx, w.dy)
}

func dxDyToDirection(dx, dy int) direction {
	if dy == 0 && dx == -1 {
		return left
	} else if dy == 0 && dx == 1 {
		return right
	} else if dy == -1 && dx == 0 {
		return up
	} else if dy == 1 && dx == 0 {
		return down
	} else {
		panic("Invalid (dx, dy) pair")
	}
}

// cubeWalker is hardcoded for my particular input. It doesn't even work for the provided example.
type cubeWalker struct {
	mmap monkeyMap
	pos  vec3
	dir  vec3 // Movement direction
	nor  vec3 // Current face normal
	side int  // Cube side length
}

func newCubeWalker(mmap monkeyMap) *cubeWalker {
	return &cubeWalker{
		mmap: mmap,
		pos:  vec3{1, 1, 1},
		dir:  vec3{1, 0, 0},
		nor:  vec3{0, 0, -1},
		side: 50, // Hardcoded
	}
}

func (w *cubeWalker) turnLeft() {
	w.dir = crossProduct(w.nor, w.dir)
}

func (w *cubeWalker) turnRight() {
	w.dir = crossProduct(w.dir, w.nor)
}

func (w *cubeWalker) moveForwardOnce() bool {
	pos := add(w.pos, w.dir)
	dir := w.dir
	nor := w.nor

	if pos.x == 0 || pos.x == w.side+1 || pos.y == 0 || pos.y == w.side+1 || pos.z == 0 || pos.z == w.side+1 {
		// Wrap around edge of the cube.
		pos = w.pos
		dir, nor = negate(nor), dir
	}

	row, column, _ := cubeCoordinatesToMonkeyMapCoordinates(pos, dir, nor, w.side)
	switch w.mmap.grid[row-1][column-1] {
	case void:
		panic("Invalid (x, y, z) triple") // Sanity check
	case open:
		w.pos = pos
		w.dir = dir
		w.nor = nor
		return true
	case solid:
		return false
	default:
		panic("Unreachable")
	}
}

func (w *cubeWalker) coordinates() (int, int, direction) {
	return cubeCoordinatesToMonkeyMapCoordinates(w.pos, w.dir, w.nor, w.side)
}

func cubeCoordinatesToMonkeyMapCoordinates(pos, dir, nor vec3, side int) (int, int, direction) {
	var row, column int
	var mapDir direction

	// I absolutely hate this puzzle ...
	switch nor {
	case vec3{0, 0, -1}:
		row, column = pos.y, side+pos.x
		mapDir = dxDyToDirection(dir.x, dir.y)
	case vec3{1, 0, 0}:
		row, column = pos.y, 2*side+pos.z
		mapDir = dxDyToDirection(dir.z, dir.y)
	case vec3{0, 1, 0}:
		row, column = side+pos.z, side+pos.x
		mapDir = dxDyToDirection(dir.x, dir.z)
	case vec3{0, 0, 1}:
		row, column = 3*side+1-pos.y, side+pos.x
		mapDir = dxDyToDirection(dir.x, -dir.y)
	case vec3{-1, 0, 0}:
		row, column = 3*side+1-pos.y, pos.z
		mapDir = dxDyToDirection(dir.z, -dir.y)
	case vec3{0, -1, 0}:
		row, column = 3*side+pos.x, pos.z
		mapDir = dxDyToDirection(dir.z, dir.x)
	default:
		panic("Invalid (nx, ny, nz) triple")
	}

	return row, column, mapDir
}

func mod(x, y int) int {
	return (x%y + y) % y
}

type vec3 struct {
	x, y, z int
}

func crossProduct(a, b vec3) vec3 {
	return vec3{a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x}
}

func add(a, b vec3) vec3 {
	return vec3{a.x + b.x, a.y + b.y, a.z + b.z}
}

func negate(v vec3) vec3 {
	return vec3{-v.x, -v.y, -v.z}
}

func main() {
	monkeyMap, path, err := parseInput("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part One:", followPath(newMonkeyMapWalker(monkeyMap), path))
	fmt.Println("Part Two:", followPath(newCubeWalker(monkeyMap), path))
}

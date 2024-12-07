package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 06
* Go Solution
*
* Day 6: Guard Gallivant
*
* https://adventofcode.com/2024/day/6
*
 */

import (
	"fmt"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

func (s Set[T]) Clone() Set[T] {
	clonedSet := make(Set[T])
	for key := range s {
		clonedSet[key] = struct{}{}
	}
	return clonedSet
}

type Direction int

const (
	UP Direction = iota
	RIGHT
	DOWN
	LEFT
)

type Guard struct {
	y int
	x int
	d Direction
}

func (g *Guard) Visit(l *Lab) {
	l.visited.Add(g.Clone())
}

func (g Guard) Clone() Guard {
	return Guard{
		y: g.y,
		x: g.x,
		d: g.d,
	}
}

func (g *Guard) Move(l *Lab) {
	var ny, nx int
	var nr Direction
	switch l.guard.d {
	case UP:
		ny, nx = l.guard.y-1, l.guard.x
		nr = RIGHT
	case RIGHT:
		ny, nx = l.guard.y, l.guard.x+1
		nr = DOWN
	case DOWN:
		ny, nx = l.guard.y+1, l.guard.x
		nr = LEFT
	case LEFT:
		ny, nx = l.guard.y, l.guard.x-1
		nr = UP
	}

	if l.Available(ny, nx) {
		g.move(ny, nx)
	} else {
		g.Rotate(nr)
	}
}

func (g *Guard) move(ny int, nx int) {
	g.y, g.x = ny, nx
}

func (g *Guard) Rotate(d Direction) {
	g.d = d
}

type Lab struct {
	cells   [][]bool
	visited Set[Guard]
	height  int
	width   int
	guard   Guard
}

func (l Lab) Parse(input_data string) *Lab {
	rows := strings.Split(input_data, "\n")
	l.visited = make(Set[Guard])
	l.height, l.width = len(rows), len(rows[0])
	l.cells = make([][]bool, l.height)
	for i, row := range rows {
		l.cells[i] = make([]bool, l.width)
		for j, cell := range row {
			if cell == '.' {
				l.cells[i][j] = true
			} else if cell == '^' {
				l.cells[i][j] = true
				l.guard = Guard{i, j, UP}
			}
		}
	}
	return &l
}

func (l *Lab) Visited() int {
	v := make(Set[struct{ y, x int }])
	for k := range l.visited {
		v.Add(struct{ y, x int }{k.y, k.x})
	}
	return len(v)
}

func (l *Lab) Available(y int, x int) bool {
	return y < 0 || y >= l.height || x < 0 || x >= l.width || l.cells[y][x]
}

func (l *Lab) GuardInSight() bool {
	return (l.guard.y >= 0 && l.guard.y < l.height) && (l.guard.x >= 0 && l.guard.x < l.width)
}

func (l *Lab) Update() {
	l.guard.Visit(l)
	l.guard.Move(l)
}

func (l *Lab) Print() {
	for _, row := range l.cells {
		for _, c := range row {
			if c {
				fmt.Print(".")
			} else {
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
}

func (l *Lab) Clone() Lab {
	cells := make([][]bool, len(l.cells))
	for i := range l.cells {
		cells[i] = append([]bool(nil), l.cells[i]...)
	}
	return Lab{
		cells:   cells,
		visited: l.visited.Clone(),
		height:  l.height,
		width:   l.width,
		guard:   l.guard,
	}
}

func (l *Lab) IsLoop() bool {
	for ; l.GuardInSight(); l.Update() {
		if l.visited.Contains(l.guard) {
			return true
		}
	}
	return false
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	lab := Lab{}.Parse(strings.TrimSpace(input_data))
	for ; lab.GuardInSight(); lab.Update() {
	}
	return lab.Visited(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	lab := Lab{}.Parse(strings.TrimSpace(input_data))

	acc := 0
	for y := range lab.height {
		for x := range lab.width {
			clone := lab.Clone()
			if !clone.Available(y, x) || (clone.guard.y == y && clone.guard.x == x) {
				continue
			}
			clone.cells[y][x] = false
			if clone.IsLoop() {
				acc++
			}
		}
	}
	return acc, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

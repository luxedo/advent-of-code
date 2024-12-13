package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 12
* Go Solution
*
* Day 12: Garden Groups
*
* https://adventofcode.com/2024/day/12
*
 */

import (
	"errors"
	// "fmt"
	"sort"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

var directions = []Point{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}

type Stack[T any] struct {
	data []T
}

func (s *Stack[T]) Push(value T) {
	s.data = append(s.data, value)
}

func (s *Stack[T]) Pop() (T, bool) {
	if s.Size() == 0 {
		var zero T
		return zero, false
	}
	value := s.data[len(s.data)-1]
	s.data = s.data[:len(s.data)-1]
	return value, true
}

func (s *Stack[T]) Size() int {
	return len(s.data)
}

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

func (s Set[T]) Subtract(other Set[T]) {
	for g := range other {
		delete(s, g)
	}
}

func (s Set[T]) Sample() T {
	var key T
	for key = range s {
		break
	}
	return key
}

func (s Set[T]) Size() int {
	return len(s)
}

type Point struct {
	y, x int
}

func (p Point) Add(q Point) Point {
	return Point{p.y + q.y, p.x + q.x}
}

type Points []Point

func (p Points) Len() int           { return len(p) }
func (p Points) Less(i, j int) bool { return p[i].y < p[j].y || (p[i] == p[j] && p[i].x < p[j].x) }
func (p Points) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }
func (p Points) Find(item Point) (int, error) {
	for i, value := range p {
		if value == item {
			return i, nil
		}
	}
	return 0, errors.New("Point not found")
}

func (p *Points) RemoveIndices(indices []int) {
	sort.Sort(sort.Reverse(sort.IntSlice(indices)))
	for _, index := range indices {
		*p = append((*p)[:index], (*p)[index+1:]...)
	}
}

type Plot struct {
	region rune
	plot   Set[Point]
}

func (p *Plot) Area() int {
	return len(p.plot)
}

func (p *Plot) Perimeter() int {
	perimeter := 0
	for point := range p.plot {
		for _, offset := range directions {
			neighbor := Point{point.y + offset.y, point.x + offset.x}
			if !p.plot.Contains(neighbor) {
				perimeter++
			}
		}
	}
	return perimeter
}

func (p *Plot) Sides() int {
	sides := make([]Points, len(directions))
	for point := range p.plot {
		for i, offset := range directions {
			neighbor := Point{point.y + offset.y, point.x + offset.x}
			if !p.plot.Contains(neighbor) {
				sides[i] = append(sides[i], point)
			}
		}
	}
	for i, dir := range directions {
		indices := []int{}
		sort.Sort(sides[i])
		dir = Point{dir.x, -dir.y}
		for _, side := range sides[i] {
			needle := side.Add(dir)
			if idx, err := sides[i].Find(needle); err == nil {
				indices = append(indices, idx)
			}
		}
		sides[i].RemoveIndices(indices)
	}
	acc := 0
	for _, side := range sides {
		acc += len(side)
	}
	return acc
}

type Garden []Plot

func (g Garden) Parse(input_data string) *Garden {
	missing := Set[Point]{}
	patches := [][]rune{}
	for y, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		row := []rune{}
		for x, c := range line {
			missing.Add(Point{y, x})
			row = append(row, c)
		}
		patches = append(patches, row)
	}
	for missing.Size() > 0 {
		s := missing.Sample()
		plot := g.FloodFill(patches, s)
		g = append(g, plot)

		plotPoints := Set[Point]{}
		for p := range plot.plot {
			plotPoints.Add(p)
		}
		missing.Subtract(plotPoints)
	}
	return &g
}

func (g Garden) FloodFill(patches [][]rune, p Point) Plot {
	stack := Stack[Point]{}
	stack.Push(p)

	seen := Set[Point]{}
	seen.Add(p)

	region := patches[p.y][p.x]
	height, width := len(patches), len(patches[0])

	for stack.Size() > 0 {
		p, _ = stack.Pop()
		for _, offset := range directions {
			y, x := p.y+offset.y, p.x+offset.x
			point := Point{y, x}
			if y < 0 || y >= height || x < 0 || x >= width {
				continue
			}
			if patches[y][x] != region {
				continue
			}
			if seen.Contains(point) {
				continue
			}
			seen.Add(point)
			stack.Push(point)
		}
	}

	return Plot{region, seen}
}

func (g *Garden) PricePerimeter() int {
	acc := 0
	for _, plot := range *g {
		acc += plot.Area() * plot.Perimeter()
	}
	return acc
}

func (g *Garden) PriceSides() int {
	acc := 0
	for _, plot := range *g {
		acc += plot.Area() * plot.Sides()
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	garden := Garden{}.Parse(input_data)
	return garden.PricePerimeter(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	garden := Garden{}.Parse(input_data)
	return garden.PriceSides(), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

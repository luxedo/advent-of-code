package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 14
* Go Solution
*
* Day 14: Restroom Redoubt
*
* https://adventofcode.com/2024/day/14
*
 */

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Point             struct{ x, y int }
	Guard             struct {
		p Point
		v Point
	}
	BathroomSecurity struct {
		height, width int
		guards        []Guard
		time          int
	}
)

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

func (g Guard) Parse(line string) Guard {
	re := regexp.MustCompile(`p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)`)
	matches := re.FindStringSubmatch(line)
	g.p.x, _ = strconv.Atoi(matches[1])
	g.p.y, _ = strconv.Atoi(matches[2])
	g.v.x, _ = strconv.Atoi(matches[3])
	g.v.y, _ = strconv.Atoi(matches[4])
	return g
}

func (b BathroomSecurity) Parse(input_data string) *BathroomSecurity {
	for _, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		b.guards = append(b.guards, Guard{}.Parse(line))
	}
	return &b
}

func (b *BathroomSecurity) String() string {
	m := make([][]rune, b.height)
	for y := range b.height {
		m[y] = make([]rune, b.width)
		for x := range b.width {
			m[y][x] = '.'
		}
	}
	for _, guard := range b.guards {
		if m[guard.p.y][guard.p.x] == '.' {
			m[guard.p.y][guard.p.x] = '1'
		} else {
			m[guard.p.y][guard.p.x]++
		}
	}
	var s string
	for _, row := range m {
		for _, r := range row {
			s += string(r)
		}
		s += "\n"
	}
	return s
}

func (b *BathroomSecurity) Move(time int) {
	b.time += time
	for i, guard := range b.guards {
		guard.p.x += time * guard.v.x
		guard.p.y += time * guard.v.y
		guard.p.x = (guard.p.x%b.width + b.width) % b.width
		guard.p.y = (guard.p.y%b.height + b.height) % b.height
		b.guards[i] = guard
	}
}

func (b *BathroomSecurity) SafetyFactor() int {
	h2, w2 := b.height/2, b.width/2
	q1, q2, q3, q4 := 0, 0, 0, 0
	for _, guard := range b.guards {
		if guard.p.y < h2 && guard.p.x < w2 {
			q1++
		} else if guard.p.y < h2 && guard.p.x > w2 {
			q2++
		} else if guard.p.y > h2 && guard.p.x < w2 {
			q3++
		} else if guard.p.y > h2 && guard.p.x > w2 {
			q4++
		} else {
			// Guard is in the line
		}
	}
	return q1 * q2 * q3 * q4
}

func (b *BathroomSecurity) CheckEasterEgg() bool {
	pointsy := make([]Set[Point], b.height)
	for y := range b.height {
		pointsy[y] = Set[Point]{}
	}
	for _, guard := range b.guards {
		pointsy[guard.p.y].Add(guard.p)
	}

	for y := range b.height {
		if len(pointsy[y]) > 30 {
			seq := 0
			for py := range pointsy[y] {
				p := Point{y: y, x: py.x + 1}
				if pointsy[y].Contains(p) {
					seq++
				}
			}
			if seq > 25 {
				return true
			}
		}
	}
	return false
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	height, width := 103, 101
	if len(args) != 0 {
		width, _ = strconv.Atoi(args[0])
		height, _ = strconv.Atoi(args[1])
	}
	bsecurity := BathroomSecurity{height: height, width: width}.Parse(input_data)
	bsecurity.Move(100)
	return bsecurity.SafetyFactor(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	height, width := 103, 101
	bsecurity := BathroomSecurity{height: height, width: width}.Parse(input_data)
	var egg int
	for egg = 1; egg < 11000; egg++ {
		bsecurity.Move(1)
		if bsecurity.CheckEasterEgg() {
			break
		}
	}
	return egg, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

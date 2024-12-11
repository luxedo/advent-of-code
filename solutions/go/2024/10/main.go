package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 10
* Go Solution
*
* Day 10: Hoof It
*
* https://adventofcode.com/2024/day/10
*
 */

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type Point struct {
	y int
	x int
}

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

type Patch struct {
	value     int
	y         int
	x         int
	neighbors []*Patch
}

func (p *Patch) String() string {
	neighbors := make([]string, len(p.neighbors))
	for i, n := range p.neighbors {
		neighbors[i] = n.Str()
	}
	return fmt.Sprintf("Patch(value=%d, y=%d, x=%d, n=%v)", p.value, p.y, p.x, neighbors)
}

func (p *Patch) Str() string {
	return fmt.Sprintf("Patch(value=%d, y=%d, x=%d)", p.value, p.y, p.x)
}

func (p *Patch) Follow() [][]*Patch {
	if p.value == 9 {
		return [][]*Patch{{}}
	}
	ret := [][]*Patch{}
	for _, n := range p.neighbors {
		for _, trail := range n.Follow() {
			trail = append(trail, n)
			ret = append(ret, trail)
		}
	}
	return ret
}

type Patches [][]*Patch

func (p Patches) Parse(input_data string) Patches {
	lines := strings.Split(strings.TrimSpace(input_data), "\n")
	width := len(lines[0])
	for y, line := range lines {
		row := make([]*Patch, width)
		for x, p := range line {
			value, err := strconv.Atoi(string(p))
			var patch Patch
			if err != nil {
				patch = Patch{-1, y, x, []*Patch{}}
			} else {
				patch = Patch{value, y, x, []*Patch{}}
			}
			row[x] = &patch
		}
		p = append(p, row)
	}

	p.FillNeighbors()
	return p
}

func (p *Patches) Height() int {
	return len(*p)
}

func (p *Patches) Width() int {
	return len((*p)[0])
}

func (p *Patches) FillNeighbors() {
	for _, row := range *p {
		for _, patch := range row {
			patch.neighbors = p.FindNeighbors(patch)
		}
	}
}

func (p *Patches) FindNeighbors(f *Patch) []*Patch {
	neighbors := []*Patch{}
	points := []Point{{f.y - 1, f.x}, {f.y, f.x + 1}, {f.y + 1, f.x}, {f.y, f.x - 1}}
	for _, pt := range points {
		if !p.Inbounds(pt) {
			continue
		}
		n := p.Get(pt)
		if f.value == (*n).value-1 {
			neighbors = append(neighbors, n)
		}
	}
	return neighbors
}

func (p *Patches) Inbounds(pt Point) bool {
	return pt.y >= 0 && pt.y < p.Height() && pt.x >= 0 && pt.x < p.Width()
}

func (p *Patches) Get(pt Point) *Patch {
	return (*p)[pt.y][pt.x]
}

func (p Patches) String() string {
	s := ""
	for _, row := range p {
		for _, c := range row {
			s += fmt.Sprintf("%v, ", c)
		}
		s += "\n"
	}
	return s
}

type Trailhead struct {
	start *Patch
	paths [][]*Patch
	ends  Set[*Patch]
}

func (t Trailhead) String() string {
	paths := []string{}
	for _, path := range t.paths {
		path_vec := []string{}
		for _, patch := range path {
			path_vec = append(path_vec, patch.Str())
		}
		paths = append(paths, fmt.Sprintf("%v\n", path_vec))
	}
	return fmt.Sprintf("T(start=%s, paths=\n%v)", t.start.Str(), paths)
}

func (t *Trailhead) ScoreEnds() int {
	return len(t.ends)
}

func (t *Trailhead) ScoreTrails() int {
	return len(t.paths)
}

type Trailheads []Trailhead

func (t *Trailheads) ScoreEnds() int {
	acc := 0
	for _, th := range *t {
		acc += th.ScoreEnds()
	}
	return acc
}

func (t *Trailheads) ScoreTrails() int {
	acc := 0
	for _, th := range *t {
		acc += th.ScoreTrails()
	}
	return acc
}

func (t Trailheads) FollowTrailheads(p *Patches) *Trailheads {
	for _, row := range *p {
		for _, patch := range row {
			if patch.value != 0 {
				continue
			}
			trailhead := Trailhead{start: patch, paths: patch.Follow()}
			trailhead.ends = Set[*Patch]{}
			for _, trail := range trailhead.paths {
				trailhead.ends.Add(trail[0])
			}
			t = append(t, trailhead)
		}
	}
	return &t
}

func (t *Trailheads) String() string {
	s := ""
	for _, trail := range *t {
		s += fmt.Sprintf("%v, ", trail)
	}
	return s
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	patches := Patches{}.Parse(input_data)
	trailheads := Trailheads{}.FollowTrailheads(&patches)
	return trailheads.ScoreEnds(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	patches := Patches{}.Parse(input_data)
	trailheads := Trailheads{}.FollowTrailheads(&patches)
	return trailheads.ScoreTrails(), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 08
* Go Solution
*
* Day 8: Resonant Collinearity
*
* https://adventofcode.com/2024/day/8
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

type Vec struct{ y, x int }

func (a Vec) Add(b Vec) Vec {
	return Vec{a.y + b.y, a.x + b.x}
}

func (a Vec) Sub(b Vec) Vec {
	return Vec{a.y - b.y, a.x - b.x}
}

func (a Vec) Mul(b int) Vec {
	return Vec{a.y * b, a.x * b}
}

type Antenna struct {
	vec Vec
	t   rune
}

func (a Antenna) String() string {
	return fmt.Sprintf("A('%c': %v)", a.t, a.vec)
}

type Antinode struct {
	vec Vec
	t   rune
}

func (a Antinode) String() string {
	return fmt.Sprintf("N('%c': %v)", a.t, a.vec)
}

type AntennaArray struct {
	height    int
	width     int
	antennas  []Antenna
	antinodes []Antinode
}

func (arr AntennaArray) Parse(input_data string) AntennaArray {
	rows := strings.Split(strings.TrimSpace(input_data), "\n")
	arr.height, arr.width = len(rows), len(rows[0])
	for y, line := range rows {
		for x, t := range line {
			if t == '.' {
				continue
			}
			arr.antennas = append(arr.antennas, Antenna{Vec{y, x}, t})
		}
	}

	return arr
}

func (arr *AntennaArray) CalculateAntinodes(method func(rune) []Antinode) {
	frequencies := Set[rune]{}
	for _, ant := range arr.antennas {
		frequencies.Add(ant.t)
	}

	for freq := range frequencies {
		arr.antinodes = append(arr.antinodes, method(freq)...)
	}
}

func (arr *AntennaArray) Inbounds(vec Vec) bool {
	return vec.y >= 0 && vec.y < arr.height && vec.x >= 0 && vec.x < arr.width
}

func (arr *AntennaArray) UniqueAntinodes() Set[Vec] {
	unique := make(Set[Vec])
	for _, anti := range arr.antinodes {
		unique.Add(anti.vec)
	}
	return unique
}

func (arr *AntennaArray) FindAntinodesPt1(freq rune) []Antinode {
	fant := []Antenna{}
	for _, ant := range arr.antennas {
		if ant.t != freq {
			continue
		}
		fant = append(fant, ant)
	}

	antinodes := []Antinode{}
	for _, antA := range fant {
		for _, antB := range fant {
			if antA == antB {
				continue
			}

			dist := antA.vec.Sub(antB.vec)
			anti := Antinode{antB.vec.Sub(dist), antA.t}

			if !arr.Inbounds(anti.vec) {
				continue
			}

			antinodes = append(antinodes, anti)
		}
	}

	return antinodes
}

func (arr *AntennaArray) FindAntinodesPt2(freq rune) []Antinode {
	fant := []Antenna{}
	for _, ant := range arr.antennas {
		if ant.t != freq {
			continue
		}
		fant = append(fant, ant)
	}

	antinodes := []Antinode{}
	for _, antA := range fant {
		for _, antB := range fant {
			if antA == antB {
				continue
			}

			dist := antA.vec.Sub(antB.vec)
			for mul := 0; ; mul++ {
				anti := Antinode{antB.vec.Sub(dist.Mul(mul)), antA.t}
				if !arr.Inbounds(anti.vec) {
					break
				}
				antinodes = append(antinodes, anti)
			}
		}
	}
	return antinodes
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	arr := AntennaArray{}.Parse(input_data)
	arr.CalculateAntinodes(arr.FindAntinodesPt1)
	unique := arr.UniqueAntinodes()
	return len(unique), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	arr := AntennaArray{}.Parse(input_data)
	arr.CalculateAntinodes(arr.FindAntinodesPt2)
	unique := arr.UniqueAntinodes()
	return len(unique), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

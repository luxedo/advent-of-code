package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 11
* Go Solution
*
* Day 11: Plutonian Pebbles
*
* https://adventofcode.com/2024/day/11
*
 */

import (
	"math"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Stone    int
	Depth    int
	Megalith []Stone
	CacheKey struct {
		s Stone
		d Depth
	}
	MegaCache map[CacheKey]int
)

var megaCache = MegaCache{}

func (s *Stone) countDigits() int {
	test := 10
	num := int(*s)
	count := 1
	for num >= test {
		num /= 10
		count++
	}
	return count
}

func (s *Stone) isEvenDigitCount() bool {
	return s.countDigits()%2 == 0
}

func (s *Stone) Split() Megalith {
	digits := s.countDigits()
	divider := int(math.Pow10(digits / 2))
	s1 := Stone(int(*s) / divider)
	s2 := Stone(int(*s) % divider)
	return Megalith{s1, s2}
}

func (s Stone) Blink() Megalith {
	if s == 0 {
		// 1. If the stone is engraved with the number 0, it is replaced by a stone engraved with the
		// number 1.
		return Megalith{Stone(1)}
	} else if s.isEvenDigitCount() {
		// 2. If the stone is engraved with a number that has an even number of digits, it is replaced
		// by two stones. The left half of the digits are engraved on the new left stone, and the right
		// half of the digits are engraved on the new right stone. (The new numbers don't keep extra
		// leading zeroes: 1000 would become stones 10 and 0.)
		return s.Split()
	}
	// 3. If the stone is engraved with a number that has an even number of digits, it is replaced
	// by two stones. The left half of the digits are engraved on the new left stone, and the right
	// half of the digits are engraved on the new right stone. (The new numbers don't keep extra
	// leading zeroes: 1000 would become stones 10 and 0.)
	return Megalith{Stone(2024 * s)}
}

func (s Stone) BlinkN(N Depth) Megalith {
	if N == 0 {
		return Megalith{s}
	}
	ret := s.Blink().BlinkN(N - 1)
	return ret
}

func (s Stone) CountBlinkN(N Depth) int {
	cacheKey := CacheKey{s, N}
	if value, ok := megaCache[cacheKey]; ok {
		return value
	}
	if N == 0 {
		return 1
	}
	ret := s.Blink().CountBlinksN(N - 1)
	megaCache[cacheKey] = ret
	return ret
}

func (m Megalith) Parse(input_data string) Megalith {
	for _, numStr := range strings.Split(strings.TrimSpace(input_data), " ") {
		num, _ := strconv.Atoi(numStr)
		m = append(m, Stone(num))
	}
	return m
}

func (m Megalith) BlinkN(N Depth) Megalith {
	nm := Megalith{}
	for _, stone := range m {
		nm = append(nm, stone.BlinkN(N)...)
	}
	return nm
}

func (m Megalith) CountBlinksN(N Depth) int {
	acc := 0
	for _, stone := range m {
		acc += stone.CountBlinkN(N)
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	depth := Depth(25)
	m := Megalith{}.Parse(input_data).BlinkN(depth)
	return len(m), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	m := Megalith{}.Parse(input_data)
	depth := Depth(75)
	count := m.CountBlinksN(depth)
	return count, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

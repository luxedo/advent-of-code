package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 04
* Go Solution
*
* Day 4: Ceres Search
*
* https://adventofcode.com/2024/day/4
*
 */

import (
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func rot90(s string) string {
	rows := strings.Split(s, "\n")
	height, width := len(rows), len(rows[0])
	rotated := make([]string, width)
	for i := 0; i < width; i++ {
		for j := height - 1; j >= 0; j-- {
			rotated[i] += string(rows[j][i])
		}
	}
	return strings.Join(rotated, "\n")
}

func rot45(s string) string {
	rows := strings.Split(s, "\n")
	height, width := len(rows), len(rows[0])
	maxHeight := len(rows) + len(rows[0])
	rotated := make([]string, maxHeight)
	for t := 0; t < maxHeight; t++ {
		for i := t; i >= 0; i-- {
			j := t - i
			if i >= height || j >= width {
				continue
			}
			rotated[t] += string(rows[i][j])
		}
	}
	return strings.Join(rotated, "\n")
}

func countWord(haystack string, needle string) int {
	eldeen := reverseString(needle)

	haystackT := rot90(haystack)
	haystack45 := rot45(haystack)
	haystack45T := rot45(rot90(haystack))

	acc := 0
	for _, hay := range []string{haystack, haystackT, haystack45, haystack45T} {
		for _, nee := range []string{needle, eldeen} {
			acc += strings.Count(hay, nee)
		}
	}
	return acc
}

func countPatterns(haystack string, pattern string) int {
	patterns := []string{
		pattern,
		rot90(pattern),
		rot90(rot90(pattern)),
		rot90(rot90(rot90(pattern))),
	}

	acc := 0
	for _, p := range patterns {
		acc += countPattern(haystack, p)
	}
	return acc
}

func countPattern(haystack string, pattern string) int {
	pRows := strings.Split(pattern, "\n")
	pHeight, pWidth := len(pRows), len(pRows[0])
	hRows := strings.Split(haystack, "\n")
	hHeight, hWidth := len(hRows), len(hRows[0])

	acc := 0
	for i, row := range hRows {
		if i >= hHeight-pHeight+1 {
			break
		}
		for j, c := range row {
			if j >= hWidth-pWidth+1 {
				break
			}
			if rune(pRows[0][0]) == c && checkPattern(hRows, pRows, i, j) {
				acc++
			}
		}
	}
	return acc
}

func checkPattern(haystack []string, pattern []string, i int, j int) bool {
	for y, pRow := range pattern {
		for x, c := range pRow {
			if c == '.' {
				continue
			}
			if c != rune(haystack[y+i][x+j]) {
				return false
			}
		}
	}
	return true
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	return countWord(strings.TrimSpace(input_data), "XMAS"), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	pattern := "M.S\n.A.\nM.S"
	return countPatterns(strings.TrimSpace(input_data), pattern), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

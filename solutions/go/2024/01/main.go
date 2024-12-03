package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 01
* Go Solution
*
* Day 1: Historian Hysteria
*
* https://adventofcode.com/2024/day/1
*
 */

import (
	"sort"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

func absDiff(a int, b int) int {
	if a > b {
		return a - b
	}
	return b - a
}

func parse(input_data string) ([]int, []int) {
	idListA := []int{}
	idListB := []int{}
	for _, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		fields := strings.Fields(line)
		a, _ := strconv.Atoi(fields[0])
		b, _ := strconv.Atoi(fields[1])
		idListA = append(idListA, a)
		idListB = append(idListB, b)
	}
	sort.Ints(idListA)
	sort.Ints(idListB)
	return idListA, idListB
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	idListA, idListB := parse(input_data)

	total := 0
	for i, idA := range idListA {
		total += absDiff(idA, idListB[i])
	}

	return total, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	idListA, idListB := parse(input_data)

	counts := make(map[int]int)
	for _, id := range idListB {
		counts[id]++
	}

	total := 0
	for _, id := range idListA {
		total += id * counts[id]
	}

	return total, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

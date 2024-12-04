package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 03
* Go Solution
*
* Day 3: Mull It Over
*
* https://adventofcode.com/2024/day/3
*
 */

import (
	"regexp"
	"strconv"

	"github.com/luxedo/esb_fireplace-go"
)

func multiply(input_data string) int {
	acc := 0
	re := regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	for _, match := range re.FindAllStringSubmatch(input_data, -1) {
		a, _ := strconv.Atoi(match[1])
		b, _ := strconv.Atoi(match[2])
		acc += a * b
	}
	return acc
}

func filter_disabled(input_data string) string {
	re := regexp.MustCompile(`(?s)don't\(\).*?do\(\)`)
	return re.ReplaceAllString(input_data, "-")
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	return multiply(input_data), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	return multiply(filter_disabled(input_data)), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

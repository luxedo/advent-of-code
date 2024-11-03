package main

/*
* ElfScript Brigade
*
* Advent Of Code 2017 Day 01
* Rust Solution
*
* Day 1: Inverse Captcha
*
* https://adventofcode.com/2017/day/1
*
 */

import (
	"github.com/luxedo/esb_fireplace-go"
	"strconv"
	"strings"
)

func solve(captcha string, steps int) int {
	captcha2 := captcha[steps:] + captcha[0:steps]
	acc := 0
	for idx, curr := range captcha {
		next := captcha2[idx]
		if rune(next) == curr {
			num, _ := strconv.Atoi(string(next))
			acc += num
		}
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	captcha := strings.Trim(input_data, "\n")
	return solve(captcha, 1), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	captcha := strings.Trim(input_data, "\n")
	return solve(captcha, len(captcha)/2), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

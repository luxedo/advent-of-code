package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 25
* Go Solution
*
* Day 25: Code Chronicle
*
* https://adventofcode.com/2024/day/25
*
 */

import (
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Cell       uint8
	Lock       [5]int
	Key        [5]int
	Schematics struct {
		Locks []Lock
		Keys  []Key
	}
)

const (
	CELL_HASH  Cell = '#'
	CELL_DOT        = '.'
	EMPTY      int  = -1
	MAX_HEIGHT int  = 5
)

func (l Lock) Parse(lockStr string) Lock {
	for i := range l {
		l[i] = EMPTY
	}
	for i, line := range strings.Split(lockStr, "\n") {
		for j := range line {
			if l[j] != EMPTY {
				continue
			}
			if line[j] != byte(CELL_HASH) {
				l[j] = i - 1
			}
		}
	}
	return l
}

func (l Lock) Fit(k Key) bool {
	for i := range l {
		if l[i]+k[i] > MAX_HEIGHT {
			return false
		}
	}
	return true
}

func (k Key) Parse(keyStr string) Key {
	for i := range k {
		k[i] = EMPTY
	}
	for i, line := range strings.Split(keyStr, "\n") {
		for j := range line {
			if k[j] != EMPTY {
				continue
			}
			if line[j] != byte(CELL_DOT) {
				// k[j] = i - 1
				k[j] = 6 - i
			}
		}
	}
	return k
}

func (s Schematics) Parse(input_data string) Schematics {
	lockKeys := strings.Split(strings.TrimSpace(input_data), "\n\n")
	for _, lockKey := range lockKeys {
		switch Cell(lockKey[0]) {
		case CELL_HASH:
			s.Locks = append(s.Locks, Lock{}.Parse(lockKey))
		case CELL_DOT:
			s.Keys = append(s.Keys, Key{}.Parse(lockKey))
		default:
			panic("Oh no! can't parse this schematic")
		}
	}
	return s
}

func (s Schematics) LockKeyFit() map[int][]int {
	ret := map[int][]int{}
	for i, l := range s.Locks {
		for j, k := range s.Keys {
			if l.Fit(k) {
				ret[i] = append(ret[i], j)
			}
		}
	}
	return ret
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	schematics := Schematics{}.Parse(input_data)
	fits := schematics.LockKeyFit()

	acc := 0
	for _, v := range fits {
		acc += len(v)
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	return "December", nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

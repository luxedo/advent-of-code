package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 21
* Go Solution
*
* Day 21: Keypad Conundrum
*
* https://adventofcode.com/2024/day/21
*
 */

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

const (
	A_BUTTON     Button = 'A'
	EMPTY_BUTTON Button = ' '
	N_ZERO       Button = '0'
	N_ONE        Button = '1'
	N_TWO        Button = '2'
	N_THREE      Button = '3'
	N_FOUR       Button = '4'
	N_FIVE       Button = '5'
	N_SIX        Button = '6'
	N_SEVEN      Button = '7'
	N_EIGHT      Button = '8'
	N_NINE       Button = '9'
	D_UP         Button = '^'
	D_RIGHT      Button = '>'
	D_DOWN       Button = 'v'
	D_LEFT       Button = '<'
)

var NUM_KEYPAD [][]Button = [][]Button{
	{N_SEVEN, N_EIGHT, N_NINE},
	{N_FOUR, N_FIVE, N_SIX},
	{N_ONE, N_TWO, N_THREE},
	{EMPTY_BUTTON, N_ZERO, A_BUTTON},
}

var DIR_KEYPAD [][]Button = [][]Button{
	{EMPTY_BUTTON, D_UP, A_BUTTON},
	{D_LEFT, D_DOWN, D_RIGHT},
}

type (
	Button byte
	Move   struct {
		src, dst Button
	}
	Code         []Button
	CodeSequence []Code
	Keypad       map[Move]Code
	Depth        int
	CodeCacheKey struct {
		code  string
		depth Depth
	}
	MoveCacheKey struct {
		move  Move
		depth Depth
	}
	CodeCache map[CodeCacheKey]int
	MoveCache map[MoveCacheKey]int
)

var numKeypad = Keypad{
	Move{'0', '0'}: Code{'A'},
	Move{'0', '1'}: Code{'^', '<', 'A'},
	Move{'0', '2'}: Code{'^', 'A'},
	Move{'0', '3'}: Code{'^', '>', 'A'},
	// Move{'0', '3'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'0', '4'}: Code{'^', '^', '<', 'A'},
	Move{'0', '5'}: Code{'^', '^', 'A'},
	// Move{'0', '6'}: Code{'^', '^', '>', 'A'}, {'>', '^', '^', 'A'},
	Move{'0', '6'}: Code{'^', '^', '>', 'A'},
	Move{'0', '7'}: Code{'^', '^', '^', '<', 'A'},
	Move{'0', '8'}: Code{'^', '^', '^', 'A'},
	// Move{'0', '9'}: Code{'^', '^', '^', '>', 'A'}, {'>', '^', '^', '^', 'A'},
	Move{'0', '9'}: Code{'^', '^', '^', '>', 'A'},
	Move{'0', 'A'}: Code{'>', 'A'},
	Move{'1', '0'}: Code{'>', 'v', 'A'},
	Move{'1', '1'}: Code{'A'},
	Move{'1', '2'}: Code{'>', 'A'},
	Move{'1', '3'}: Code{'>', '>', 'A'},
	Move{'1', '4'}: Code{'^', 'A'},
	// Move{'1', '5'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'1', '5'}: Code{'^', '>', 'A'},
	// Move{'1', '6'}: Code{'^', '>', '>', 'A'}, {'>', '>', '^', 'A'},
	Move{'1', '6'}: Code{'^', '>', '>', 'A'},
	Move{'1', '7'}: Code{'^', '^', 'A'},
	// Move{'1', '8'}: Code{'^', '^', '>', 'A'}, {'>', '^', '^', 'A'},
	Move{'1', '8'}: Code{'^', '^', '>', 'A'},
	// Move{'1', '9'}: Code{'^', '^', '>', '>', 'A'}, {'>', '>', '^', '^', 'A'},
	Move{'1', '9'}: Code{'^', '^', '>', '>', 'A'},
	Move{'1', 'A'}: Code{'>', '>', 'v', 'A'},
	Move{'2', '0'}: Code{'v', 'A'},
	Move{'2', '1'}: Code{'<', 'A'},
	Move{'2', '2'}: Code{'A'},
	Move{'2', '3'}: Code{'>', 'A'},
	// Move{'2', '4'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'2', '4'}: Code{'<', '^', 'A'},
	Move{'2', '5'}: Code{'^', 'A'},
	// Move{'2', '6'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'2', '6'}: Code{'^', '>', 'A'},
	// Move{'2', '7'}: Code{'^', '^', '<', 'A'}, {'<', '^', '^', 'A'},
	Move{'2', '7'}: Code{'<', '^', '^', 'A'},
	Move{'2', '8'}: Code{'^', '^', 'A'},
	// Move{'2', '9'}: Code{'^', '^', '>', 'A'}, {'>', '^', '^', 'A'},
	Move{'2', '9'}: Code{'^', '^', '>', 'A'},
	// Move{'2', 'A'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'2', 'A'}: Code{'v', '>', 'A'},
	// Move{'3', '0'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'3', '0'}: Code{'<', 'v', 'A'},
	Move{'3', '1'}: Code{'<', '<', 'A'},
	Move{'3', '2'}: Code{'<', 'A'},
	Move{'3', '3'}: Code{'A'},
	// Move{'3', '4'}: Code{'^', '<', '<', 'A'}, {'<', '<', '^', 'A'},
	Move{'3', '4'}: Code{'<', '<', '^', 'A'},
	// Move{'3', '5'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'3', '5'}: Code{'<', '^', 'A'},
	Move{'3', '6'}: Code{'^', 'A'},
	// Move{'3', '7'}: Code{'^', '^', '<', '<', 'A'}, {'<', '<', '^', '^', 'A'},
	Move{'3', '7'}: Code{'<', '<', '^', '^', 'A'},
	// Move{'3', '8'}: Code{'^', '^', '<', 'A'}, {'<', '^', '^', 'A'},
	Move{'3', '8'}: Code{'<', '^', '^', 'A'},
	Move{'3', '9'}: Code{'^', '^', 'A'},
	Move{'3', 'A'}: Code{'v', 'A'},
	Move{'4', '0'}: Code{'>', 'v', 'v', 'A'},
	Move{'4', '1'}: Code{'v', 'A'},
	// Move{'4', '2'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'4', '2'}: Code{'v', '>', 'A'},
	// Move{'4', '3'}: Code{'>', '>', 'v', 'A'}, {'v', '>', '>', 'A'},
	Move{'4', '3'}: Code{'v', '>', '>', 'A'},
	Move{'4', '4'}: Code{'A'},
	Move{'4', '5'}: Code{'>', 'A'},
	Move{'4', '6'}: Code{'>', '>', 'A'},
	Move{'4', '7'}: Code{'^', 'A'},
	// Move{'4', '8'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'4', '8'}: Code{'^', '>', 'A'},
	// Move{'4', '9'}: Code{'^', '>', '>', 'A'}, {'>', '>', '^', 'A'},
	Move{'4', '9'}: Code{'^', '>', '>', 'A'},
	Move{'4', 'A'}: Code{'>', '>', 'v', 'v', 'A'},
	Move{'5', '0'}: Code{'v', 'v', 'A'},
	// Move{'5', '1'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'5', '1'}: Code{'<', 'v', 'A'},
	Move{'5', '2'}: Code{'v', 'A'},
	// Move{'5', '3'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'5', '3'}: Code{'v', '>', 'A'},
	Move{'5', '4'}: Code{'<', 'A'},
	Move{'5', '5'}: Code{'A'},
	Move{'5', '6'}: Code{'>', 'A'},
	// Move{'5', '7'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'5', '7'}: Code{'<', '^', 'A'},
	Move{'5', '8'}: Code{'^', 'A'},
	// Move{'5', '9'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'5', '9'}: Code{'^', '>', 'A'},
	// Move{'5', 'A'}: Code{'>', 'v', 'v', 'A'}, {'v', 'v', '>', 'A'},
	Move{'5', 'A'}: Code{'v', 'v', '>', 'A'},
	// Move{'6', '0'}: Code{'v', 'v', '<', 'A'}, {'<', 'v', 'v', 'A'},
	Move{'6', '0'}: Code{'<', 'v', 'v', 'A'},
	// Move{'6', '1'}: Code{'v', '<', '<', 'A'}, {'<', '<', 'v', 'A'},
	Move{'6', '1'}: Code{'<', '<', 'v', 'A'},
	// Move{'6', '2'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'6', '2'}: Code{'<', 'v', 'A'},
	Move{'6', '3'}: Code{'v', 'A'},
	Move{'6', '4'}: Code{'<', '<', 'A'},
	Move{'6', '5'}: Code{'<', 'A'},
	Move{'6', '6'}: Code{'A'},
	// Move{'6', '7'}: Code{'^', '<', '<', 'A'}, {'<', '<', '^', 'A'},
	Move{'6', '7'}: Code{'<', '<', '^', 'A'},
	// Move{'6', '8'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'6', '8'}: Code{'<', '^', 'A'},
	Move{'6', '9'}: Code{'^', 'A'},
	Move{'6', 'A'}: Code{'v', 'v', 'A'},
	Move{'7', '0'}: Code{'>', 'v', 'v', 'v', 'A'},
	Move{'7', '1'}: Code{'v', 'v', 'A'},
	// Move{'7', '2'}: Code{'>', 'v', 'v', 'A'}, {'v', 'v', '>', 'A'},
	Move{'7', '2'}: Code{'v', 'v', '>', 'A'},
	// Move{'7', '3'}: Code{'>', '>', 'v', 'v', 'A'}, {'v', 'v', '>', '>', 'A'},
	Move{'7', '3'}: Code{'v', 'v', '>', '>', 'A'},
	Move{'7', '4'}: Code{'v', 'A'},
	// Move{'7', '5'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'7', '5'}: Code{'v', '>', 'A'},
	// Move{'7', '6'}: Code{'>', '>', 'v', 'A'}, {'v', '>', '>', 'A'},
	Move{'7', '6'}: Code{'v', '>', '>', 'A'},
	Move{'7', '7'}: Code{'A'},
	Move{'7', '8'}: Code{'>', 'A'},
	Move{'7', '9'}: Code{'>', '>', 'A'},
	Move{'7', 'A'}: Code{'>', '>', 'v', 'v', 'v', 'A'},
	Move{'8', '0'}: Code{'v', 'v', 'v', 'A'},
	// Move{'8', '1'}: Code{'v', 'v', '<', 'A'}, {'<', 'v', 'v', 'A'},
	Move{'8', '1'}: Code{'<', 'v', 'v', 'A'},
	Move{'8', '2'}: Code{'v', 'v', 'A'},
	// Move{'8', '3'}: Code{'>', 'v', 'v', 'A'}, {'v', 'v', '>', 'A'},
	Move{'8', '3'}: Code{'v', 'v', '>', 'A'},
	// Move{'8', '4'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'8', '4'}: Code{'<', 'v', 'A'},
	Move{'8', '5'}: Code{'v', 'A'},
	// Move{'8', '6'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'8', '6'}: Code{'v', '>', 'A'},
	Move{'8', '7'}: Code{'<', 'A'},
	Move{'8', '8'}: Code{'A'},
	Move{'8', '9'}: Code{'>', 'A'},
	// Move{'8', 'A'}: Code{'>', 'v', 'v', 'v', 'A'}, {'v', 'v', 'v', '>', 'A'},
	Move{'8', 'A'}: Code{'v', 'v', 'v', '>', 'A'},
	// Move{'9', '0'}: Code{'v', 'v', 'v', '<', 'A'}, {'<', 'v', 'v', 'v', 'A'},
	Move{'9', '0'}: Code{'<', 'v', 'v', 'v', 'A'},
	// Move{'9', '1'}: Code{'v', 'v', '<', '<', 'A'}, {'<', '<', 'v', 'v', 'A'},
	Move{'9', '1'}: Code{'<', '<', 'v', 'v', 'A'},
	// Move{'9', '2'}: Code{'v', 'v', '<', 'A'}, {'<', 'v', 'v', 'A'},
	Move{'9', '2'}: Code{'<', 'v', 'v', 'A'},
	Move{'9', '3'}: Code{'v', 'v', 'A'},
	// Move{'9', '4'}: Code{'v', '<', '<', 'A'}, {'<', '<', 'v', 'A'},
	Move{'9', '4'}: Code{'<', '<', 'v', 'A'},
	// Move{'9', '5'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'9', '5'}: Code{'<', 'v', 'A'},
	Move{'9', '6'}: Code{'v', 'A'},
	Move{'9', '7'}: Code{'<', '<', 'A'},
	Move{'9', '8'}: Code{'<', 'A'},
	Move{'9', '9'}: Code{'A'},
	Move{'9', 'A'}: Code{'v', 'v', 'v', 'A'},
	Move{'A', '0'}: Code{'<', 'A'},
	Move{'A', '1'}: Code{'^', '<', '<', 'A'},
	// Move{'A', '2'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'A', '2'}: Code{'<', '^', 'A'},
	Move{'A', '3'}: Code{'^', 'A'},
	Move{'A', '4'}: Code{'^', '^', '<', '<', 'A'},
	// Move{'A', '5'}: Code{'^', '^', '<', 'A'}, {'<', '^', '^', 'A'},
	Move{'A', '5'}: Code{'<', '^', '^', 'A'},
	Move{'A', '6'}: Code{'^', '^', 'A'},
	Move{'A', '7'}: Code{'^', '^', '^', '<', '<', 'A'},
	// Move{'A', '8'}: Code{'^', '^', '^', '<', 'A'}, {'<', '^', '^', '^', 'A'},
	Move{'A', '8'}: Code{'<', '^', '^', '^', 'A'},
	Move{'A', '9'}: Code{'^', '^', '^', 'A'},
	Move{'A', 'A'}: Code{'A'},
}

var dirKeypad = Keypad{
	Move{'<', 'v'}: Code{'>', 'A'},
	Move{'>', '>'}: Code{'A'},
	Move{'^', '^'}: Code{'A'},
	Move{'A', '<'}: Code{'v', '<', '<', 'A'},
	Move{'<', 'A'}: Code{'>', '>', '^', 'A'},
	// Move{'>', '^'}: Code{'^', '<', 'A'}, {'<', '^', 'A'},
	Move{'>', '^'}: Code{'<', '^', 'A'},
	Move{'>', 'A'}: Code{'^', 'A'},
	Move{'^', 'v'}: Code{'v', 'A'},
	Move{'A', 'A'}: Code{'A'},
	// Move{'A', 'v'}: Code{'v', '<', 'A'}, {'<', 'v', 'A'},
	Move{'A', 'v'}: Code{'<', 'v', 'A'},
	Move{'<', '^'}: Code{'>', '^', 'A'},
	Move{'<', '<'}: Code{'A'},
	Move{'<', '>'}: Code{'>', '>', 'A'},
	Move{'v', '<'}: Code{'<', 'A'},
	Move{'v', 'v'}: Code{'A'},
	Move{'A', '^'}: Code{'<', 'A'},
	Move{'v', '>'}: Code{'>', 'A'},
	Move{'^', '<'}: Code{'v', '<', 'A'},
	// Move{'^', '>'}: Code{'>', 'v', 'A'}, {'v', '>', 'A'},
	Move{'^', '>'}: Code{'v', '>', 'A'},
	Move{'A', '>'}: Code{'v', 'A'},
	Move{'v', '^'}: Code{'^', 'A'},
	// Move{'v', 'A'}: Code{'^', '>', 'A'}, {'>', '^', 'A'},
	Move{'v', 'A'}: Code{'^', '>', 'A'},
	Move{'>', '<'}: Code{'<', '<', 'A'},
	Move{'>', 'v'}: Code{'<', 'A'},
	Move{'^', 'A'}: Code{'>', 'A'},
}

func CodeToInt(bytes Code) int {
	str := string(bytes)
	num, _ := strconv.Atoi(str)
	return num
}

func (d CodeSequence) Parse(input_data string) CodeSequence {
	rows := strings.Split(strings.TrimSpace(input_data), "\n")
	d = make(CodeSequence, len(rows))
	for i, line := range rows {
		for _, c := range line {
			d[i] = append(d[i], Button(c))
		}
	}
	return d
}

func (b Button) String() string {
	return string(b)
}

func (d Code) String() string {
	v := make([]string, len(d))
	for i, c := range d {
		v[i] = string(c)
	}
	return fmt.Sprintf("%v", v)
}

func (m Move) String() string {
	return fmt.Sprintf("Move(%c %c)", m.src, m.dst)
}

func (k Keypad) Combinations(code Code) Code {
	currButton := A_BUTTON
	movements := Code{}
	for _, nextButton := range code {
		s := Move{currButton, nextButton}
		movements = append(movements, k[s]...)
		currButton = nextButton
	}
	return movements
}

func (k Keypad) LockCombinations(lock CodeSequence) CodeSequence {
	combinations := make(CodeSequence, len(lock))
	for i, code := range lock {
		combinations[i] = k.Combinations(code)
	}
	return combinations
}

func (k Keypad) CombinationsFast(
	code Code,
	depth Depth,
	codeCache *CodeCache,
	moveCache *MoveCache,
) int {
	key := CodeCacheKey{code: string(code), depth: depth}
	if value, ok := (*codeCache)[key]; ok {
		return value
	}
	// fmt.Println("  Comb:", code, depth)
	if depth == 0 {
		return len(code)
	}

	length := 0
	currButton := A_BUTTON
	for _, nextButton := range code {
		move := Move{src: currButton, dst: nextButton}
		length += k.GetCombinationsFast(move, depth, codeCache, moveCache)
		currButton = nextButton
	}

	(*codeCache)[key] = length
	return length
}

func (k Keypad) GetCombinationsFast(
	move Move,
	depth Depth,
	codeCache *CodeCache,
	moveCache *MoveCache,
) int {
	key := MoveCacheKey{move: move, depth: depth}
	if value, ok := (*moveCache)[key]; ok {
		return value
	}
	if move.src == move.dst {
		return 1
	}
	// fmt.Println("    GetComb:")
	// fmt.Println("   ", currButton, nextButton, s, depth)
	comb := k.CombinationsFast(k[move], depth-1, codeCache, moveCache)
	(*moveCache)[key] = comb
	return comb
}

func (k Keypad) LockCombinationsFast(lock CodeSequence, depth Depth) []int {
	moveCache := make(MoveCache)
	codeCache := make(CodeCache)
	lengths := make([]int, len(lock))
	for i, code := range lock {
		// fmt.Println()
		// fmt.Println("Sequence", i)
		lengths[i] = k.CombinationsFast(code, depth, &codeCache, &moveCache)
		// fmt.Println(combinations[i])
		// break
	}
	return lengths
}

func (c CodeSequence) Complexity(dirLock CodeSequence) int {
	lengths := []int{}
	for _, comb := range dirLock {
		lengths = append(lengths, len(comb))
	}
	return c.ComplexityFromLengths(lengths)
}

func (c CodeSequence) ComplexityFromLengths(lengths []int) int {
	acc := 0
	for i, code := range c {
		length := lengths[i]
		codeValue := CodeToInt(code[:len(code)-1])
		acc += codeValue * length
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	doorLock := CodeSequence{}.Parse(input_data)

	dirLockRad := numKeypad.LockCombinations(doorLock)
	dirLockFreeze := dirKeypad.LockCombinations(dirLockRad)
	dirLockHist := dirKeypad.LockCombinations(dirLockFreeze)

	return doorLock.Complexity(dirLockHist), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	doorLock := CodeSequence{}.Parse(input_data)

	robots := Depth(25)
	dirLock := numKeypad.LockCombinations(doorLock)
	dirLockLenghts := dirKeypad.LockCombinationsFast(dirLock, robots)
	// 324560699541678

	return doorLock.ComplexityFromLengths(dirLockLenghts), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

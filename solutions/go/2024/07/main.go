package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 07
* Go Solution
*
* Day 7: Bridge Repair
*
* https://adventofcode.com/2024/day/7
*
 */

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type Deque[T any] []T

func (d *Deque[T]) Clone() Deque[T] {
	clone := make(Deque[T], len(*d))
	copy(clone, *d)
	return clone
}

func (d *Deque[T]) PushBack(value T) {
	*d = append(*d, value)
}

func (d *Deque[T]) PushFront(value T) {
	*d = append([]T{value}, *d...) // Creates a new slice with 'value' at the front
}

func (d *Deque[T]) PopFront() (T, error) {
	size := d.Size()
	if size == 0 {
		var zero T
		return zero, errors.New("Deque is empty: cannot pop")
	}
	front := (*d)[0]
	*d = (*d)[1:]
	return front, nil
}

func (d *Deque[T]) Peek(idx int) (T, error) {
	size := d.Size()
	if size == 0 {
		var zero T
		return zero, errors.New("Deque is empty: cannot peek")
	} else if idx >= size {
		var zero T
		return zero, errors.New("Index not available")
	}
	return (*d)[idx], nil
}

func (d *Deque[T]) Size() int {
	return len(*d)
}

type Operator func(a, b int) int

var OpAdd Operator = func(a, b int) int {
	return a + b
}

var OpMul Operator = func(a, b int) int {
	return a * b
}

var OpConcat Operator = func(a, b int) int {
	multiplier := 1
	for temp := b; temp > 0; temp /= 10 {
		multiplier *= 10
	}
	return a*multiplier + b
}

type Equation struct {
	answer    int
	valid     bool
	equation  Deque[int]
	operators Deque[Operator]
}

func (eq Equation) String() string {
	operators := []string{}
	opAddAddr, opMulAddr, opConcatAddr := fmt.Sprintf(
		"%p",
		OpAdd,
	), fmt.Sprintf(
		"%p",
		OpMul,
	), fmt.Sprintf(
		"%p",
		OpConcat,
	) // Daaaaamn!
	for _, op := range eq.operators {
		switch fmt.Sprintf("%p", op) {
		case opAddAddr:
			operators = append(operators, "+")
		case opMulAddr:
			operators = append(operators, "*")
		case opConcatAddr:
			operators = append(operators, "||")
		default:
			operators = append(operators, "Unknown")
		}
	}
	return fmt.Sprintf("{%d %t %v %v}", eq.answer, eq.valid, eq.equation, operators)
}

func (c *Equation) Clone() *Equation {
	return &Equation{
		answer:   c.answer,
		equation: c.equation.Clone(),
	}
}

func (Equation) Parse(line string) Equation {
	parts := strings.SplitN(line, ":", 2)
	answer, _ := strconv.Atoi(parts[0])
	equation := Deque[int]{}
	for _, num_str := range strings.Split(strings.TrimSpace(parts[1]), " ") {
		num, _ := strconv.Atoi(num_str)
		equation.PushBack(num)
	}
	return Equation{answer: answer, equation: equation, operators: Deque[Operator]{}, valid: false}
}

func (e *Equation) Solve() error {
	if e.operators.Size() == 0 {
		return nil
	}

	num1, err1 := e.equation.PopFront()
	num2, err2 := e.equation.PopFront()
	if err1 != nil || err2 != nil {
		return errors.New("Malformed equation")
	}
	op, _ := e.operators.PopFront()
	e.equation.PushFront(op(num1, num2))
	return nil
}

func (e *Equation) Calibrate(operators []*Operator) Equation {
	if e.equation.Size() == 1 {
		v, _ := e.equation.Peek(0)
		if v == e.answer {
			e.valid = true
		} else {
			e.valid = false
		}
		return *e
	}

	for _, op := range operators {
		ne := e.Clone()
		ne.operators.PushBack(*op)
		ne.Solve()
		ne.Calibrate(operators)
		if ne.valid {
			(*e).operators = ne.operators.Clone()
			(*e).operators.PushBack(*op)
			e.valid = true
			return *e
		}
	}
	return *e
}

type Calibrator []Equation

func (c Calibrator) Parse(input_data string) Calibrator {
	for _, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		eq := Equation{}
		c = append(c, eq.Parse(line))
	}
	return c
}

func (c Calibrator) Calibrate(operators []*Operator) Calibrator {
	for i, eq := range c {
		c[i] = eq.Calibrate(operators)
	}
	return c
}

func (c Calibrator) Run(input_data string, operators []*Operator) int {
	c = c.Parse(input_data).Calibrate(operators)
	acc := 0
	for _, eq := range c {
		if eq.valid {
			acc += eq.answer
		}
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	operators := []*Operator{&OpAdd, &OpMul}
	result := Calibrator{}.Run(input_data, operators)
	return result, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	operators := []*Operator{&OpAdd, &OpMul, &OpConcat}
	result := Calibrator{}.Run(input_data, operators)
	return result, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

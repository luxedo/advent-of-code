package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 17
* Go Solution
*
* Day 17: Chronospatial Computer
*
* https://adventofcode.com/2024/day/17
*
 */

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Instruction     uint8
	Operand         uint8
	InstructionFunc func(c *Computer, operandValue uint8)
	OperandFunc     func(c *Computer) int
	Program         []uint8
	Computer        struct {
		registerA int
		registerB int
		registerC int
		ip        int
		outputs   []uint8
	}
)

const (
	BITS = 3
)

const (
	ADV Instruction = 0
	BXL Instruction = 1
	BST Instruction = 2
	JNZ Instruction = 3
	BXC Instruction = 4
	OUT Instruction = 5
	BDV Instruction = 6
	CDV Instruction = 7
)

var INSTRUCTIONS = map[Instruction]InstructionFunc{
	ADV: func(c *Computer, operand uint8) {
		c.registerA >>= c.Combo(operand)
	},
	BXL: func(c *Computer, operand uint8) {
		c.registerB ^= int(operand)
	},
	BST: func(c *Computer, operand uint8) {
		c.registerB = c.Combo(operand) % 8
	},
	JNZ: func(c *Computer, operand uint8) {
		if c.registerA != 0 {
			c.ip = int(operand)
		}
	},
	BXC: func(c *Computer, operand uint8) {
		c.registerB ^= c.registerC
	},
	OUT: func(c *Computer, operand uint8) {
		c.outputs = append(c.outputs, uint8(c.Combo(operand)%8))
	},
	BDV: func(c *Computer, operand uint8) {
		c.registerB = c.registerA >> c.Combo(operand)
	},
	CDV: func(c *Computer, operand uint8) {
		c.registerC = c.registerA >> c.Combo(operand)
	},
}

func (p Program) Parse(instructions string) *Program {
	blocks := strings.Split(instructions, ": ")
	for _, inst := range strings.Split(strings.TrimSpace(blocks[1]), ",") {
		value, _ := strconv.Atoi(inst)
		p = append(p, uint8(value))
	}
	return &p
}

func (c *Computer) Combo(operand uint8) int {
	switch operand {
	case 0, 1, 2, 3:
		return int(operand)
	case 4:
		return c.registerA
	case 5:
		return c.registerB
	case 6:
		return c.registerC
	default:
		panic("Oh no! Which operand?")
	}
}

func (c Computer) Parse(input_data string) (*Computer, *Program) {
	blocks := strings.Split(strings.TrimSpace(input_data), "\n\n")
	for _, line := range strings.Split(blocks[0], "\n") {
		reg := strings.Split(line, ": ")
		value, _ := strconv.Atoi(reg[1])
		switch reg[0] {
		case "Register A":
			c.registerA = value
		case "Register B":
			c.registerB = value
		case "Register C":
			c.registerC = value
		}
	}
	p := Program{}.Parse(blocks[1])
	return &c, p
}

func (c *Computer) Clone() *Computer {
	nc := Computer{
		registerA: c.registerA, registerB: c.registerB, registerC: c.registerC, ip: 0, outputs: []uint8{},
	}
	return &nc
}

func (c *Computer) Run(program *Program) []int {
	for c.ip = 0; c.ip < len(*program); {
		opcode := (*program)[c.ip]
		operand := (*program)[c.ip+1]
		c.ip += 2
		INSTRUCTIONS[Instruction(opcode)](c, operand)
	}
	return []int{}
}

func (c *Computer) Output() string {
	out := make([]string, len(c.outputs))
	for i, o := range c.outputs {
		out[i] = fmt.Sprint(o)
	}
	return strings.Join(out, ",")
}

func (p *Program) Equals(a *[]uint8) bool {
	if len(*a) != len(*p) {
		return false
	}
	for i := range *a {
		if (*a)[i] != (*p)[i] {
			return false
		}
	}
	return true
}

func (p *Program) RevPartialEqual(values *[]uint8, index int) bool {
	if len(*p) < index || len(*values) < index {
		return false
	}
	for i := range len(*p) - index {
		i = len(*p) - i - 1
		if (*values)[i] != (*p)[i] {
			return false
		}
	}
	return true
}

func (c *Computer) SearchProgram(program *Program, acc int, index int) (int, error) {
	if index < 0 {
		return 0, errors.New("negative index")
	}
	for j := range 1 << BITS {
		offset := BITS * index
		registerA := acc | (j << offset)
		clone := c.Clone()
		clone.registerA = registerA
		clone.Run(program)
		if program.Equals(&clone.outputs) {
			return registerA, nil
		}
		if program.RevPartialEqual(&clone.outputs, index) {
			if found, err := c.SearchProgram(program, acc|registerA, index-1); err == nil {
				return found, nil
			}
		}
	}
	return 0, errors.New("could not find solution :(")
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	computer, program := Computer{}.Parse(input_data)
	computer.Run(program)
	return computer.Output(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	computer, program := Computer{}.Parse(input_data)
	acc := 0
	index := len(*program) - 1
	return computer.SearchProgram(program, acc, index)
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

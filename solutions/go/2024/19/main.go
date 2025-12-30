package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 19
* Go Solution
*
* Day 19: Linen Layout
*
* https://adventofcode.com/2024/day/19
*
 */

import (
	"fmt"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Color             byte
	Towel             struct {
		stripes [MAX_STRIPES]Color
	}
	Towels      Set[Towel]
	TowelDesign []Towel
	Design      struct {
		colors [MAX_COLORS]Color
		length int
	}
	Designs    []*Design
	TowelCache map[Design]int
)

const (
	MAX_STRIPES = 8
	MAX_COLORS  = 64
)

const (
	NO_COLOR Color = 0
	WHITE    Color = 'w'
	BLUE     Color = 'u'
	BLACK    Color = 'b'
	RED      Color = 'r'
	GREEN    Color = 'g'
)

func ParseColor(color rune) Color {
	switch color {
	case rune(WHITE):
		return WHITE
	case rune(BLUE):
		return BLUE
	case rune(BLACK):
		return BLACK
	case rune(RED):
		return RED
	case rune(GREEN):
		return GREEN
	default:
		panic("Oh no! Can't parse color!")
	}
}

func (s Towels) Add(g Towel) {
	s[g] = struct{}{}
}

func (s Towels) Contains(g Towel) bool {
	_, exists := s[g]
	return exists
}

func (s Towels) Remove(g Towel) {
	delete(s, g)
}

func (s Color) String() string {
	return string(s)
}

func (t Towel) Parse(towelStr string) *Towel {
	for i, c := range towelStr {
		t.stripes[i] = ParseColor(c)
	}
	return &t
}

func (t Towel) String() string {
	s := []string{}
	for _, color := range t.stripes {
		if color == 0 {
			break
		}
		s = append(s, fmt.Sprint(color))
	}
	return fmt.Sprintf("%s", s)
}

func (t Towel) New(colors []Color) *Towel {
	copy(t.stripes[:], colors)
	return &t
}

func (t Towels) String() string {
	s := []string{}
	for towel := range t {
		s = append(s, fmt.Sprint(towel))
	}
	return fmt.Sprintf("Towels%s", s)
}

func (t Towels) Parse(towelStr string) *Towels {
	for _, tStr := range strings.Split(towelStr, ", ") {
		t.Add(*Towel{}.Parse(tStr))
	}
	return &t
}

func (d Design) String() string {
	s := []string{}
	for _, color := range (d).colors {
		if color == 0 {
			break
		}
		s = append(s, fmt.Sprint(color))
	}
	return fmt.Sprintf("Design%v", s)
}

func (d Design) Parse(designStr string) *Design {
	for i, c := range designStr {
		d.colors[i] = ParseColor(c)
		d.length++
	}
	return &d
}

func (d *Design) Empty() bool {
	return d.length == 0
}

func (d TowelDesign) String() string {
	s := make([]string, len(d))
	for i, towel := range d {
		s[i] = fmt.Sprint(towel)
	}
	return fmt.Sprintf("TowelDesign%v", s)
}

func (c TowelCache) String() string {
	s := []string{}
	for key, value := range c {
		s = append(s, fmt.Sprintf("%v : %v", key, value))
	}
	return fmt.Sprintf("Cache(%s)", strings.Join(s, ", "))
}

func (d *Design) Count(towels Towels, cache TowelCache) int {
	if ret, ok := cache[*d]; ok {
		return ret
	}

	if d.Empty() {
		return 1
	}

	ret := 0
	for i := 1; i < d.length+1 && i < MAX_STRIPES+1; i++ {
		guess := *Towel{}.New((*d).colors[:i])
		if !towels.Contains(guess) {
			continue
		}
		nd := Design{}
		copy(nd.colors[:], d.colors[i:])
		nd.length = d.length - i
		ret += nd.Count(towels, cache)
	}
	cache[*d] = ret
	return ret
}

func (d Designs) Parse(designStr string) *Designs {
	for _, line := range strings.Split(designStr, "\n") {
		d = append(d, Design{}.Parse(line))
	}
	return &d
}

func (d *Designs) Count(towels Towels) []int {
	ret := make([]int, len(*d))
	cache := TowelCache{}
	for i, design := range *d {
		ret[i] = design.Count(towels, cache)
	}
	return ret
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	data := strings.Split(strings.TrimSpace(input_data), "\n\n")
	towels := Towels{}.Parse(data[0])
	designs := Designs{}.Parse(data[1])
	counts := designs.Count(*towels)
	acc := 0
	for _, c := range counts {
		if c > 0 {
			acc++
		}
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	data := strings.Split(strings.TrimSpace(input_data), "\n\n")
	towels := Towels{}.Parse(data[0])
	designs := Designs{}.Parse(data[1])
	counts := designs.Count(*towels)
	acc := 0
	for _, c := range counts {
		acc += c
	}
	// 705450725313648 - Low
	return acc, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

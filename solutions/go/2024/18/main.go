package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 18
* Go Solution
*
* Day 18: RAM Run
*
* https://adventofcode.com/2024/day/18
*
 */

import (
	"container/heap"
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Vec               struct{ y, x int }
	Dir               Vec
	Set[T comparable] map[T]struct{}
	PriorityQueue     struct {
		q []*PositionItem
		s Set[Position]
	}
	Position     Vec
	PositionItem struct {
		value    Position
		priority int
		index    int
	}
	Score     map[Position]int
	CameFrom  map[Position]Position
	Historian struct {
		p   Position
		end Position
	}
	Memory struct {
		sides     int
		corrupted [][]bool
		falling   []Position
	}
)

var (
	UP         = Dir{-1, 0}
	RIGHT      = Dir{0, 1}
	DOWN       = Dir{1, 0}
	LEFT       = Dir{0, -1}
	DIRECTIONS = []Dir{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
)

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

func (s Set[T]) Extend(g []T) {
	for _, item := range g {
		s.Add(item)
	}
}

func (s Set[T]) Remove(g T) {
	delete(s, g)
}

func (pq PriorityQueue) Len() int { return len(pq.q) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq.q[i].priority < pq.q[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq.q[i], pq.q[j] = pq.q[j], pq.q[i]
	pq.q[i].index = i
	pq.q[j].index = j
}

func (pq PriorityQueue) New(N int) *PriorityQueue {
	pq.q = make([]*PositionItem, N)
	pq.s = Set[Position]{}
	for i := range N {
		pq.q[i] = &PositionItem{
			value:    Position{},
			priority: math.MaxInt,
			index:    i,
		}
	}
	return &pq
}

func (pq *PriorityQueue) PushPosition(position Position, priority int) {
	positionItem := &PositionItem{position, priority, 0}
	heap.Push(pq, positionItem)
}

func (pq *PriorityQueue) PopPosition() *Position {
	if pq.Len() == 0 {
		return nil
	}
	item := heap.Pop(pq).(*PositionItem)
	return &item.value
}

func (pq *PriorityQueue) Push(x any) {
	n := len((*pq).q)
	item := x.(*PositionItem)
	item.index = n
	(*pq).q = append((*pq).q, item)
	(*pq).s.Add(item.value)
}

func (pq *PriorityQueue) Pop() any {
	old := (*pq).q
	n := len(old)
	if n == 0 {
		return nil
	}
	item := old[n-1]
	old[n-1] = nil
	item.index = -1
	(*pq).q = old[0 : n-1]
	(*pq).s.Remove(item.value)
	return item
}

func (pq *PriorityQueue) Contains(m Position) bool {
	return (*pq).s.Contains(m)
}

func (pq *PriorityQueue) update(item *PositionItem, value Position, priority int) {
	item.value = value
	item.priority = priority
	heap.Fix(pq, item.index)
}

func (m Memory) Parse(input_data string) *Memory {
	m.corrupted = make([][]bool, m.sides)
	for y := range m.sides {
		m.corrupted[y] = make([]bool, m.sides)
	}

	for _, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		values := strings.Split(line, ",")
		x, _ := strconv.Atoi(values[0])
		y, _ := strconv.Atoi(values[1])
		m.falling = append(m.falling, Position{y, x})
	}
	return &m
}

func (m *Memory) Clone() *Memory {
	nm := Memory{sides: m.sides}
	nm.corrupted = make([][]bool, m.sides)
	for y := range m.sides {
		nm.corrupted[y] = make([]bool, m.sides)
		for x := range m.sides {
			nm.corrupted[y][x] = m.corrupted[y][x]
		}
	}
	nm.falling = make([]Position, len(m.falling))
	for i, fall := range m.falling {
		nm.falling[i] = fall
	}
	return &nm
}

func (m *Memory) Fall() {
	f := m.falling[0]
	m.falling = m.falling[1:]
	m.corrupted[f.y][f.x] = true
}

func (m *Memory) Wait(time int) {
	for range time {
		m.Fall()
	}
}

func (m *Memory) String() string {
	s := make([]string, m.sides)
	for y := range m.sides {
		s[y] = ""
		for x := range m.sides {
			switch m.corrupted[y][x] {
			case true:
				s[y] += "#"
			case false:
				s[y] += "."
			}
		}
	}
	return strings.Join(s, "\n")
}

func (m *Memory) PathString(positions []Position) string {
	pSet := Set[Position]{}
	for _, pos := range positions {
		pSet.Add(pos)
	}
	s := make([]string, m.sides)
	for y := range m.sides {
		s[y] = ""
		for x := range m.sides {
			if pSet.Contains(Position{y, x}) {
				s[y] += "O"
			} else if m.corrupted[y][x] {
				s[y] += "#"
			} else {
				s[y] += "."
			}
		}
	}
	return strings.Join(s, "\n")
}

func (h *Historian) RebuildPath(cameFrom *CameFrom, curr Position) []Position {
	parent, ok := (*cameFrom)[curr]
	if !ok {
		return []Position{curr}
	}
	return append(h.RebuildPath(cameFrom, parent), curr)
}

func (h *Historian) PrepareEscape(m *Memory) (PriorityQueue, Score, CameFrom) {
	// 1. Priority Queue
	N := 1024
	pq := PriorityQueue{}.New(N)
	start := Position(h.p)
	pq.PushPosition(start, 0)

	// 2. Current cost map
	gScore := Score{}
	for y, line := range m.corrupted {
		for x, corrupted := range line {
			if corrupted {
				continue
			}
			gScore[Position{y, x}] = math.MaxInt
		}
	}
	gScore[start] = 0

	cameFrom := CameFrom{}
	return *pq, gScore, cameFrom
}

func (h *Historian) Escape(m *Memory) ([]Position, int) {
	pq, gScore, cameFrom := h.PrepareEscape(m)
	for curr := pq.PopPosition(); curr != nil; curr = pq.PopPosition() {
		if *curr == h.end {
			return h.RebuildPath(&cameFrom, *curr), gScore[*curr]
		}
		for _, dir := range DIRECTIONS {
			np := Position{curr.y + dir.y, curr.x + dir.x}
			if np.y < 0 || np.y >= m.sides || np.x < 0 || np.x >= m.sides ||
				m.corrupted[np.y][np.x] {
				continue
			}
			nScore := gScore[*curr] + 1
			if _, ok := gScore[np]; !ok || nScore < gScore[np] {
				cameFrom[np] = Position(*curr)
				gScore[np] = nScore
				if !pq.Contains(np) {
					pq.PushPosition(np, nScore)
				}
			} else if nScore == gScore[np] {
				cameFrom[np] = Position(*curr)
			}
		}
	}
	return []Position{}, 0
}

func ParseArguments(args []string) (int, int) {
	var sides int
	var wait int
	if len(args) != 0 {
		sides, _ = strconv.Atoi(args[0])
		wait, _ = strconv.Atoi(args[1])
	} else {
		sides = 70
		wait = 1024
	}
	return sides, wait
}

func (h *Historian) SearchBlockingByte(m *Memory) Position {
	var mid int
	low, high := 0, len(m.falling)
	for low <= high {
		mid = (high + low) / 2
		clone := m.Clone()
		clone.Wait(mid)
		_, s := h.Escape(clone)
		if s == 0 {
			high = mid - 1
		} else {
			low = mid + 1
		}
	}
	return m.falling[mid-1]
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	sides, wait := ParseArguments(args)
	memory := Memory{sides: sides + 1}.Parse(input_data)
	memory.Wait(wait)
	you := Historian{Position{0, 0}, Position{sides, sides}}
	path, _ := you.Escape(memory)
	return len(path) - 1, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	sides, _ := ParseArguments(args)
	memory := Memory{sides: sides + 1}.Parse(input_data)
	you := Historian{Position{0, 0}, Position{sides, sides}}
	answer := you.SearchBlockingByte(memory)
	return fmt.Sprintf("%d,%d", answer.x, answer.y), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

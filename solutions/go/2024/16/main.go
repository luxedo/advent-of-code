package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 16
* Go Solution
*
* Day 16: Reindeer Maze
*
* https://adventofcode.com/2024/day/16
*
 */

import (
	"container/heap"
	"fmt"
	"math"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	PriorityQueue     struct {
		q []*MoveItem
		s Set[Move]
	}
	Vec  struct{ y, x int }
	Dir  struct{ y, x int }
	Move struct {
		p Vec
		d Dir
	}
	Score    map[Move]int
	CameFrom map[Move][]Move
	Cell     rune
	Maze     struct {
		height, width int
		start         Vec
		end           Vec
		grid          [][]Cell
	}
	MoveItem struct {
		value    Move
		priority int
		index    int
	}
	Solver struct{}
)

var (
	UP         = Dir{-1, 0}
	RIGHT      = Dir{0, 1}
	DOWN       = Dir{1, 0}
	LEFT       = Dir{0, -1}
	DIRECTIONS = []Dir{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
)

const (
	WALL     Cell = '#'
	CORRIDOR Cell = '.'
	START    Cell = 'S'
	END      Cell = 'E'
	PATH     Cell = '*'
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

func Abs(num int) int {
	if num < 0 {
		return -num
	}
	return num
}

func (m Move) String() string {
	var c rune
	switch m.d {
	case UP:
		c = '^'
	case RIGHT:
		c = '>'
	case DOWN:
		c = 'v'
	case LEFT:
		c = '<'

	}
	return fmt.Sprintf("(%v%c)", m.p, c)
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
	pq.q = make([]*MoveItem, N)
	pq.s = Set[Move]{}
	for i := range N {
		pq.q[i] = &MoveItem{
			value:    Move{},
			priority: math.MaxInt,
			index:    i,
		}
	}
	return &pq
}

func (pq *PriorityQueue) PushMove(move Move, priority int) {
	moveItem := &MoveItem{move, priority, 0}
	heap.Push(pq, moveItem)
}

func (pq *PriorityQueue) PopMove() *Move {
	item := heap.Pop(pq).(*MoveItem)
	if item == nil {
		return nil
	}
	return &item.value
}

func (pq *PriorityQueue) Push(x any) {
	n := len((*pq).q)
	item := x.(*MoveItem)
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

func (pq *PriorityQueue) Contains(m Move) bool {
	return (*pq).s.Contains(m)
}

func (pq *PriorityQueue) update(item *MoveItem, value Move, priority int) {
	item.value = value
	item.priority = priority
	heap.Fix(pq, item.index)
}

func (c Cell) String() string {
	return string(c)
}

func (m *Maze) String() string {
	s := ""
	for y, line := range m.grid {
		for x, c := range line {
			if y == m.start.y && x == m.start.x {
				s += string(START)
			} else if y == m.end.y && x == m.end.x {
				s += string(END)
			} else {
				s += string(c)
			}
		}
		s += "\n"
	}
	return s
}

func (m Maze) Parse(input_data string) *Maze {
	rows := strings.Split(strings.TrimSpace(input_data), "\n")
	m.height, m.width = len(rows), len(rows[0])
	m.grid = make([][]Cell, m.height)
	for y, line := range rows {
		m.grid[y] = make([]Cell, m.width)
		for x, c := range line {
			switch c {
			case rune(WALL), rune(CORRIDOR):
				m.grid[y][x] = Cell(c)
			case rune(START):
				m.grid[y][x] = Cell(CORRIDOR)
				m.start = Vec{y, x}
			case rune(END):
				m.grid[y][x] = Cell(CORRIDOR)
				m.end = Vec{y, x}
			}
		}
	}
	return &m
}

func (m *Maze) Heuristic(v Vec) int {
	return Abs(m.end.y-v.y) + Abs(m.end.x-v.x)
}

func (m *Maze) Cost(curr Move, next Move) int {
	if curr.d != next.d {
		return 1000
	}
	return 1
}

func (m *Maze) MovesVec(v Vec) []Dir {
	n := []Dir{}
	for _, d := range DIRECTIONS {
		y, x := v.y+d.y, v.x+d.x
		if y < 0 || y >= m.height || x < 0 || x >= m.width {
			continue
		}
		if m.grid[y][x] == CORRIDOR {
			n = append(n, d)
		}
	}
	return n
}

func (m *Maze) Moves(curr Move) []Move {
	n := []Move{}
	for _, d := range DIRECTIONS {
		y, x := curr.p.y+d.y, curr.p.x+d.x
		if y < 0 || y >= m.height || x < 0 || x >= m.width {
			continue
		}
		if (curr.d.y+d.y == 0) && (curr.d.x+d.x == 0) {
			continue
		}
		if m.grid[y][x] != CORRIDOR {
			continue
		}
		var nextMove Move
		if curr.d == d {
			nextMove = Move{Vec{curr.p.y + d.y, curr.p.x + d.x}, d}
		} else {
			nextMove = Move{curr.p, d}
		}
		n = append(n, nextMove)
	}
	return n
}

func (m *Maze) Clone() *Maze {
	nm := Maze{}
	nm.height, nm.width = m.height, m.width
	nm.start, nm.end = m.start, m.end
	nm.grid = make([][]Cell, nm.height)
	for y, line := range m.grid {
		nm.grid[y] = make([]Cell, nm.width)
		for x, c := range line {
			nm.grid[y][x] = c
		}
	}
	return &nm
}

func (m *Maze) Format(solution []Move) string {
	nm := m.Clone()
	for _, path := range solution {
		nm.grid[path.p.y][path.p.x] = Cell(PATH)
	}
	return nm.String()
}

func (s *Solver) DijkstraSetup(maze *Maze, start Vec) (PriorityQueue, Score, CameFrom) {
	// 1. Priority Queue
	N := 1024
	pq := PriorityQueue{}.New(N)
	move := Move{start, RIGHT}
	pq.PushMove(move, 0)

	// 2. Current cost map
	gScore := Score{}
	for y, line := range maze.grid {
		for x, c := range line {
			if c == CORRIDOR {
				for _, d := range DIRECTIONS {
					gScore[Move{Vec{y, x}, d}] = math.MaxInt
				}
			}
		}
	}
	gScore[move] = 0

	cameFrom := CameFrom{}
	return *pq, gScore, cameFrom
}

func (s *Solver) Dijkstra(maze *Maze, start Vec, end Vec) ([][]Move, int) {
	pq, gScore, cameFrom := s.DijkstraSetup(maze, start)
	for curr := pq.PopMove(); curr != nil; curr = pq.PopMove() {
		if curr.p == end {
			return s.RebuildPaths(&cameFrom, *curr), gScore[*curr]
		}
		for _, nextMove := range maze.Moves(*curr) {
			nScore := gScore[*curr] + maze.Cost(*curr, nextMove)
			if _, ok := gScore[nextMove]; !ok || nScore < gScore[nextMove] {
				cameFrom[nextMove] = []Move{*curr}
				gScore[nextMove] = nScore
				if !pq.Contains(nextMove) {
					pq.PushMove(nextMove, nScore)
				} 
			} else if nScore == gScore[nextMove] {
				cameFrom[nextMove] = append(cameFrom[nextMove], *curr)
			}
		}
	}
	return [][]Move{}, 0
}

func (s *Solver) RebuildPaths(cameFrom *CameFrom, curr Move) [][]Move {
	parents, ok := (*cameFrom)[curr]
	if !ok {
		return [][]Move{{curr}}
	}
	allPaths := [][]Move{}
	for _, parent := range parents {
		pathsFromParent := s.RebuildPaths(cameFrom, parent)
		for _, path := range pathsFromParent {
			allPaths = append(allPaths, append(path, curr))
		}
	}
	return allPaths
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	maze := Maze{}.Parse(input_data)
	solver := Solver{}
	_, score := solver.Dijkstra(maze, maze.start, maze.end)
	return score, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	maze := Maze{}.Parse(input_data)
	solver := Solver{}
	paths, _ := solver.Dijkstra(maze, maze.start, maze.end)
	// for _, path := range paths {
		// fmt.Println(maze.Format(path))
	// }
	allTiles := Set[Vec]{}
	for _, path := range paths {
		for _, move := range path {
			allTiles.Add(move.p)
		}
	}
	return len(allTiles), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

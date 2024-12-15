package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 15
* Go Solution
*
* Day 15: Warehouse Woes
*
* https://adventofcode.com/2024/day/15
*
 */

import (
	"errors"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Action            byte
	Object            byte
	Move              struct {
		y, x int
	}
	Point struct {
		y, x int
	}
	Robot struct {
		t *Object
		p Point
	}
	Warehouse struct {
		height, width int
		robot         Robot
		grid          [][]*Object
	}
	Actions []Action
	Moves   []Move
)

const (
	WALL  Object = '#'
	BOX   Object = 'O'
	BOXL  Object = '['
	BOXR  Object = ']'
	FREE  Object = '.'
	ROBOT Object = '@'
)

const (
	UP    Action = '^'
	RIGHT Action = '>'
	DOWN  Action = 'v'
	LEFT  Action = '<'
)

var (
	UpMove    Move = Move{-1, 0}
	RightMove Move = Move{0, 1}
	DownMove  Move = Move{1, 0}
	LeftMove  Move = Move{0, -1}
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

func (s Set[T]) ToSlice() []T {
	ret := []T{}
	for key := range s {
		ret = append(ret, key)
	}
	return ret
}

func (a Action) String() string {
	return string(a)
}

func (m Move) String() string {
	switch m {
	case UpMove:
		return string(UP)
	case RightMove:
		return string(RIGHT)
	case DownMove:
		return string(DOWN)
	case LeftMove:
		return string(LEFT)
	default:
		panic("Oh no! Can't convert move")
	}
}

func (a Actions) ParseAction(aStr rune) (Action, error) {
	switch aStr {
	case rune(UP):
		return UP, nil
	case rune(RIGHT):
		return RIGHT, nil
	case rune(DOWN):
		return DOWN, nil
	case rune(LEFT):
		return LEFT, nil
	default:
		return Action(0), errors.New("Cannot parse action")
	}
}

func (a Actions) Parse(actionsStr string) Actions {
	for _, c := range actionsStr {
		action, err := a.ParseAction(c)
		if err != nil {
			continue
		}
		a = append(a, action)
	}
	return a
}

func (m Move) From(action Action) Move {
	switch action {
	case UP:
		m = UpMove
	case RIGHT:
		m = RightMove
	case DOWN:
		m = DownMove
	case LEFT:
		m = LeftMove
	}
	return m
}

func (Moves) From(actions Actions) Moves {
	moves := make(Moves, len(actions))
	for i, action := range actions {
		moves[i] = Move{}.From(action)
	}
	return moves
}

func (o *Object) String() string {
	return string(*o)
}

func ParseObject(tRune rune) *Object {
	var o Object
	switch tRune {
	case rune(WALL):
		o = WALL
	case rune(BOX):
		o = BOX
	case rune(BOXL):
		o = BOXL
	case rune(BOXR):
		o = BOXR
	case rune(FREE):
		o = FREE
	case rune(ROBOT):
		o = ROBOT
	default:
		panic("Oh no! Can't parse!")
	}
	return &o
}

func (Warehouse) ParseGrid(gridStr string) ([][]*Object, Robot) {
	rows := strings.Split(gridStr, "\n")
	height, width := len(rows), len(rows[0])
	grid := make([][]*Object, height)
	var robot Robot
	for y, line := range rows {
		grid[y] = make([]*Object, width)
		for x, c := range line {
			object := ParseObject(c)
			grid[y][x] = object
			if *object == ROBOT {
				robot.t = object
				robot.p = Point{y, x}
				grid[y][x] = ParseObject('.')
			}
		}
	}
	return grid, robot
}

func (w Warehouse) Parse(input_data string) (*Warehouse, Moves) {
	input := strings.Split(strings.TrimSpace(input_data), "\n\n")
	w.grid, w.robot = Warehouse{}.ParseGrid(input[0])
	w.height, w.width = len(w.grid), len(w.grid[0])
	actions := Actions{}.Parse(input[1])
	moves := Moves{}.From(actions)
	return &w, moves
}

func (w Warehouse) ParseWide(input_data string) (*Warehouse, Moves) {
	ware, moves := w.Parse(input_data)
	ngrid := make([][]*Object, ware.height)
	for y, row := range ware.grid {
		ngrid[y] = make([]*Object, 2*ware.width)
		for x, c := range row {
			var o1, o2 *Object
			switch *c {
			case '#':
				o1 = ParseObject('#')
				o2 = ParseObject('#')
			case 'O':
				o1 = ParseObject('[')
				o2 = ParseObject(']')
			case '.':
				o1 = ParseObject('.')
				o2 = ParseObject('.')
			}
			ngrid[y][2*x] = o1
			ngrid[y][2*x+1] = o2
		}
	}
	ware.grid = ngrid
	ware.width = len(ware.grid[0])
	ware.robot.p.x *= 2
	return ware, moves
}

func (w *Warehouse) String() string {
	m := make([][]rune, w.height)
	for y := range w.height {
		m[y] = make([]rune, w.width)
		for x := range w.width {
			if w.robot.p.y == y && w.robot.p.x == x {
				m[y][x] = rune(ROBOT)
			} else {
				m[y][x] = rune(*w.grid[y][x])
			}
		}
	}
	var s string
	for _, row := range m {
		for _, r := range row {
			s += string(r)
		}
		s += "\n"
	}
	return s
}

func (w *Warehouse) MoveAll(moves Moves) {
	for _, move := range moves {
		w.Move(move)
	}
}

func (w *Warehouse) Move(move Move) {
	if move == RightMove || move == LeftMove {
		w.MoveHorizontal(move)
	} else {
		w.MoveVertical(move)
	}
}

func (w *Warehouse) MoveHorizontal(move Move) {
	curr := Point{w.robot.p.y + move.y, w.robot.p.x + move.x}
	moveCoords := []Point{}
	for curr.x > 0 && curr.x < w.width {
		object := w.grid[curr.y][curr.x]
		switch *object {
		case BOX, BOXL, BOXR:
			moveCoords = append(moveCoords, curr)
			curr.y += move.y
			curr.x += move.x
		case WALL:
			return
		case FREE:
			w.MoveObjects(move, moveCoords)
			w.robot.p.y += move.y
			w.robot.p.x += move.x
			return
		default:
			panic("What should I do?!?!")
		}
	}
}

func (w *Warehouse) MoveVertical(move Move) {
	curr := Point{w.robot.p.y + move.y, w.robot.p.x + move.x}
	moveFront := []Point{curr}
	switch *w.grid[curr.y][curr.x] {
	case FREE:
		w.robot.p.y += move.y
		w.robot.p.x += move.x
		return
	case WALL:
		return
	case BOXL:
		moveFront = append(moveFront, Point{curr.y, curr.x + 1})
	case BOXR:
		moveFront = append(moveFront, Point{curr.y, curr.x - 1})
	case BOX:
	default:
		panic("What should I do?!?!")

	}

	y := curr.y + move.y
	moveCoords := Set[Point]{}
	moveCoords.Extend(moveFront)
	for y > 0 && y < w.height {
		newFront := []Point{}
		for _, coord := range moveFront {
			object := w.grid[y][coord.x]
			switch *object {
			case WALL:
				return
			case FREE:
				continue
			case BOX:
				newFront = append(newFront, Point{y, coord.x})
			case BOXL:
				newFront = append(newFront, Point{y, coord.x}, Point{y, coord.x + 1})
			case BOXR:
				newFront = append(newFront, Point{y, coord.x}, Point{y, coord.x - 1})
			}
		}
		if len(newFront) == 0 {
			w.MoveObjects(move, moveCoords.ToSlice())
			w.robot.p.y += move.y
			w.robot.p.x += move.x
			break
		}
		y += move.y
		moveCoords.Extend(newFront)
		moveFront = append(moveFront[:0], newFront...)
	}
}

func (w *Warehouse) MoveObjects(move Move, moveCoords []Point) {
	newObjects := make([]*Object, len(moveCoords))
	for i, coord := range moveCoords {
		newObjects[i] = w.grid[coord.y][coord.x]
		w.grid[coord.y][coord.x] = ParseObject('.')
	}
	for i, object := range newObjects {
		coord := moveCoords[i]
		w.grid[coord.y+move.y][coord.x+move.x] = object
	}
}

func (w *Warehouse) GPS() int {
	acc := 0
	for y, row := range w.grid {
		for x, c := range row {
			if *c != BOX && *c != BOXL {
				continue
			}
			acc += 100*y + x
		}
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	warehouse, moves := Warehouse{}.Parse(input_data)
	warehouse.MoveAll(moves)
	return warehouse.GPS(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	warehouse, moves := Warehouse{}.ParseWide(input_data)
	warehouse.MoveAll(moves)
	return warehouse.GPS(), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

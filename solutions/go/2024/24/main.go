package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 24
* Go Solution
*
* Day 24: Crossed Wires
*
* https://adventofcode.com/2024/day/24
*
 */

import (
	"fmt"
	"regexp"
	"sort"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	WireState         uint8
	Wire              struct {
		value WireState
		name  WireName
		io    bool
	}
	CircuitIO []*Wire
	WireName  [3]byte
	Circuit   struct {
		initialState map[WireName]WireState
		x            CircuitIO
		y            CircuitIO
		z            CircuitIO
		wires        map[WireName]*Wire
		gates        Gates
	}
	GateType string
	GatePair [2]*Gate
	Gate     struct {
		t      GateType
		inputA *Wire
		inputB *Wire
		output *Wire
	}
	Gates struct {
		gate map[*Wire]*Gate
		name map[WireName]*Gate
	}
)

const (
	False    WireState = 0
	True               = 1
	Off                = 255
	GATE_AND GateType  = "AND"
	GATE_OR            = "OR"
	GATE_XOR           = "XOR"
)

func SetBitIndices(num uint) Set[int] {
	indices := Set[int]{}
	i := 0
	for num > 0 {
		if (num & 1) == 1 {
			indices.Add(i)
		}
		num >>= 1
		i++
	}
	return indices
}

func (s Set[T]) Clone() Set[T] {
	ns := Set[T]{}
	for k := range s {
		ns.Add(k)
	}
	return ns
}

func (s Set[T]) Add(value T) Set[T] {
	s[value] = struct{}{}
	return s
}

func (s Set[T]) Remove(value T) Set[T] {
	delete(s, value)
	return s
}

func (s Set[T]) Union(other Set[T]) Set[T] {
	for k := range other {
		s.Add(k)
	}
	return s
}

func (s Set[T]) Diff(other Set[T]) Set[T] {
	for k := range other {
		s.Remove(k)
	}
	return s
}

func (s Set[T]) Intersection(other Set[T]) Set[T] {
	ns := Set[T]{}
	for k := range s {
		if other.Contains(k) {
			ns.Add(k)
		}
	}

	return ns
}

func (s Set[T]) Equals(other Set[T]) bool {
	if len(s) != len(other) {
		return false
	}

	for k := range s {
		if !other.Contains(k) {
			return false
		}
	}
	for k := range other {
		if !s.Contains(k) {
			return false
		}
	}
	return true
}

func (s Set[T]) Contains(value T) bool {
	_, ok := s[value]
	return ok
}

func NewWireState(s string) WireState {
	switch s {
	case "0":
		return False
	case "1":
		return True
	default:
		panic("Oh no! Can't parse Wire")
	}
}

func (w Wire) String() string {
	switch w.value {
	case True:
		return fmt.Sprintf("%s: 1", w.name)
	case False:
		return fmt.Sprintf("%s: 0", w.name)
	case Off:
		return fmt.Sprintf("%s: [Off]", w.name)
	default:
		panic("Wait! Weird wire!")
	}
}

func (w *Wire) SetStr(value string) {
	switch value {
	case "0":
		w.Set(False)
	case "1":
		w.Set(True)
	default:
		panic("Oh no! Can't parse Wire")
	}
}

func (w *Wire) Set(v WireState) {
	w.value = v
}

func (w *Wire) Get() WireState {
	return w.value
}

func (w *Wire) On() bool {
	return w.value != Off
}

func (w *Wire) Reset() {
	w.value = Off
}

func (w WireName) Parse(value string) WireName {
	copy(w[:], value)
	return w
}

func (w WireName) String() string {
	return fmt.Sprintf("%c%c%c", w[0], w[1], w[2])
}

func (g Gate) String() string {
	return fmt.Sprintf("%s{%v %v -> %v}", g.t, g.inputA, g.inputB, g.output)
}

func (g *Gate) SwitchOutput(w *Wire) {
	g.output = w
}

func (g Gate) Eval() {
	if g.inputA.On() && g.inputB.On() && !g.output.On() {
		switch g.t {
		case GATE_AND:
			g.output.Set(g.inputA.Get() & g.inputB.Get())
		case GATE_OR:
			g.output.Set(g.inputA.Get() | g.inputB.Get())
		case GATE_XOR:
			g.output.Set(g.inputA.Get() ^ g.inputB.Get())
		}
	}
}

func (g Gate) New(gateStr string, wireA, wireB, output *Wire) Gate {
	g.inputA = wireA
	g.inputB = wireB
	g.output = output
	switch GateType(gateStr) {
	case GATE_AND:
		g.t = GATE_AND
	case GATE_OR:
		g.t = GATE_OR
	case GATE_XOR:
		g.t = GATE_XOR
	default:
		panic("Oh no! Can't create gate")
	}
	return g
}

func (g Gates) Parse(gatesStr string) (Gates, Circuit) {
	circuit := Circuit{}.New()
	g.gate = make(map[*Wire]*Gate)
	g.name = make(map[WireName]*Gate)
	re := regexp.MustCompile(
		`^([^\s]{3}) (` + string(
			GATE_AND,
		) + `|` + string(
			GATE_OR,
		) + `|` + string(
			GATE_XOR,
		) + `) ([^\s]{3}) -> ([^\s]{3})$`,
	)
	for _, line := range strings.Split(gatesStr, "\n") {
		values := re.FindStringSubmatch(line)
		wireNameA := WireName{}.Parse(values[1])
		wireNameB := WireName{}.Parse(values[3])
		wireNameOut := WireName{}.Parse(values[4])

		inputA, ok := circuit.Get(wireNameA)
		if !ok {
			inputA = &Wire{Off, wireNameA, false}
			circuit.wires[wireNameA] = inputA
		}
		inputB, ok := circuit.Get(wireNameB)
		if !ok {
			inputB = &Wire{Off, wireNameB, false}
			circuit.wires[wireNameB] = inputB
		}
		output, ok := circuit.Get(wireNameOut)
		if !ok {
			output = &Wire{Off, wireNameOut, false}
			circuit.wires[wireNameOut] = output
		}

		gate := Gate{}.New(values[2], inputA, inputB, output)
		g.gate[output] = &gate
		g.name[wireNameOut] = &gate
	}
	circuit.gates = g
	return g, circuit.LinkIO()
}

func (c CircuitIO) Value() (uint, bool) {
	ret := uint(0)
	for i := len(c) - 1; i >= 0; i-- {
		wire := c[i]
		if !wire.On() {
			return ret, false
		}
		ret <<= 1
		ret |= uint(wire.Get())
	}
	return ret, true
}

func (c CircuitIO) GetBit(index int) WireState {
	return c[index].Get()
}

func (c Circuit) New() Circuit {
	c.wires = map[WireName]*Wire{}
	c.x = CircuitIO{}
	c.y = CircuitIO{}
	c.z = CircuitIO{}
	return c
}

func (c *Circuit) Reset() *Circuit {
	for _, v := range c.wires {
		v.Reset()
	}
	for k, v := range c.initialState {
		c.Set(k, v)
	}
	return c
}

func (c *Circuit) Set(name WireName, state WireState) *Circuit {
	c.wires[name].Set(state)
	return c
}

func (c *Circuit) SetMany(names []WireName, states []WireState) *Circuit {
	for i, name := range names {
		c.Set(name, states[i])
	}
	return c
}

func (c Circuit) Parse(input_data string) *Circuit {
	data := strings.Split(strings.TrimSpace(input_data), "\n\n")
	_, circuit := Gates{}.Parse(data[1])
	circuit.SetInitialStateStr(data[0]).Reset()
	return &circuit
}

func (c *Circuit) SetInitialStateStr(wiresStr string) *Circuit {
	initialState := map[WireName]WireState{}
	for _, line := range strings.Split(wiresStr, "\n") {
		values := strings.Split(line, ": ")
		name := WireName{}.Parse(values[0])
		state := NewWireState(values[1])
		initialState[name] = state
	}
	c.initialState = initialState
	return c
}

func (c *Circuit) SetState(x uint, y uint) *Circuit {
	setX := SetBitIndices(x)
	setY := SetBitIndices(y)
	for i := range c.x {
		if setX.Contains(i) {
			(*c).x[i].value = True
		} else {
			(*c).x[i].value = False
		}
	}
	for i := range c.y {
		if setY.Contains(i) {
			(*c).y[i].value = True
		} else {
			(*c).y[i].value = False
		}
	}
	return c
}

func (c *Circuit) GetChain(wire *Wire) Set[*Gate] {
	gate := (*c).gates.gate[wire]
	inputA, inputB := (*gate).inputA, (*gate).inputB
	ret := Set[*Gate]{}
	ret.Add(gate)
	if !inputA.io {
		ret.Union(c.GetChain(inputA))
	}
	if !inputB.io {
		ret.Union(c.GetChain(inputB))
	}
	return ret
}

func (c Circuit) Get(name WireName) (*Wire, bool) {
	wire, ok := c.wires[name]
	return wire, ok
}

func (c Circuit) LinkIO() Circuit {
	for i := 0; i < 100; i++ {
		nameX := WireName{}.Parse(fmt.Sprintf("x%02d", i))
		nameY := WireName{}.Parse(fmt.Sprintf("y%02d", i))
		nameZ := WireName{}.Parse(fmt.Sprintf("z%02d", i))
		wireX, okX := c.wires[nameX]
		if okX {
			wireX.io = true
			c.x = append(c.x, wireX)
		}
		wireY, okY := c.wires[nameY]
		if okY {
			wireY.io = true
			c.y = append(c.y, wireY)
		}
		wireZ, okZ := c.wires[nameZ]
		if okZ {
			wireZ.io = true
			c.z = append(c.z, wireZ)
		}
		if !okX && !okY && !okZ {
			break
		}
	}
	return c
}

func (c *Circuit) SwapOutputs(g0 *Gate, g1 *Gate) {
	tmp0, tmp1 := (*g0).output, (*g1).output
	(*g0).SwitchOutput(tmp1)
	(*g1).SwitchOutput(tmp0)
	(*c).gates.gate[tmp0] = g1
	(*c).gates.gate[tmp1] = g0
	(*c).gates.name[tmp0.name] = g1
	(*c).gates.name[tmp1.name] = g0
}

func (c *Circuit) Clock(gate *Gate, seen Set[*Gate]) bool {
	if seen.Contains(gate) {
		// Theres a cycle in the graph
		return false
	}
	seen.Add(gate)
	wireA, wireB := (*gate).inputA, (*gate).inputB
	if !wireA.On() {
		if ok := c.Clock((*c).gates.gate[wireA], seen); !ok {
			return ok
		}
	}
	if !wireB.On() {
		if ok := c.Clock((*c).gates.gate[wireB], seen); !ok {
			return ok
		}
	}
	(*gate).Eval()
	return true
}

func (c *Circuit) Run() bool {
	for _, wire := range c.z {
		if !(*wire).On() {
			seen := Set[*Gate]{}
			if ok := c.Clock((*c).gates.gate[wire], seen); !ok {
				return false
			}
		}
	}
	return true
}

func (c *Circuit) CheckErrors(expected uint) (Set[int], bool) {
	actualZ, ok := c.z.Value()
	zDiff := (expected ^ actualZ) & (1<<len(c.z) - 1)
	return SetBitIndices(zDiff), ok
}

func (g GatePair) New(g0 *Gate, g1 *Gate) GatePair {
	if (*g0.output).name[0] < (*g1.output).name[0] {
		g0, g1 = g1, g0
	} else if (*g0.output).name[0] == (*g1.output).name[0] {
		if (*g0.output).name[1] < (*g1.output).name[1] {
			g0, g1 = g1, g0
		} else if (*g0.output).name[1] == (*g1.output).name[1] {
			if (*g0.output).name[2] < (*g1.output).name[2] {
				g0, g1 = g1, g0
			}
		}
	}
	g[0] = g0
	g[1] = g1

	return g
}

func (c *Circuit) Compare(
	x, y, z uint,
) (Set[int], Set[int], bool) {
	ok := c.Reset().SetState(x, y).Run()
	expected := SetBitIndices(z)
	actualZ, _ := c.z.Value()
	actual := SetBitIndices(actualZ)
	return expected, actual, ok
}

func (c *CircuitIO) GetWire(index int) *Wire {
	return (*c)[index]
}

func (c *Circuit) SwapAndTest(
	candidates Set[*Gate],
	i int,
	allCandidates map[int]Set[*Gate],
) Set[[2]*Gate] {
	seen := Set[*Gate]{}
	newCandidates := Set[[2]*Gate]{}
	for g0 := range candidates {
		seen.Add(g0)
	outer:
		for g1 := range candidates {
			if seen.Contains(g1) {
				continue
			}
			c.SwapOutputs(g0, g1)
			nc, ok := c.ValidateAllBits()
			c.SwapOutputs(g0, g1)

			if !ok || len(nc) >= len(allCandidates) {
				continue
			}

			for k := range nc {
				if _, ok := allCandidates[k]; !ok {
					continue outer
				}
			}
			newCandidates.Add([2]*Gate{g0, g1})
		}
	}
	return newCandidates
}

func (c *Circuit) ValidateAllBits() (map[int]Set[*Gate], bool) {
	candidates := map[int]Set[*Gate]{}
	for i := range len(c.x) {
		// x := uint(0)
		x := uint(1 << i)
		// y := uint(1 << i)
		y := uint(0)
		z := x + y
		expected, actual, ok := c.Compare(x, y, z)
		if !ok {
			return candidates, ok
		}
		if !expected.Equals(actual) {
			if _, ok := candidates[i]; !ok {
				candidates[i] = Set[*Gate]{}
			}
			for idxActual := range actual {
				wireActual := c.z.GetWire(idxActual)
				cActual := c.GetChain(wireActual)
				candidates[i].Union(cActual)
			}
			for idxExpected := range expected {
				wireExpected := c.z.GetWire(idxExpected)
				cExpected := c.GetChain(wireExpected)
				candidates[i].Union(cExpected)
			}
		}
	}

	return candidates, true
}

func (c *Circuit) FixErrorGates(candidatePairs []Set[[2]*Gate]) []*Gate {
	if len(candidatePairs) == 0 {
		return []*Gate{}
	}

	pairs := candidatePairs[0]
	newPairs := candidatePairs[1:]
	ret := []*Gate{}
	for p0 := range pairs {
		g0, g1 := p0[0], p0[1]
		c.SwapOutputs(g0, g1)
		errors, ok := c.ValidateAllBits()
		ret = c.FixErrorGates(newPairs)
		if len(errors) == 0 {
			ok := c.FuzzTest()
			if ok {
				c.SwapOutputs(g0, g1)
				return []*Gate{g0, g1}
			}
		}
		c.SwapOutputs(g0, g1)

		if !ok {
			continue
		}

		if len(ret) != 0 {
			return append(ret, g0, g1)
		}
	}
	return ret
}

func (c *Circuit) Test(x, y int) bool {
	z := (x + y) & ((1 << len(c.z)) - 1)
	expected, actual, ok := c.Compare(uint(x), uint(y), uint(z))
	return ok && expected.Equals(actual)
}

func (c *Circuit) FuzzTest() bool {
	for i := range len(c.x) {
		for j := range len(c.y) {
			if !c.Test(1<<i, 1<<j) {
				return false
			}
		}
	}
	for i := range len(c.x) {
		y := (1 << len(c.y)) - 1
		if !c.Test(1<<i, y) {
			return false
		}
	}
	for j := range len(c.y) {
		x := (1 << len(c.x)) - 1
		if !c.Test(x, 1<<j) {
			return false
		}
	}
	return true
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	circuit := Circuit{}.Parse(input_data)
	circuit.Run()
	value, _ := circuit.z.Value()
	return value, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	circuit := Circuit{}.Parse(input_data)

	allCandidates, _ := circuit.ValidateAllBits()
	candidatePairs := []Set[[2]*Gate]{}
	seen := Set[*Gate]{}
	for i := 0; i < len(circuit.z); i++ {
		candidates, ok := allCandidates[i]
		if !ok {
			continue
		}
		candidatePairs = append(
			candidatePairs,
			circuit.SwapAndTest(candidates.Diff(seen), i, allCandidates),
		)
		seen.Union(candidates)
	}
	fix := circuit.FixErrorGates(candidatePairs)
	ret := make([]string, len(fix))
	for i, f := range fix {
		ret[i] = string(f.output.name[:])
	}
	sort.Strings(ret)
	return strings.Join(ret, ","), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

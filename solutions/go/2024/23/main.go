package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 23
* Go Solution
*
* Day 23: LAN Party
*
* https://adventofcode.com/2024/day/23
*
 */

import (
	"encoding/binary"
	"fmt"
	"sort"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Computer          uint16
	Clique2           [2]Computer
	Clique3           [3]Computer
	Network           map[Computer]Set[Computer]
)

func (s Set[T]) Clone() Set[T] {
	if s == nil {
		return nil
	}
	clone := make(Set[T], len(s))
	for item := range s {
		clone.Add(item)
	}
	return clone
}

func (s Set[T]) Add(value T) Set[T] {
	s[value] = struct{}{}
	return s
}

func (s Set[T]) Remove(value T) Set[T] {
	delete(s, value)
	return s
}

func (s Set[T]) Intersect(other Set[T]) Set[T] {
	ret := Set[T]{}
	for k := range s {
		if other.Contains(k) {
			ret.Add(k)
		}
	}
	return ret
}

func (s Set[T]) ToSlice() []T {
	ret := make([]T, len(s))
	i := 0
	for value := range s {
		ret[i] = value
		i++
	}
	return ret
}

func (s Set[T]) Contains(value T) bool {
	_, ok := s[value]
	return ok
}

func (s Set[T]) String() string {
	ret := []string{}
	for computer := range s {
		ret = append(ret, fmt.Sprint(computer))
	}
	return fmt.Sprintf("Set%v", ret)
}

func (c2 Clique2) New(a Computer, b Computer) Clique2 {
	if a > b {
		a, b = b, a
	}
	c2[0], c2[1] = a, b
	return c2
}

func (c3 Clique3) New(a Computer, b Computer, c Computer) Clique3 {
	// Keys are always ordered
	if a > b {
		a, b = b, a
	}
	if b > c {
		b, c = c, b
	}
	if a > b {
		a, b = b, a
	}
	c3[0], c3[1], c3[2] = a, b, c
	return c3
}

func NewComputer(b1, b2 byte) Computer {
	return Computer(binary.BigEndian.Uint16([]byte{b1, b2}))
}

func (c Computer) String() string {
	bytes := make([]byte, 2)
	binary.BigEndian.PutUint16(bytes, uint16(c))
	return string(bytes)
}

func (c Computer) StartsWith(value byte) bool {
	b := make([]byte, 2)
	binary.BigEndian.PutUint16(b, uint16(c))
	return b[0] == value
}

func (n Network) Parse(input_data string) Network {
	for _, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		computers := strings.Split(line, "-")
		c0 := NewComputer(computers[0][0], computers[0][1])
		c1 := NewComputer(computers[1][0], computers[1][1])
		if _, ok := n[c0]; !ok {
			n[c0] = Set[Computer]{}
		}
		if _, ok := n[c1]; !ok {
			n[c1] = Set[Computer]{}
		}
		n[c0].Add(c1)
		n[c1].Add(c0)
	}
	return n
}

func (n Network) Subset(computers Set[Computer]) Network {
	nn := Network{}
	for computer := range computers {
		nn[computer] = n[computer]
	}
	return nn
}

func (n Network) Clique2() Set[Clique2] {
	ret := Set[Clique2]{}
	for key0, value0 := range n {
		for key1 := range n {
			if key0 == key1 {
				continue
			}
			if value0.Contains(key1) {
				ret.Add(Clique2{}.New(key0, key1))
			}
		}
	}
	return ret
}

func (n Network) Clique3() Set[Clique3] {
	ret := Set[Clique3]{}
	for computer, neighbors := range n {
		clique2Set := n.Subset(neighbors).Clique2()
		for clique2 := range clique2Set {
			value := Clique3{}.New(computer, clique2[0], clique2[1])
			ret.Add(value)
		}
	}
	return ret
}

func (n Network) BronKerbosch() []Set[Computer] {
	R, P, X := Set[Computer]{}, Set[Computer]{}, Set[Computer]{}
	for k := range n {
		P.Add(k)
	}
	return n.bronKerbosch(R, P, X)
}

func (n Network) bronKerbosch(R, P, X Set[Computer]) []Set[Computer] {
	if len(P) == 0 && len(X) == 0 {
		ret := make([]Set[Computer], 1)
		ret[0] = R.Clone()
		return ret
	}

	ret := []Set[Computer]{}
	iterP := P.Clone()
	for v := range iterP {
		nR := R.Clone().Add(v)
		nP := P.Clone().Intersect(n[v])
		nX := X.Clone().Intersect(n[v])
		ret = append(ret, n.bronKerbosch(nR, nP, nX)...)
		P = P.Remove(v)
		X = X.Add(v)
	}
	return ret
}

func (n Network) MaxClique() []Computer {
	maxClique := Set[Computer]{}
	for _, clique := range n.BronKerbosch() {
		if len(clique) > len(maxClique) {
			maxClique = clique
		}
	}

	ret := maxClique.ToSlice()
	sort.Slice(ret, func(i, j int) bool {
		return ret[i] < ret[j]
	})

	return ret
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	network := Network{}.Parse(input_data)
	candidates := network.Clique3()

	acc := 0
	for candidate := range candidates {
		for _, computer := range candidate {
			if computer.StartsWith('t') {
				acc++
				break
			}
		}
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	network := Network{}.Parse(input_data)
	maxClique := network.MaxClique()
	ret := make([]string, len(maxClique))
	for i, c := range maxClique {
		ret[i] = c.String()
	}
	return strings.Join(ret, ","), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

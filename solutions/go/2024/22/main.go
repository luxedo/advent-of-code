package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 22
* Go Solution
*
* Day 22: Monkey Market
*
* https://adventofcode.com/2024/day/22
*
 */

import (
	"math"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Sequence          [4]int
	Monkey            struct {
		initial int
		current int
		price   int
		steps   int
		values  []int
		prices  []int
		deltas  []int
	}
	Market    []Monkey
	ScoresKey struct {
		monkeyId int
		sequence Sequence
	}
	ScoresMap map[ScoresKey]int
)

func (s Set[T]) Add(value T) {
	s[value] = struct{}{}
}

func (s Set[T]) Contains(value T) bool {
	_, ok := s[value]
	return ok
}

func Max(seq []int) int {
	max := math.MinInt
	for _, value := range seq {
		if value > max {
			max = value
		}
	}
	return max
}

func (m Monkey) New(initial int) Monkey {
	m.current = initial
	m.initial = initial
	m.price = initial % 10
	m.steps = 0
	m.values = []int{initial}
	m.prices = []int{m.price}
	m.deltas = []int{0}
	return m
}

func (m *Monkey) Next() {
	next := m.PseudoRandom(m.current)
	price := next % 10
	delta := price - m.price
	m.current = next
	m.price = price
	m.steps++
	m.values = append(m.values, m.current)
	m.prices = append(m.prices, m.price)
	m.deltas = append(m.deltas, delta)
}

func (Monkey) Mix(secret int, value int) int {
	// To mix a value into the secret number, calculate the bitwise XOR of the given value and the
	// secret number. Then, the secret number becomes the result of that operation. (If the secret
	// number is 42 and you were to mix 15 into the secret number, the secret number would become 37.)
	return secret ^ value
}

func (Monkey) Prune(value int) int {
	// To prune the secret number, calculate the value of the secret number modulo 16777216. Then,
	// the secret number becomes the result of that operation. (If the secret number is 100000000
	// and you were to prune the secret number, the secret number would become 16113920.)
	return value % (2 << 23) // 16777216
}

func (m *Monkey) PseudoRandom(secret int) int {
	// 1. Calculate the result of multiplying the secret number by 64. Then, mix this result into the
	// secret number. Finally, prune the secret number.
	secret = m.Prune(m.Mix(secret, secret<<6))

	// 2. Calculate the result of dividing the secret number by 32. Round the result down to the
	// nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
	secret = m.Prune(m.Mix(secret, secret>>5))

	// 3. Calculate the result of multiplying the secret number by 2048. Then, mix this result into
	// the secret number. Finally, prune the secret number.
	secret = m.Prune(m.Mix(secret, secret<<11))
	return secret
}

func (m Market) Parse(input_data string) Market {
	rows := strings.Split(strings.TrimSpace(input_data), "\n")
	m = make(Market, len(rows))
	for i, initialStr := range rows {
		initial, _ := strconv.Atoi(initialStr)
		m[i] = Monkey{}.New(initial)
	}
	return m
}

func (m *Market) Next() *Market {
	for i := range *m {
		(*m)[i].Next()
	}
	return m
}

func (m *Market) NextMany(steps int) *Market {
	for range steps {
		m.Next()
	}
	return m
}

func (m *Market) CurrentSecret() []int {
	ret := make([]int, len(*m))
	for i, monkey := range *m {
		ret[i] = monkey.current
	}
	return ret
}

func (m *Market) BuildScoresMap(lag int) ScoresMap {
	scoresMap := ScoresMap{}
	for i, monkey := range *m {
		for j := range monkey.deltas[:len(monkey.deltas)-lag+1] {
			s := Sequence(monkey.deltas[j : j+lag])
			scoresKey := ScoresKey{monkeyId: i, sequence: s}
			if _, ok := scoresMap[scoresKey]; !ok {
				scoresMap[scoresKey] = monkey.prices[j+lag-1]
			}
		}
	}
	return scoresMap
}

func (m *Market) GetMostBananas(lag int) int {
	scoresMap := m.BuildScoresMap(lag)
	candidates := Set[Sequence]{}
	for key, value := range scoresMap {
		if value == 9 {
			candidates.Add(key.sequence)
		}
	}

	maxBananas := 0
	for candidate := range candidates {
		bananas := 0
		for i := range *m {
			scoresKey := ScoresKey{monkeyId: i, sequence: candidate}
			bananas += scoresMap[scoresKey]
		}
		if bananas > maxBananas {
			maxBananas = bananas
		}
	}
	return maxBananas
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	market := Market{}.Parse(input_data)
	prices := market.NextMany(2000).CurrentSecret()

	acc := 0
	for _, price := range prices {
		acc += price
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	market := Market{}.Parse(input_data)
	market.NextMany(2000)
	lag := 4
	return market.GetMostBananas(lag), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

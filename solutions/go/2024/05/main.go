package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 05
* Go Solution
*
* Day 5: Print Queue
*
* https://adventofcode.com/2024/day/5
*
 */

import (
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(g T) {
	s[g] = struct{}{}
}

func (s Set[T]) Contains(g T) bool {
	_, exists := s[g]
	return exists
}

type Rule struct {
	page  int
	pages Set[int]
}
type Rules map[int]Rule

func (r Rules) Parse(rules_str string) Rules {
	for _, line := range strings.Split(rules_str, "\n") {
		blocks := strings.SplitN(line, "|", 2)
		before_str, after_str := blocks[0], blocks[1]
		before, _ := strconv.Atoi(before_str)
		after, _ := strconv.Atoi(after_str)

		if _, ok := r[after]; !ok {
			r[after] = Rule{page: after, pages: Set[int]{}}
		}
		r[after].pages.Add(before)
	}
	return r
}

type Update struct {
	pages []int
	valid bool
}

func (u *Update) Validate(rules *Rules) (Rule, bool) {
	for i, page := range u.pages {
		rule, ok := (*rules)[page]
		if !ok {
			continue
		}
		for _, p := range u.pages[i:] {
			if rule.pages.Contains(p) {
				return rule, false
			}
		}
	}
	u.valid = true
	var zero Rule
	return zero, true
}

func (u *Update) Fix(rules *Rules) Update {
	rule, valid := u.Validate(rules)
	if valid {
		u.valid = valid
		return *u
	}
	*u = (*u).Switch(&rule)
	return u.Fix(rules)
}

func (u *Update) Switch(rule *Rule) Update {
	idx := -1
	for i, v := range u.pages {
		if v == rule.page {
			idx = i
			break
		}
	}

	for i := len(u.pages) - 1; i >= 0; i-- {
		page := u.pages[i]
		if rule.pages.Contains(page) {
			u.pages[i], u.pages[idx] = u.pages[idx], u.pages[i]
		}
	}
	return *u
}

type Updates []Update

func (u Updates) Parse(updates_str string) Updates {
	for i, line := range strings.Split(updates_str, "\n") {
		update := Update{pages: []int{}, valid: false}
		u = append(u, update)
		for _, page := range strings.Split(line, ",") {
			p, _ := strconv.Atoi(page)
			u[i].pages = append(u[i].pages, p)
		}
	}
	return u
}

func (u *Updates) Validate(rules *Rules) {
	for i, update := range *u {
		_, valid := update.Validate(rules)
		(*u)[i].valid = valid
	}
}

type Printer struct {
	rules   Rules
	updates Updates
}

func (p Printer) Parse(input_data string) Printer {
	blocks := strings.SplitN(strings.TrimSpace(input_data), "\n\n", 2)
	rules_str, updates_str := blocks[0], blocks[1]

	p.rules = Rules{}.Parse(rules_str)
	p.updates = Updates{}.Parse(updates_str)

	p.updates.Validate(&p.rules)
	return p
}

func (p *Printer) MiddlePageNumbers() []int {
	middle := []int{}
	for _, update := range p.updates {
		if !update.valid {
			continue
		}
		middle = append(middle, update.pages[len(update.pages)/2])
	}
	return middle
}

func (p *Printer) FilterValidUpdates() Updates {
	new_updates := Updates{}
	for _, update := range p.updates {
		if update.valid {
			continue
		}
		new_updates = append(new_updates, update)
	}
	return new_updates
}

func (p *Printer) FixUpdates(updates Updates) Updates {
	new_updates := Updates{}
	for _, update := range updates {
		new_updates = append(new_updates, update.Fix(&p.rules))
	}
	return new_updates
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	printer := Printer{}.Parse(input_data)
	middle := printer.MiddlePageNumbers()

	acc := 0
	for _, m := range middle {
		acc += m
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	printer := Printer{}.Parse(input_data)
	printer.updates = printer.FixUpdates(printer.FilterValidUpdates())
	middle := printer.MiddlePageNumbers()

	acc := 0
	for _, m := range middle {
		acc += m
	}
	return acc, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

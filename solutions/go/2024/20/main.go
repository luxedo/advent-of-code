package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 20
* Go Solution
*
* Day 20: Race Condition
*
* https://adventofcode.com/2024/day/20
*
 */

import (
	"fmt"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

type (
	Set[T comparable] map[T]struct{}
	Vec               struct{ y, x int }
	Dir               struct{ y, x int }
	Cell              byte
	Track             struct {
		p        Vec
		distance int
		next     *Track
	}
	Cheat     struct{ s, d Vec }
	Racetrack struct {
		height, width int
		start         Vec
		end           Vec
		trackLength   int
		track         *Track
		tracks        map[Vec]*Track
	}
)

var (
	UP         = Dir{-1, 0}
	RIGHT      = Dir{0, 1}
	DOWN       = Dir{1, 0}
	LEFT       = Dir{0, -1}
	DIRECTIONS = []Dir{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
)

const (
	WALL  Cell = '#'
	TRACK Cell = '.'
	START Cell = 'S'
	END   Cell = 'E'
	PATH  Cell = '*'
)

func Abs(value int) int {
	if value < 0 {
		return -value
	}
	return value
}

func (s Set[T]) Add(value T) {
	s[value] = struct{}{}
}

func (s Set[T]) Remove(value T) {
	delete(s, value)
}

func (s Set[T]) Contains(value T) bool {
	_, ok := s[value]
	return ok
}

func (t Track) New(p Vec) *Track {
	t.p = p
	return &t
}

func (t *Track) Length() int {
	if t.next == nil {
		return 1
	}
	return 1 + t.next.Length()
}

func (t *Track) DistanceFrom(other *Track) int {
	return Abs(t.distance - other.distance)
}

func (t Track) String() string {
	s := fmt.Sprintf("Track(%d %v)", t.distance, t.p)
	if t.next == nil {
		return s
	}
	return s + "-> " + t.next.String()
}

func (r Racetrack) Parse(input_data string) *Racetrack {
	rows := strings.Split(strings.TrimSpace(input_data), "\n")
	r.height, r.width = len(rows), len(rows[0])
	tracks := map[Vec]*Track{}
	for y, line := range rows {
		for x, c := range line {
			p := Vec{y, x}
			switch c {
			case rune(WALL):
			case rune(TRACK):
				tracks[p] = Track{}.New(p)
			case rune(START):
				tracks[p] = Track{}.New(p)
				r.start = Vec{y, x}
			case rune(END):
				tracks[p] = Track{}.New(p)
				r.end = Vec{y, x}
			}
		}
	}

	r.track = r.LinkTrack(tracks, r.start, r.end)
	r.trackLength = r.track.Length()

	return &r
}

func (r *Racetrack) LinkTrack(tracks map[Vec]*Track, start Vec, end Vec) *Track {
	curr := start
	startTrack := tracks[start]
	visited := Set[Vec]{}

	for i, currTrack := 1, tracks[curr]; curr != end; currTrack, i = tracks[curr], i+1 {
		visited.Add(curr)
		possibleTracks := []*Track{}
		for _, d := range DIRECTIONS {
			next := Vec{curr.y + d.y, curr.x + d.x}
			if visited.Contains(next) {
				continue
			}
			if nextTrack, ok := tracks[next]; ok {
				possibleTracks = append(possibleTracks, nextTrack)
			}
		}
		if len(possibleTracks) > 1 {
			panic("Oh no! Should have a single path")
		}
		nextTrack := possibleTracks[0]
		nextTrack.distance = i
		currTrack.next = nextTrack
		curr = nextTrack.p
	}

	r.tracks = map[Vec]*Track{}
	for currTrack := startTrack; currTrack != nil; currTrack = currTrack.next {
		r.tracks[currTrack.p] = currTrack
	}

	return startTrack
}

func (r *Racetrack) CheatSaves(pico int) map[Cheat]int {
	seen := map[Cheat]int{}
	search := []Vec{}
	for y := -pico; y <= pico; y++ {
		for x := -pico - y; x <= pico-y; x++ {
			d := Abs(x) + Abs(y)
			if d != pico {
				continue
			}
			search = append(search, Vec{y, x})
		}
	}

	for v0, track0 := range r.tracks {
		for _, dv1 := range search {
			v1 := Vec{v0.y + dv1.y, v0.x + dv1.x}
			if track1, ok := r.tracks[v1]; ok {
				var cheat Cheat
				if track0.distance < track1.distance {
					cheat = Cheat{v0, v1}
				} else {
					cheat = Cheat{v1, v0}
				}

				d := track0.DistanceFrom(track1)
				seen[cheat] = d - Abs(v0.y-v1.y) - Abs(v0.x-v1.x)
			}
		}
	}
	return seen
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	racetrack := Racetrack{}.Parse(input_data)
	saves := racetrack.CheatSaves(2)

	acc := 0
	for _, s := range saves {
		if s >= 100 {
			acc++
		}
	}
	return acc, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	racetrack := Racetrack{}.Parse(input_data)

	saves := map[Cheat]int{}
	for i := 0; i <= 20; i++ {
		for k, v := range racetrack.CheatSaves(i) {
			saves[k] = v
		}
	}
	acc := 0
	for _, s := range saves {
		if s >= 100 {
			acc++
		}
	}
	return acc, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

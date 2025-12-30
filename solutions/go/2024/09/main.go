package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 09
* Go Solution
*
* Day 9: Disk Fragmenter
*
* https://adventofcode.com/2024/day/9
*
 */

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

const (
	FREE = -1
)

type (
	Block struct {
		id   int
		size int
		prev *Block
		next *Block
	}
	Disk struct {
		front  *Block
		back   *Block
		length int
		files  map[int]*Block
	}
	DMap   []int
	Blocks []int
)

func (b Block) String() string {
	if b.id == FREE {
		return fmt.Sprintf("Free(size: %d)", b.size)
	}
	return fmt.Sprintf("Block(id: %d, size: %d)", b.id, b.size)
}

func (d Disk) String() string {
	s := ""
	curr := d.front
	for curr != nil {
		var add string
		if curr.id == FREE {
			add = strings.Repeat(".", curr.size)
		} else {
			add = strings.Repeat(strconv.Itoa(curr.id), curr.size)
		}
		s += add
		curr = curr.next
	}
	return s
}

func (d Disk) ListString() string {
	s := ""
	curr := d.front
	for curr != nil {
		s += fmt.Sprintf("%v", curr)
		if curr.next != nil {
			s += " -> "
		}
		curr = curr.next
	}
	return s
}

func (d Disk) Parse(input_data string) *Disk {
	diskMapStr := strings.TrimSpace(input_data)
	dMap := make(DMap, len(diskMapStr))
	for i, c := range diskMapStr {
		v, err := strconv.Atoi(string(c))
		if err != nil {
			panic("Oh no!")
		}
		dMap[i] = v
	}
	d.Build(dMap)
	return &d
}

func (d *Disk) Build(dMap DMap) {
	d.files = make(map[int]*Block)
	for id := 0; id <= len(dMap)/2; id++ {
		idx := id * 2
		length := dMap[idx]
		block := Block{id, length, nil, nil}
		d.PushBack(&block)
		d.files[id] = &block

		var free int
		if idx+1 == len(dMap) {
			free = 0
		} else {
			free = dMap[idx+1]
		}
		if free == 0 {
			continue
		}
		freeBlock := Block{FREE, free, nil, nil}
		d.PushBack(&freeBlock)
	}
}

func (d *Disk) PushFront(block *Block) {
	d.length++
	if d.front == nil {
		d.front, d.back = block, block
		return
	}

	block.next = d.front
	d.front.prev = block
	d.front = block
}

func (d *Disk) PushBack(block *Block) {
	d.length++
	if d.back == nil {
		d.front, d.back = block, block
		return
	}

	block.prev = d.back
	d.back.next = block
	d.back = block
}

func (d *Disk) PushAt(block *Block, index int) error {
	if index < 0 || index >= d.length {
		return errors.New("cannot insert. Check your index")
	}
	if index == 0 {
		d.PushFront(block)
		return nil
	}
	if index == d.length-1 {
		d.PushBack(block)
		return nil
	}

	next := d.front
	for i := 0; i < index; i++ {
		next = next.next
	}

	block.next = next
	block.prev = next.prev
	next.prev.next = block
	next.prev = block
	d.length++

	return nil
}

func (d *Disk) PopFront() (*Block, error) {
	if d.front == nil {
		return nil, errors.New("cannot pop front. Please stop")
	}
	d.length--
	front := d.front
	if front.next == nil {
		d.front = nil
		d.back = nil
		return front, nil
	}
	d.front = front.next
	d.front.prev = nil
	return front, nil
}

func (d *Disk) PopBack() (*Block, error) {
	if d.back == nil {
		return nil, errors.New("cannot pop back. Please stop")
	}
	d.length--
	back := d.back
	if back.prev == nil {
		d.front = nil
		d.back = nil
		return back, nil
	}
	d.back = back.prev
	d.back.next = nil
	return back, nil
}

func (d *Disk) Peek(index int) (*Block, error) {
	if index < 0 || index >= d.length {
		return nil, errors.New("cannot peek. Check your index")
	}
	block := d.front
	for i := 0; i < index; i++ {
		block = block.next
	}
	return block, nil
}

func (d *Disk) CompactBlocks() {
	finalBlocks := []*Block{}
	front, _ := d.PopFront()
	// back, _ := d.PopBack()
	finalBlocks = append(finalBlocks, front)
	totalFree := 0
	for d.length > 0 {
		free, _ := d.PopFront()
		if free.id != FREE {
			finalBlocks = append(finalBlocks, free)
			continue
		}
		file, _ := d.PopBack()
		if file.id == FREE {
			totalFree += file.size
			file, _ = d.PopBack()
		}

		if free.size > file.size {
			finalBlocks = append(finalBlocks, file)
			size := free.size - file.size
			nfree := Block{id: FREE, size: size}
			d.PushFront(&nfree)
			totalFree += size
		} else {
			nfile := Block{id: file.id, size: free.size}
			finalBlocks = append(finalBlocks, &nfile)
			size := file.size - free.size
			ofile := Block{id: file.id, size: size}
			d.PushBack(&ofile)
			totalFree += free.size
		}
	}

	free := Block{id: FREE, size: totalFree}
	finalBlocks = append(finalBlocks, &free)
	for _, block := range finalBlocks {
		d.PushBack(block)
	}
}

func (d *Disk) CompactFiles() {
	for i := d.back.id; i >= 0; i-- {
		d.SwapFree(d.files[i])
	}
}

func (d *Disk) SwapFree(file *Block) {
	free := d.front
	for free != file && free != nil {
		if free.id == FREE && free.size >= file.size {
			d.Swap(file, free)
			return
		}
		free = free.next
	}
}

func (d *Disk) Swap(file *Block, free *Block) {
	d.files[file.id] = free
	if file.size == free.size {
		free.id = file.id
		file.id = FREE
		return
	}

	diff := free.size - file.size
	nFree := Block{id: FREE, size: diff, next: free.next, prev: free}
	free.id, free.size = file.id, file.size
	free.next.prev, free.next = &nFree, &nFree
	file.id = FREE
	// Missing merge FREE blocks. It worked anyway
}

func (d *Disk) Checksum() int {
	acc := 0
	i := 0
	for block, _ := d.PopFront(); block != nil; block, _ = d.PopFront() {
		var mul int
		if block.id == FREE {
			mul = 0
		} else {
			mul = block.id
		}
		for range block.size {
			acc += mul * i
			i++
		}
	}
	return acc
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	disk := Disk{}.Parse(input_data)
	disk.CompactBlocks()
	return disk.Checksum(), nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	disk := Disk{}.Parse(input_data)
	disk.CompactFiles()
	// fmt.Println(disk)
	return disk.Checksum(), nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

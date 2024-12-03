package main

/*
* ElfScript Brigade
*
* Advent Of Code 2024 Day 02
* Go Solution
*
* Day 2: Red-Nosed Reports
*
* https://adventofcode.com/2024/day/2
*
 */

import (
	"strconv"
	"strings"

	"github.com/luxedo/esb_fireplace-go"
)

func parse(input_data string) [][]int {
	ret := [][]int{}
	for i, line := range strings.Split(strings.TrimSpace(input_data), "\n") {
		ret = append(ret, []int{})
		for _, level := range strings.Fields(line) {
			l, _ := strconv.Atoi(level)
			ret[i] = append(ret[i], l)
		}
	}
	return ret
}

type Report struct {
	Report []int
	Safe   bool
}

func NewReports(input_data string) []Report {
	reports := []Report{}
	for _, report := range parse(input_data) {
		reports = append(reports, NewReport(report))
	}
	return reports
}

func NewReport(report []int) Report {
	return Report{
		Report: report,
		Safe:   isSafeReport(report),
	}
}

func isSafeReport(report []int) bool {
	if report[0] < report[1] {
		return isSafeReportMonotonincallyIncreasing(report)
	} else if report[0] > report[1] {
		return isSafeReportMonotonincallyDecreasing(report)
	} else {
		return false
	}
}

func absDiff(a int, b int) int {
	if a > b {
		return a - b
	}
	return b - a
}

func isSafeReportMonotonincallyIncreasing(report []int) bool {
	prev := report[0]
	for _, level := range report[1:] {
		diff := absDiff(prev, level)
		if level < prev || diff < 1 || diff > 3 {
			return false
		}
		prev = level
	}
	return true
}

func isSafeReportMonotonincallyDecreasing(report []int) bool {
	prev := report[0]
	for _, level := range report[1:] {
		diff := absDiff(prev, level)
		if level > prev || diff < 1 || diff > 3 {
			return false
		}
		prev = level
	}
	return true
}

func NewDampenedReports(input_data string) []Report {
	reports := []Report{}
	for i, report_input := range parse(input_data) {
		report := NewReport(report_input)
		reports = append(reports, report)
		if report.Safe {
			continue
		}

		for j := range report_input {
			test_report_input := make([]int, len(report_input)-1)
			copy(test_report_input, report_input[:j])
			copy(test_report_input[j:], report_input[j+1:])
			test_report := NewReport(test_report_input)
			if test_report.Safe {
				reports[i] = test_report
				break
			}
		}
	}
	return reports
}

func solve_pt1(input_data string, args []string) (interface{}, error) {
	reports := NewReports(input_data)
	safeCount := 0
	for _, report := range reports {
		if report.Safe {
			safeCount++
		}
	}
	return safeCount, nil
}

func solve_pt2(input_data string, args []string) (interface{}, error) {
	reports := NewDampenedReports(input_data)
	safeCount := 0
	for _, report := range reports {
		if report.Safe {
			safeCount++
		}
	}
	return safeCount, nil
}

func main() {
	// 🎅🎄❄️☃️🎁🦌
	// Bright christmas lights HERE
	esb_fireplace.V1Run(solve_pt1, solve_pt2)
}

--[[
ElfScript Brigade

Advent Of Code 2017 Day 01
Lua Solution

Day 1: Inverse Captcha

https://adventofcode.com/2017/day/1
]]

package.path = "/home/john/code/fireplace/esb_fireplace-lua/src/?.lua;" .. package.path
local fireplace = require("fireplace")

-- 🎅🎄❄️☃️🎁🦌
-- Bright Christmas lights HERE

local function solve_pt1(input_data, args)
	local s = input_data .. input_data:sub(1, 1)
	local total = 0
	for i = 1, #input_data do
		local curr = s:sub(i, i)
		local next = s:sub(i + 1, i + 1)
		if curr == next then
			total = total + tonumber(curr)
		end
	end
	return total
end

local function solve_pt2(input_data, args)
	local middle = #input_data // 2
	local total = 0
  for i = 1, middle do
    local curr = input_data:sub(i, i)
		local next = input_data:sub(middle + i, middle + i)
    if curr == next then
			total = total + 2 * tonumber(curr)
		end
  end
  return total
end

fireplace.v1_run(solve_pt1, solve_pt2)

local jets = {}
for line in io.lines("input") do
    for jet in string.gmatch(line, ".") do
        table.insert(jets, jet)
    end
end

local rocks = {
    {
        {x = 1, y = 1},
        {x = 2, y = 1},
        {x = 3, y = 1},
        {x = 4, y = 1},
    },
    {
        {x = 2, y = 1},
        {x = 1, y = 2},
        {x = 2, y = 2},
        {x = 3, y = 2},
        {x = 2, y = 3},
    },
    {
        {x = 1, y = 1},
        {x = 2, y = 1},
        {x = 3, y = 1},
        {x = 3, y = 2},
        {x = 3, y = 3},
    },
    {
        {x = 1, y = 1},
        {x = 1, y = 2},
        {x = 1, y = 3},
        {x = 1, y = 4},
    },
    {
        {x = 1, y = 1},
        {x = 2, y = 1},
        {x = 1, y = 2},
        {x = 2, y = 2},
    },
}

local function copy_rock(rock)
    local copy = {}
    for i, v in ipairs(rock) do
        copy[i] = {x = rock[i].x, y = rock[i].y}
    end
    return copy
end

local chamber = {}
local height = {0}
local next_jet = 1
local next_rock = 1
local cache = {}
local cycle_begin
local cycle_length

local function move_if_possible(rock, dx, dy)
    for _, p in ipairs(rock) do
        local x = p.x + dx
        local y = p.y + dy
        if x < 1 or x > 7 or y < 1 or chamber[x .. ";" .. y] then
            return false, rock
        end
    end
    for _, p in ipairs(rock) do
        p.x = p.x + dx
        p.y = p.y + dy
    end
    return true, rock
end

local function compute_outline()
    local outline = {}
    local visited = {}

    local function floodfill(x, y)
        if visited[x .. ";" .. y] then return end
        visited[x .. ";" .. y] = true

        if chamber[x .. ";" .. y] then
            table.insert(outline, x .. ";" .. height[#height] - y)
            return
        end

        if x > 1 then floodfill(x - 1, y) end
        if x < 7 then floodfill(x + 1, y) end
        if y > 1 then floodfill(x, y - 1) end
    end

    floodfill(1, height[#height] + 1)
    return table.concat(outline, " ")
end

for rock_number = 1, 2022 do
    if cycle_begin == nil then
        local descriptor = next_rock .. " " .. next_jet .. " " .. compute_outline()
        if cache[descriptor] then
            cycle_begin = cache[descriptor]
            cycle_length = rock_number - cycle_begin
        end
        cache[descriptor] = rock_number
    end

    local rock = copy_rock(rocks[next_rock])
    next_rock = 1 + next_rock % #rocks
    move_if_possible(rock, 2, height[#height] + 3)

    while true do
        move_if_possible(rock, jets[next_jet] == "<" and -1 or 1, 0)
        next_jet = 1 + next_jet % #jets

        if not move_if_possible(rock, 0, -1) then
            for _, p in ipairs(rock) do
                chamber[p.x .. ";" .. p.y] = "@"
            end
            height[rock_number] = math.max(rock[#rock].y, height[#height])
            break
        end
    end
end

print("Part One: " .. height[#height])

local p = (1000000000000 - cycle_begin) // cycle_length
local q = (1000000000000 - cycle_begin) % cycle_length
print("Part Two: " .. height[cycle_begin + q] + p * (height[cycle_begin + cycle_length] - height[cycle_begin]))

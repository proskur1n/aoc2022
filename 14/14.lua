local function coordinates(line)
    local iter = string.gmatch(line, "(%d+),(%d+)")
    return function ()
        local x, y = iter()
        return tonumber(x), tonumber(y)
    end
end

local function to_coords(x, y)
    return x .. " " .. y
end

local cave = {}
local maxy = 0
for line in io.lines("input") do
    local next_xy = coordinates(line)
    local prevx, prevy = next_xy()
    maxy = math.max(maxy, prevy)
    while true do
        local currx, curry = next_xy()
        if currx == nil then
            break
        end
        for x = prevx, currx, prevx < currx and 1 or -1 do
            cave[to_coords(x, curry)] = "#"
        end
        for y = prevy, curry, prevy < curry and 1 or -1 do
            cave[to_coords(currx, y)] = "#"
        end
        prevx, prevy = currx, curry
        maxy = math.max(maxy, prevy)
    end
end

local function fall_down_once(x, y)
    if cave[to_coords(x, y + 1)] == nil then
        y = y + 1
    elseif cave[to_coords(x - 1, y + 1)] == nil then
        x = x - 1
        y = y + 1
    elseif cave[to_coords(x + 1, y + 1)] == nil then
        x = x + 1
        y = y + 1
    else
        cave[to_coords(x, y)] = "o"
        return x, y, true
    end
    return x, y, false
end

local units = 0
while true do
    local x, y = 500, 0
    local rest = false
    while y < maxy and not rest do
        x, y, rest = fall_down_once(x, y)
        if rest then
            units = units + 1
        end
    end
    if y >= maxy then
        break
    end
end
print("Part One: " .. units)

local floor = maxy + 2
while cave[to_coords(500, 0)] == nil do
    local x, y = 500, 0
    local rest = false
    while not rest do
        x, y, rest = fall_down_once(x, y)
        if y + 1 == floor then
            cave[to_coords(x, y)] = "o"
            rest = true
        end
    end
    units = units + 1
end
print("Part Two: " .. units)

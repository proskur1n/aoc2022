local function compare(left, right)
    local l = left:sub(2, -2)
    local r = right:sub(2, -2)
    while l ~= "" and r ~= "" do
        if l:match("^%d") and r:match("^%d") then
            local lnum, lrest = l:match("^(%d+),?(.*)")
            local rnum, rrest = r:match("^(%d+),?(.*)")
            lnum = tonumber(lnum)
            rnum = tonumber(rnum)
            l = lrest
            r = rrest
            if lnum ~= rnum then
                return lnum < rnum and -1 or 1
            end
        else
            local llist, lrest = l:gsub("^%d+", "[%1]"):match("(%b[]),?(.*)")
            local rlist, rrest = r:gsub("^%d+", "[%1]"):match("(%b[]),?(.*)")
            l = lrest
            r = rrest
            local c = compare(llist, rlist)
            if c ~= 0 then
                return c
            end
        end
    end
    return l == "" and (r == "" and 0 or -1) or 1
end

local packets = {}
for line in io.lines("input") do
    if line ~= "" then
        table.insert(packets, line)
    end
end

local sum = 0
for i = 1, #packets, 2 do
    if compare(packets[i], packets[i + 1]) <= 0 then
        sum = sum + (i + 1) // 2
    end
end
print("Part One: " .. sum)

table.insert(packets, "[[2]]")
table.insert(packets, "[[6]]")
table.sort(packets, function (a, b) return compare(a, b) < 0 end)
local fst, snd
for i, packet in ipairs(packets) do
    if packet == "[[2]]" then
        fst = i
    elseif packet == "[[6]]" then
        snd = i
        break
    end
end
print("Part Two: " .. fst * snd)

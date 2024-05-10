local numbers = {}
for line in io.lines("input") do
    table.insert(numbers, tonumber(line))
end

local function decrypt(key, rounds)
    local list = {}
    for i, number in ipairs(numbers) do
        list[i] = {value = number}
    end
    for i in ipairs(list) do
        list[i].i = i
        list[i].next = list[1 + i % #list]
    end
    local first = list[1]
    local length = #list

    for _ = 1, rounds do
        local ptr = first
        repeat
            local shift = ptr.value * key % (length - 1)
            for i = ptr.i, ptr.i + shift - 1 do
                ptr.i = 1 + i % length
                list[ptr.i].i = 1 + (i - 1) % length
                list[list[ptr.i].i] = list[ptr.i]
            end
            list[ptr.i] = ptr
            ptr = ptr.next
        until ptr == first
    end

    local zero
    for i, number in ipairs(list) do
        if number.value == 0 then
            zero = i
            break
        end
    end
    local x = list[1 + (zero + 999) % length].value
    local y = list[1 + (zero + 1999) % length].value
    local z = list[1 + (zero + 2999) % length].value
    return (x + y + z) * key
end

print("Part One: " .. decrypt(1, 1))
print("Part Two: " .. decrypt(811589153, 10))

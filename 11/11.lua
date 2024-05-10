local monkeys = {}
local next_line = io.lines("input")
while true do
    next_line()
    local items = {}
    for item in next_line():gmatch("%d+") do
        table.insert(items, tonumber(item))
    end
    local op, operand = next_line():match("new = old (%p) (%w+)")
    if operand == "old" then
        if op == "+" then
            op = function (x) return x + x end
        elseif op == "*" then
            op = function (x) return x * x end
        else
            error("unknown operation")
        end
    else
        operand = assert(tonumber(operand))
        if op == "+" then
            op = function (x) return x + operand end
        elseif op == "*" then
            op = function (x) return x * operand end
        else
            error("unknown operation")
        end
    end
    local divisible_by = tonumber(next_line():match("%d+"))
    local if_true = tonumber(next_line():match("%d+"))
    local if_false = tonumber(next_line():match("%d+"))
    table.insert(monkeys, {
        items = items,
        op = op,
        divisible_by = divisible_by,
        if_true = if_true + 1,
        if_false = if_false + 1,
    })
    if next_line() == nil then
        break
    end
end

local function calculate_monkey_business(rounds, manage_worry_level)
    local activity = {}
    local items = {}
    for i, monkey in ipairs(monkeys) do
        activity[i] = 0
        items[i] = {}
        for _, item in ipairs(monkey.items) do
            table.insert(items[i], item)
        end
    end

    for _ = 1, rounds do
        for i, monkey in ipairs(monkeys) do
            for _, worry_level in ipairs(items[i]) do
                worry_level = monkey.op(worry_level)
                worry_level = manage_worry_level(worry_level)
                if worry_level % monkey.divisible_by == 0 then
                    table.insert(items[monkey.if_true], worry_level)
                else
                    table.insert(items[monkey.if_false], worry_level)
                end
                activity[i] = activity[i] + 1
            end
            items[i] = {}
        end
    end

    table.sort(activity)
    return activity[#activity - 1] * activity[#activity]
end

print("Part One: " .. calculate_monkey_business(20, function (w) return w // 3 end))

local modulo = 1
for _, monkey in ipairs(monkeys) do
    modulo = modulo * monkey.divisible_by
end
print("Part Two: " .. calculate_monkey_business(10000, function (w) return w % modulo end))

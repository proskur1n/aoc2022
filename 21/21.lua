local graph = {}
for line in io.lines("input") do
    local monkey, x, op, y = string.match(line, "(%a+): (%a+) ([+*/-]) (%a+)")
    if monkey ~= nil then
        graph[monkey] = {x = x, y = y, op = op}
    else
        local monkey, number = string.match(line, "(%a+): (%d+)")
        graph[monkey] = {number = tonumber(number)}
    end
end

local function eval(monkey)
    if type(monkey) == "string" then
        monkey = graph[monkey]
    end
    if monkey.number ~= nil then
        return monkey.number
    end
    if monkey.op == "+" then
        return eval(monkey.x) + eval(monkey.y)
    elseif monkey.op == "-" then
        return eval(monkey.x) - eval(monkey.y)
    elseif monkey.op == "*" then
        return eval(monkey.x) * eval(monkey.y)
    else
        return eval(monkey.x) // eval(monkey.y)
    end
end

print("Part One: " .. eval("root"))

local function mark_dependent(name)
    local monkey = graph[name]
    if name == "humn" then
        monkey.dependent = true
    elseif monkey.number == nil then
        mark_dependent(monkey.x)
        mark_dependent(monkey.y)
        monkey.dependent = graph[monkey.x].dependent or graph[monkey.y].dependent
    else
        monkey.dependent = false
    end
end

local function equality(number, exp)
    if exp.number ~= nil then
        return number
    end
    local x, y = graph[exp.x], graph[exp.y]
    if exp.op == "+" then
        if x.dependent then
            return equality(number - eval(y), x)
        else
            return equality(number - eval(x), y)
        end
    elseif exp.op == "-" then
        if x.dependent then
            return equality(number + eval(y), x)
        else
            return equality(eval(x) - number, y)
        end
    elseif exp.op == "*" then
        if x.dependent then
            return equality(number // eval(y), x)
        else
            return equality(number // eval(x), y)
        end
    else
        if x.dependent then
            return equality(number * eval(y), x)
        else
            return equality(eval(x) // number, y)
        end
    end
end

mark_dependent("root")
local part_two
if graph[graph.root.x].dependent then
    part_two = equality(eval(graph.root.y), graph[graph.root.x])
else
    part_two = equality(eval(graph.root.x), graph[graph.root.y])
end
print("Part Two: " .. part_two)

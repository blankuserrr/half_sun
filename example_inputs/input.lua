-- Lua 5.1 example: tables, functions, nested loops, metatables, closures, and error handling

-- Define a table with nested tables and a metatable
local t = {
  a = 1,
  b = 2,
  c = 3,
  nested = {x = 10, y = 20}
}

local mt = {
  __index = function(tbl, key)
    return "missing:" .. tostring(key)
  end,
  __tostring = function(tbl)
    local s = {}
    for k, v in pairs(tbl) do
      table.insert(s, k .. "=" .. tostring(v))
    end
    return "{" .. table.concat(s, ", ") .. "}"
  end
}
setmetatable(t, mt)

-- Function to sum all numeric values in a table, including nested tables
function deep_sum(tbl)
  local sum = 0
  for k, v in pairs(tbl) do
    if type(v) == "number" then
      sum = sum + v
    elseif type(v) == "table" then
      sum = sum + deep_sum(v)
    end
  end
  return sum
end

-- Function returning a closure that multiplies input by a captured factor
function make_multiplier(factor)
  return function(x)
    return x * factor
  end
end

local double = make_multiplier(2)
local triple = make_multiplier(3)

-- Demonstrate nested loops and error handling
local status, err = pcall(function()
  for i = 1, 3 do
    for j = 1, 2 do
      print(string.format("i=%d, j=%d, double(i*j)=%d, triple(i+j)=%d", i, j, double(i*j), triple(i+j)))
      if i == 2 and j == 2 then
        error("Intentional error at i=2, j=2")
      end
    end
  end
end)

if not status then
  print("Caught error:", err)
end

-- Print the sum of all values in t (including nested)
print("Deep sum:", deep_sum(t))

-- Demonstrate metatable __index and __tostring
print("t.z (missing key):", t.z)
print("t as string:", tostring(t))

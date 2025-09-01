-- Lua 5.1 example: table, function, loop, and print

-- Define a table with some values
local t = {a = 1, b = 2, c = 3}

-- Function to sum values in a table
function sum_table(tbl)
  local sum = 0
  for k, v in pairs(tbl) do
    sum = sum + v
  end
  return sum
end

-- Print the sum
print("Sum:", sum_table(t))

-- Demonstrate a simple loop
for i = 1, 5 do
  print("i:", i)
end

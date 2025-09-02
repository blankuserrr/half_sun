-- Example: Lua 5.4 features, including to-be-closed variables, labels, goto, and new for loop syntax

-- To-be-closed variable (Lua 5.4)
local function open_resource()
    local res = { closed = false }
    function res:close()
        self.closed = true
        print("Resource closed!")
    end
    return res
end

do
    local res <close> = open_resource()
    print("Using resource, closed?", res.closed)
end
-- Resource is automatically closed here

-- Numeric for loop with 'continue' (Lua 5.2+)
for i = 1, 10 do
    if i % 2 == 0 then
        goto continue
    end
    print("Odd number:", i)
    ::continue::
end

-- Generic for loop with multiple variables
local t = { a = 1, b = 2, c = 3 }
for k, v in pairs(t) do
    print("key:", k, "value:", v)
end

-- To-be-closed variable with error handling
local function may_fail()
    error("Something went wrong!")
end

local function test_tbc()
    local x <close> = { close = function(self) print("x closed") end }
    local y <close> = { close = function(self) print("y closed") end }
    print("Before error")
    may_fail()
    print("After error") -- will not be reached
end

pcall(test_tbc)

-- Labels and goto for breaking out of nested loops
for i = 1, 3 do
    for j = 1, 3 do
        if i * j == 4 then
            goto found
        end
    end
end
print("Not found")
goto after
::found::
print("Found i*j == 4")
::after::

-- Function with vararg and table.pack/unpack (Lua 5.2+)
local function collect_args(...)
    local t = table.pack(...)
    for i = 1, t.n do
        print("arg", i, t[i])
    end
    return table.unpack(t, 1, t.n)
end

collect_args("a", "b", 42, true)

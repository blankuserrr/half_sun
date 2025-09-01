-- Syntax errors and invalid constructs for parser error testing, uncomment to test

--x = 1 +     -- missing right-hand side
-- y =         -- missing expression, although this is not a syntax error
-- if then     -- missing condition
-- end

--for i = 1, 10 do
--    print(i)
-- missing end for for-loop

-- function foo(   -- missing closing parenthesis and body
-- bar = 2        -- assignment outside of function

--repeat
--    x = x - 1
-- missing until

--while x > 0    -- missing do
--    x = x - 1
-- end

-- if x > 0 then
--    print("ok")
-- elseif         -- missing condition
--    print("bad")
-- end

-- ::label        -- missing closing ::

-- goto           -- missing label

-- z = 1 + * 2    -- invalid operator usage

-- a = (1 + 2     -- missing closing parenthesis

-- b = 1 + 2))    -- extra closing parenthesis

-- c = "unterminated string

-- d = 123abc     -- invalid number

-- e = 0xGHI      -- invalid hex number

-- f = [[unclosed long string

letrec odd(x) = if (x = 0) then 0 else (even (x-1)) and even(x) = if (x = 0) then 1 else (odd (x-1))
in (odd 13)

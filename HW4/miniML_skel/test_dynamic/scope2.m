let x = 1
in let f = proc (y) (x+y)
in let x = 2
in let g = proc (y) (x+y)
in (f 1) + (g 1)

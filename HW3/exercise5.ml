let rec iter n f x= 
  if n > 1 then (iter (n - 1) f (f x))
  else f;;
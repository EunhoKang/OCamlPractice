let f = let counter = ref(0)
        in proc (x) ((counter := !(counter)+1); !(counter)) in
let a = (f 0) in
let b = (f 0) in
  (a-b)

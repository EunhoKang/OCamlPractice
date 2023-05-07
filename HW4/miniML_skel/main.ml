let main () =
    let print_code = ref false in
		let static = ref false in
		let dynamic = ref false in
    let src = ref "" in
    let spec = 
			[("-pp", Arg.Set print_code, "pretty print the input program");
			 ("-static", Arg.Set static, "run the input program with static scoping");
			 ("-dynamic", Arg.Set dynamic, "run the input program with dynamic scoping")
			] 
		in
    let usage = "Usage: run <options> <file>" in
    let _ = Arg.parse spec
                (fun
                   x ->
                     if Sys.file_exists x then src := x
                     else raise (Arg.Bad (x ^ ": No files given")))
                usage
    in
    
	if !src = "" then Arg.usage spec usage
  else
  	let file_channel = open_in !src in
  	let lexbuf = Lexing.from_channel file_channel in
  	let pgm = Parser.program Lexer.start lexbuf in
		if !print_code then print_endline (Lang.string_of_exp pgm)
		else
      try
      	if !static then 
        	print_endline (Interpreter_static.value2str (Interpreter_static.run pgm))
      	else if !dynamic then 
      		print_endline (Interpreter_dynamic.value2str (Interpreter_dynamic.run pgm))
				else 
					Arg.usage spec usage
      with Lexer.LexicalError -> print_endline (!src ^ ": Lexical Error")

let _ = main ()

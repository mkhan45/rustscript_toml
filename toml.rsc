let is_ident_char_fn() = {
    let ident_chars = to_charlist("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
    fn(ch) => any([i == ch for i in ident_chars])
}

let is_digit_char_fn() = {
    let digit_chars = to_charlist("0123456789")
    fn(ch) => any([d == ch for d in digit_chars])
}

let skip_newlines(ls) = match ls
    | ["\n" | xs] -> skip_newlines(xs)
    | _ -> ls

let scan_number(ls) = {
    let is_digit_char = is_digit_char_fn()

    let loop = fn(ls, acc) => match ls
	| [d | xs] when is_digit_char(d) -> loop(xs, [d | acc])
	| _ -> (reverse(acc), ls)

    let (num_chars, remaining) = loop(ls, [])
    let (:ok, num) = num_chars |> concat |> string_to_int
    ((:number, num), remaining)
}

let scan_ident(ls) = {
    let is_ident_char = is_ident_char_fn()

    let loop = fn(ls, acc) => match ls
	| [i | xs] when is_ident_char(i) -> loop(xs, [i | acc])
	| _ -> (reverse(acc), ls)

    let (ident_chars, remaining) = loop(ls, [])
    let ident = concat(ident_chars)
    ((:ident, ident), remaining)
}

let scan_string(ls) = {
    let loop(ls, acc) = match ls
	| ["\"" | xs] -> (reverse(acc), xs)
	| [ch | xs] -> loop(xs, [ch | acc])
	| [] -> {
	    println("Error while scanning string: unmatched quote")
	    let () = 1 # crashes the program
	}

    let (string, remaining) = loop(ls, [])
    let string = concat(string)
    ((:string, string), remaining)
}

let scan_multiline_string(ls) = {
    let loop(ls, acc) = match ls
	| ["\"", "\"", "\"" | xs] -> (reverse(acc), xs)
	| [ch | xs] -> loop(xs, [ch | acc])
	| [] -> {
	    println("Error while scanning string: unmatched quote")
	    let () = 1 # crashes the program
	}

    let (string, remaining) = loop(ls, [])
    let string = concat(string)
    ((:string, string), remaining)
}

let skip_until_newline(ls) = match ls
    | [] -> []
    | ["\n" | xs] -> xs
    | [_ | xs] -> skip_until_newline(xs)

let scan(ls) = {
    let is_ident_char = is_ident_char_fn()
    let is_digit_char = is_digit_char_fn()

    let loop = fn(ls, acc) => match ls
	| [] ->
	    reverse(acc)

	| [" " | xs] | ["\t" | xs] | ["\n" | xs]->
	    loop(xs, acc)

	| ["[" | xs] ->
	    loop(xs, [:lbracket | acc])

	| ["]" | xs] ->
	    loop(xs, [:rbracket | acc])

	| ["," | xs] ->
	    loop(xs, [:comma | acc])

	| ["=" | xs] ->
	    loop(xs, [:equal | acc])

	| ["#" | xs] -> {
	    let xs = skip_until_newline(xs)
	    loop(xs, acc)
	}

	| ["\"", "\"", "\"" | xs] -> {
	    let (string, remaining) = scan_multiline_string(xs)
	    loop(remaining, [string | acc])
	}

	| ["\"" | xs] -> {
	    let (string, remaining) = scan_string(xs)
	    loop(remaining, [string | acc])
	}

	| [n | _] when is_digit_char(n) -> {
	    let (number, remaining) = scan_number(ls)
	    loop(remaining, [number | acc])
	}

	| [ch | _] when is_ident_char(ch) -> {
	    let (ident, remaining) = scan_ident(ls)
	    loop(remaining, [ident | acc])
	}

    loop(ls, [])
}

let parse_arr(ls) = {
    let loop(ls, acc) = {
	let ls = skip_newlines(ls)
	match(ls)
	    | [:rbracket | rest] -> (reverse(acc), rest)
	    | [] -> {
		println("Error parsing list: unmatched lbracket")
		let () = 1
	    }
	    | [:comma | xs] -> {
		let ls = skip_newlines(xs)
		match xs
		    | [:rbracket | rest] -> (reverse(acc), rest)
		    | _ -> {
			let (el, xs) = parse_expr(xs)
			loop(xs, [el | acc])
		    }
	    }
	    | _ -> {
		let (el, xs) = parse_expr(ls)
		loop(xs, [el | acc])
	    }
    }

    let (els, remaining) = loop(ls, [])
    ((:array, els), remaining)
}

let parse_expr(ls) = match skip_newlines(ls)
    | [(:string, s) | xs] ->
	((:string, s), xs)

    | [(:number, n) | xs] ->
	((:number, n), xs)

    | [:lbracket | xs] ->
	parse_arr(xs)

let parse_stmt(ls) = match ls
    | [(:ident, name), :equal | xs] -> {
	let (expr, remaining) = parse_expr(xs)
	((:assignment, name, expr), remaining)
    }
    | [:lbracket, :lbracket, (:ident, name), :rbracket, :rbracket | xs] -> {
	let (expr_ls, remaining) = parse_arr_entry(xs)
	((:arr_entry, name, expr_ls), remaining)
    }

let parse_arr_entry(ls) = {
    let loop(ls, acc) = {
	let ls = skip_newlines(ls)
	match ls
	    | [] -> (reverse(acc), [])
	    | [:lbracket, :lbracket | _] ->  (reverse(acc), ls)
	    | _ -> {
		let (stmt, remaining) = parse_stmt(ls)
		loop(remaining, [stmt | acc])
	    }
    }

    let (stmts, remaining) = loop(ls, [])
    (stmts, remaining)
}

let parse_stmts(ls) = {
    let loop(ls, acc) = match ls
	| [] -> reverse(acc)
	| _ -> {
	    let (stmt, remaining) = parse_stmt(ls)
	    loop(remaining, [stmt | acc])
	}

    loop(ls, [])
}

let eval_toml_expr(expr) = match expr
    | (:number, n) -> n
    | (:string, s) -> s
    | (:array, els) -> map(eval_toml_expr, els)

let toml_to_map(toml, acc) = match toml
    | [] -> acc
    | [(:assignment, name, expr) | xs] -> {
	let evaled = eval_toml_expr(expr)
	toml_to_map(xs, %{name => evaled | acc})
    }
    | [(:arr_entry, name, ls) | xs] -> {
	match acc(name)
	    | () -> {
		let entry = toml_to_map(ls, %{})
		toml_to_map(xs, %{name => [entry] | acc})
	    }
	    | entries -> {
		let entry = toml_to_map(ls, %{})
		toml_to_map(xs, %{name => entries + [entry] | acc})
	    }
    }

let parse_toml(s) = s
    |> to_charlist
    |> scan
    |> parse_stmts
    |> toml_to_map(_, %{})

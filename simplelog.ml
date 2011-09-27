let print_help () =
    print_endline "Usage:";
    print_endline "\tsimplelog file [regex1 regex2 ...]"

let main () =
    let rec get_regexes = function
        | 0 | 1 -> []
        | num -> (Str.regexp Sys.argv.(num))::(get_regexes (num-1))
    in
    let file = open_out Sys.argv.(1) in
    at_exit (fun () -> close_out file);
    let regs = get_regexes ((Array.length Sys.argv) - 1) in
    while true do
        try
            let line = read_line () in
            (match regs with
            | [] -> output_string file (line ^ "\n")
            | _ -> List.iter (fun x -> if Str.string_match x line 0 then output_string file (line ^ "\n")) regs);
            print_endline line
        with _ -> exit 0
    done

let _ =
    if Array.length Sys.argv < 2 then print_help()
    else
        match Sys.argv.(1) with
        | "-h" | "--help" -> print_help ()
        | _ -> main ()

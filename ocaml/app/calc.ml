type tokens =
  | Number of int
  | Keyword of char

type context =
  { tokens : tokens list
  ; str : char list
  }

let parse str =
  let list_is_empty = function
    | [] -> true
    | __ -> false
  in let context_parse_digit ctx ch =
    { tokens = ctx.tokens
    ; str = ctx.str @ [ ch ]
    }
  in let context_parser_token ctx ch =
    { tokens = ctx.tokens @ [ Keyword ch ]
    ; str = ctx.str
    }
  in let context_flush ctx =
    { tokens =
      if list_is_empty ctx.str
      then ctx.tokens
      else
      (
        let base = 10
        in let int_value ch = Char.code ch - Char.code '0'
        in let append_char value ch = (value * base) + (int_value ch)
        in let num = List.fold_left append_char 0 ctx.str
        in let token = Number num
        in ctx.tokens @ [ token ]
      )
    ; str = []
    }
  in let parser_dispatch ctx ch = match ch with
    | '0' -> context_parse_digit ctx ch
    | '1' -> context_parse_digit ctx ch
    | '2' -> context_parse_digit ctx ch
    | '3' -> context_parse_digit ctx ch
    | '4' -> context_parse_digit ctx ch
    | '5' -> context_parse_digit ctx ch
    | '6' -> context_parse_digit ctx ch
    | '7' -> context_parse_digit ctx ch
    | '8' -> context_parse_digit ctx ch
    | '9' -> context_parse_digit ctx ch
    | '+' -> context_parser_token ( context_flush ctx ) ch
    | '-' -> context_parser_token ( context_flush ctx ) ch
    | '*' -> context_parser_token ( context_flush ctx ) ch
    | '/' -> context_parser_token ( context_flush ctx ) ch
    | '(' -> context_parser_token ( context_flush ctx ) ch
    | ')' -> context_parser_token ( context_flush ctx ) ch
    |  _  -> context_flush ctx
  in let initial_context =
    { tokens = []
    ; str = []
    }
  in let characters =
    let len = String.length str
    in let get = String.get str
    in let rec range i =
      if i < len
      then i :: (range (i + 1))
      else []
    in let indexes = range 0
    in List.map get indexes
  in let almost_final_context = List.fold_left parser_dispatch initial_context characters
  in let final_context = context_flush almost_final_context
  in final_context.tokens
;;

let main () =
  print_endline "Hello world!";
  parse " 111 + 2 *( 33    - 444444) /2"
;;

let _ = main ()
;;

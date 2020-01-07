{

type i18n_expr =
  | Arg of string
  | Str of string
  | Elision of string * string

let flush buffer acc =
  let acc = match Buffer.contents buffer with
    | "" -> acc
    | x -> Str x :: acc in
  Buffer.clear buffer ;
  acc

let need_split = function
  | "!languages"
  | "(date)"
  | "(french revolution month)"
  | "(hebrew month)"
  | "(month)"
  | "(week day)"
  | "a 2nd cousin"
  | "a 3rd cousin"
  | "a cousin"
  | "a descendant"
  | "alive"
  | "an ancestor"
  | "and"
  | "a %s cousin"
  | "baptized"
  | "born"
  | "buried"
  | "cremated"
  | "died"
  | "died young"
  | "disappeared"
  | "engaged%t to"
  | "executed (legally killed)"
  | "grand-parents"
  | "great-grand-parents"
  | "inversion done"
  | "killed (in action)"
  | "married%t to"
  | "murdered"
  | "next sibling"
  | "nth"
  | "nth (cousin)"
  | "nth (generation)"
  | "previous sibling"
  | "relationship%t to"
  | "the spouse"
  | "would be his/her own ancestor"
  | "died at an advanced age"
  | "half siblings"
  | "(short month)"
    -> true
  | "is born after his/her child"
  | "loop in database: %s is his/her own ancestor"
  | "marriage of %t after his/her death"
  | "marriage of %t before his/her birth"
  | "%t was witness after his/her death"
  | "%t was witness before his/her birth"
  | "%t's %s before his/her %s"
  | "%t witnessed the %s after his/her death"
  | "%t witnessed the %s before his/her birth"
    -> false
  | t -> String.contains t '/'

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let num = ['0'-'9']

let id = (lower | ['_']) (lower | upper | num | ['_'])*

let line = [^ '\n' ]+
let eol = '\n'

rule p_main acc = parse
  | ' '+ (line as t) eol
      { p_main ((t, p_lang (need_split t) [] lexbuf) :: acc) lexbuf }
  | _
      { p_main acc lexbuf }
  | eof { acc }

and p_lang split acc = parse
  | ((lower | '-' )+ as lang) ':' ' '? (line as trad) eol {
      let trad =
        if split then Array.of_list @@ String.split_on_char '/' trad
        else [| trad |]
      in
      let trad =
        Array.map (fun t -> p_trad (Buffer.create 42) [] @@ Lexing.from_string t) trad
      in
      p_lang split ((lang, trad) :: acc) lexbuf
    }
  | "" { acc }

and p_trad buffer acc = parse
  | '%' (num+ as n) {
      let acc = flush buffer acc in
      p_trad buffer (Arg ("_" ^ n) :: acc) lexbuf
    }
  | '%' (lower+ as n) {
      let acc = flush buffer acc in
      p_trad buffer (Arg n :: acc) lexbuf
    }
  | '[' ([^'|']* as s1) '|' ([^']']* as s2) ']' {
      let acc = flush buffer acc in
      p_trad buffer (Elision (s1, s2) :: acc) lexbuf
    }
  | _ as c {
      Buffer.add_char buffer c ;
      p_trad buffer acc lexbuf
    }
  | eof {
      let occ c acc =
        List.fold_left (fun sum -> function Arg x when x.[0] = c -> sum + 1 | _ -> sum) 1 acc
      in
      let rec loop acc = function
        | Arg hd :: tl ->
          let c = hd.[0] in
          if c <> '_' then
            let occ = occ c tl in
            if occ = 1
            && not (List.exists (function Arg s -> s.[0] = c | _ -> false) acc)
            then loop (Arg hd :: acc) tl
            else loop (Arg (hd ^ string_of_int occ) :: acc) tl
          else loop (Arg hd :: acc) tl
        | hd :: tl -> loop (hd :: acc) tl
        | [] -> acc
      in
      loop [] (flush buffer acc)
      |> Array.of_list
    }

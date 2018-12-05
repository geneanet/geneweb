(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let iso_8859_1_unknown = '\129'
let ansel_unknown = 129

let no_accent =
  function
    '\224'..'\229' -> 'a'
  | '\162' | '\231' -> 'c'
  | '\232'..'\235' -> 'e'
  | '\236'..'\239' -> 'i'
  | '\241' -> 'n'
  | '\242'..'\246' -> 'o'
  | '\249'..'\252' -> 'u'
  | '\253' | '\255' -> 'y'
  | '\192'..'\197' -> 'A'
  | '\199' -> 'C'
  | '\200'..'\203' -> 'E'
  | '\204'..'\207' -> 'I'
  | '\209' -> 'N'
  | '\210'..'\214' -> 'O'
  | '\217'..'\220' -> 'U'
  | '\221' -> 'Y'
  | '\168' | '\176' | '\180' | '\184' | '\186' -> ' '
  | '\171' -> '<'
  | '\187' -> '>'
  | c -> c

let accent_code =
  (* and 1-to-1 conversions *)
  function
    '\192' | '\200' | '\204' | '\210' | '\217' | '\224' | '\232' | '\236' |
    '\242' | '\249' ->
      225
  | '\193' | '\201' | '\205' | '\211' | '\218' | '\221' | '\180' | '\225' |
    '\233' | '\237' | '\243' | '\250' | '\253' ->
      226
  | '\194' | '\202' | '\206' | '\212' | '\219' | '\226' | '\234' | '\238' |
    '\244' | '\251' ->
      227
  | '\195' | '\209' | '\213' | '\227' | '\241' | '\245' -> 228
  | '\196' | '\203' | '\207' | '\214' | '\220' | '\168' | '\228' | '\235' |
    '\239' | '\246' | '\252' | '\255' ->
      232
  | '\197' | '\229' | '\176' | '\186' -> 234
  | '\199' | '\231' | '\184' -> 240
  | '\161' -> 198
  | '\162' -> 252
  | '\163' -> 185
  | '\164' -> 0x6f
  | '\165' -> 0x59
  | '\166' -> 0x7c
  | '\169' -> 195
  | '\170' -> 0x61
  | '\171' -> 0x3c
  | '\173' -> 0x2d
  | '\174' -> 170
  | '\177' -> 171
  | '\178' -> 0x32
  | '\179' -> 0x33
  | '\183' -> 168
  | '\185' -> 0x31
  | '\187' -> 0x3e
  | '\191' -> 197
  | '\198' -> 165
  | '\230' -> 181
  | '\208' -> 163
  | '\240' -> 179
  | '\216' -> 162
  | '\248' -> 178
  | '\222' -> 164
  | '\254' -> 180
  | '\223' -> 207
  | c when c >= '\161' -> ansel_unknown
  | _ -> 0

let of_iso_8859_1 s =
  let (len, identical) =
    let rec loop i len identical =
      if i = String.length s then len, identical
      else
        let a = accent_code s.[i] in
        if a = 0 then loop (i + 1) (len + 1) identical
        else
          let n = no_accent s.[i] in
          if n = s.[i] then loop (i + 1) (len + 1) false
          else loop (i + 1) (len + 2) false
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else
        let i' =
          let a = accent_code s.[i] in
          if a > 0 then
            begin
              Bytes.set s' i' (Char.chr a);
              let n = no_accent s.[i] in
              if n = s.[i] then i'
              else begin Bytes.set s' (i' + 1) n; i' + 1 end
            end
          else begin Bytes.set s' i' s.[i]; i' end
        in
        loop (i + 1) (i' + 1)
    in
    loop 0 0

let grave =
  function
    'a' -> '\224'
  | 'e' -> '\232'
  | 'i' -> '\236'
  | 'o' -> '\242'
  | 'u' -> '\249'
  | 'A' -> '\192'
  | 'E' -> '\200'
  | 'I' -> '\204'
  | 'O' -> '\210'
  | 'U' -> '\217'
  | ' ' -> '`'
  | x -> x

let acute =
  function
    'a' -> '\225'
  | 'e' -> '\233'
  | 'i' -> '\237'
  | 'o' -> '\243'
  | 'u' -> '\250'
  | 'y' -> '\253'
  | 'A' -> '\193'
  | 'E' -> '\201'
  | 'I' -> '\205'
  | 'O' -> '\211'
  | 'U' -> '\218'
  | 'Y' -> '\221'
  | ' ' -> '\180'
  | x -> x

let circum =
  function
    'a' -> '\226'
  | 'e' -> '\234'
  | 'i' -> '\238'
  | 'o' -> '\244'
  | 'u' -> '\251'
  | 'A' -> '\194'
  | 'E' -> '\202'
  | 'I' -> '\206'
  | 'O' -> '\212'
  | 'U' -> '\219'
  | ' ' -> '^'
  | x -> x

let uml =
  function
    'a' -> '\228'
  | 'e' -> '\235'
  | 'i' -> '\239'
  | 'o' -> '\246'
  | 'u' -> '\252'
  | 'y' -> '\255'
  | 'A' -> '\196'
  | 'E' -> '\203'
  | 'I' -> '\207'
  | 'O' -> '\214'
  | 'U' -> '\220'
  | ' ' -> '\168'
  | x -> x

let circle =
  function
    'a' -> '\229'
  | 'A' -> '\197'
  | ' ' -> '\176'
  | x -> x

let tilde =
  function
    'a' -> '\227'
  | 'n' -> '\241'
  | 'o' -> '\245'
  | 'A' -> '\195'
  | 'N' -> '\209'
  | 'O' -> '\213'
  | ' ' -> '~'
  | x -> x

let cedil =
  function
    'c' -> '\231'
  | 'C' -> '\199'
  | ' ' -> '\184'
  | x -> x

let slash =
  function
    'C' | 'c' -> '\162'
  | 'O' -> '\216'
  | 'o' -> '\248'
  | ' ' -> '/'
  | x -> x

let to_iso_8859_1 s =
  let (len, identical) =
    let rec loop i len identical =
      if i = String.length s then len, identical
      else if i = String.length s - 1 then len + 1, identical
      else
        match Char.code s.[i] with
          166 | 172 | 173 | 182 | 188 | 189 -> loop (i + 1) (len + 2) false
        | c when c >= 224 -> loop (i + 2) (len + 1) false
        | c when c >= 161 -> loop (i + 1) (len + 1) false
        | _ -> loop (i + 1) (len + 1) identical
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else if i = String.length s - 1 then
        begin Bytes.set s' i' s.[i] ; Bytes.unsafe_to_string s' end
      else
        match Char.code s.[i] with
          166 | 172 | 173 | 182 | 188 | 189 as c ->
            let (c', c'') =
              match c with
                166 -> 'O', 'E'
              | 172 -> 'O', '\180'
              | 173 -> 'U', '\180'
              | 182 -> 'o', 'e'
              | 188 -> 'o', '\180'
              | 189 -> 'u', '\180'
              | _ -> iso_8859_1_unknown, iso_8859_1_unknown
            in
            Bytes.set s' i' c';
            Bytes.set s' (i' + 1) c'';
            loop (i + 1) (i' + 2)
        | c when c >= 224 ->
            let c' = s.[i+1] in
            let c' =
              match c with
                224 | 226 | 235 | 237 | 254 -> acute c'
              | 225 | 236 -> grave c'
              | 227 | 250 -> circum c'
              | 228 | 230 | 233 -> tilde c'
              | 232 | 238 -> uml c'
              | 231 | 234 -> circle c'
              | 240 | 241 | 242 | 243 | 244 | 247 | 248 | 249 -> cedil c'
              | 252 -> slash c'
              | _ -> c'
            in
            Bytes.set s' i' c'; loop (i + 2) (i' + 1)
        | c ->
            let c' =
              match c with
                161 -> 'L'
              | 162 -> '\216'
              | 163 -> '\208'
              | 164 -> '\222'
              | 165 -> '\198'
              | 167 | 174 | 176 -> '\180'
              | 168 -> '\183'
              | 169 -> 'b'
              | 170 -> '\174'
              | 171 -> '\177'
              | 177 | 193 -> 'l'
              | 178 -> '\248'
              | 179 | 186 -> '\240'
              | 180 -> '\254'
              | 181 -> '\230'
              | 183 -> '"'
              | 184 -> 'i'
              | 185 -> '\163'
              | 190 | 191 -> iso_8859_1_unknown
              | 192 -> '\176'
              | 194 -> 'P'
              | 195 -> '\169'
              | 196 -> '#'
              | 197 -> '\191'
              | 198 -> '\161'
              | 205 -> '\101'
              | 206 -> '\111'
              | 207 -> '\223'
              | _ -> Char.chr c
            in
            Bytes.set s' i' c'; loop (i + 1) (i' + 1)
    in
    loop 0 0

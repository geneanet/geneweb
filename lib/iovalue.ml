(* $Id: iovalue.ml,v 5.15 2012-01-27 16:27:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* Input:
   read inside a value output by output_value (no headers) must
   match OCaml's input_value system (intern.c) *)

let sizeof_long = 4
let sign_extend_shift = (Sys.word_size / 8 - 1) * 8 - 1
let sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift

let prefix_SMALL_BLOCK = 0x80
let prefix_SMALL_INT = 0x40
let prefix_SMALL_STRING = 0x20
let code_INT8 = 0x0
let code_INT16 = 0x1
let code_INT32 = 0x2
let code_INT64 = 0x3
let code_BLOCK32 = 0x8
let code_BLOCK64 = 0x13
let code_STRING8 = 0x9
let code_STRING32 = 0xA

type 'a in_funs =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> bytes -> int -> int -> unit }

let input_binary_int64 ifuns ic =
  let rec loop cnt n =
    if cnt = 0 then n else loop (cnt - 1) (n lsl 8 + ifuns.input_byte ic)
  in
  loop 8 0

let rec input_loop ifuns ic =
  let code = ifuns.input_byte ic in
  if code >= prefix_SMALL_INT then
    if code >= prefix_SMALL_BLOCK then
      input_block ifuns ic (code land 0xf) (code lsr 4 land 0x7)
    else Obj.magic (code land 0x3f)
  else if code >= prefix_SMALL_STRING then
    let len = code land 0x1F in
    let s = Bytes.create len in ifuns.input ic s 0 len; Obj.magic s
  else if code = code_INT8 then Obj.magic (sign_extend (ifuns.input_byte ic))
  else if code = code_INT16 then
    let h = ifuns.input_byte ic in
    Obj.magic (sign_extend h lsl 8 + ifuns.input_byte ic)
  else if code = code_INT32 then
    let x1 = ifuns.input_byte ic in
    let x2 = ifuns.input_byte ic in
    let x3 = ifuns.input_byte ic in
    let x4 = ifuns.input_byte ic in
    Obj.magic (sign_extend x1 lsl 24 + x2 lsl 16 + x3 lsl 8 + x4)
  else if code = code_INT64 then
    let () = assert (Sys.word_size = 64) in
    Obj.magic (input_binary_int64 ifuns ic)
  else if code = code_BLOCK32 then
    let header = ifuns.input_binary_int ic in
    Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
  else if code = code_BLOCK64 then
    if Sys.word_size = 64 then
      let header = input_binary_int64 ifuns ic in
      Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
    else failwith "input bad code block 64"
  else if code = code_STRING8 then
    let len = ifuns.input_byte ic in
    let s = Bytes.create len in ifuns.input ic s 0 len; Obj.magic s
  else if code = code_STRING32 then
    let len = ifuns.input_binary_int ic in
    let s = Bytes.create len in ifuns.input ic s 0 len; Obj.magic s
  else failwith (Printf.sprintf "input bad code 0x%x" code)
and input_block ifuns ic tag size =
  let v =
    if tag = 0 then Obj.magic (Array.make size (Obj.magic 0))
    else Obj.new_block tag size
  in
  for i = 0 to size - 1 do
    let x = input_loop ifuns ic in Obj.set_field v i (Obj.magic x)
  done;
  v

let in_channel_funs =
  {input_byte = input_byte; input_binary_int = input_binary_int;
   input = really_input}

let input ic = Obj.magic (input_loop in_channel_funs ic)
let gen_input ifuns i = Obj.magic (input_loop ifuns i)

(* Output *)

type 'a out_funs =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }

let size_32 = ref 0
let size_64 = ref 0

let output_binary_int64 ofuns oc x =
  for i = 1 to 8 do
    ofuns.output_byte oc (x lsr (64 - 8 * i) land 0xFF)
  done

let gen_output_block_header ofuns oc tag size =
  let hd = size lsl 10 + tag in
  if tag < 16 && size < 8 then
    ofuns.output_byte oc (prefix_SMALL_BLOCK + tag + size lsl 4)
  else if Sys.word_size = 64 && hd >= 1 lsl 32 then
    begin
      ofuns.output_byte oc code_BLOCK64;
      output_binary_int64 ofuns oc hd
    end
  else
    begin
      ofuns.output_byte oc code_BLOCK32;
      (* hd = size << 10 + tag *)
      ofuns.output_byte oc (size lsr 14 land 0xFF);
      ofuns.output_byte oc (size lsr 6 land 0xFF);
      ofuns.output_byte oc (size lsl 2 land 0xFF);
      ofuns.output_byte oc (size lsl 10 land 0xFF + tag)
    end;
  if size = 0 then ()
  else
    begin size_32 := !size_32 + 1 + size; size_64 := !size_64 + 1 + size end

let rec output_loop ofuns oc x =
  if Obj.is_int x then
    if Obj.magic x >= 0 && Obj.magic x < 0x40 then
      ofuns.output_byte oc (prefix_SMALL_INT + Obj.magic x)
    else if Obj.magic x >= -128 && Obj.magic x < 128 then
      begin
        ofuns.output_byte oc code_INT8;
        ofuns.output_byte oc (Obj.magic x)
      end
    else if Obj.magic x >= -32768 && Obj.magic x <= 32767 then
      begin
        ofuns.output_byte oc code_INT16;
        ofuns.output_byte oc (Obj.magic x lsr 8);
        ofuns.output_byte oc (Obj.magic x)
      end
    else if Obj.magic x >= -1073741824 && Obj.magic x <= 1073741823 then
      begin
        ofuns.output_byte oc code_INT32;
        ofuns.output_binary_int oc (Obj.magic x)
      end
    else
      begin
        ofuns.output_byte oc code_INT64;
        output_binary_int64 ofuns oc (Obj.magic x)
      end
  else if Obj.tag x = Obj.string_tag then
    let len = String.length (Obj.magic x) in
    if len < 0x20 then ofuns.output_byte oc (prefix_SMALL_STRING + len)
    else if len < 0x100 then
      begin ofuns.output_byte oc code_STRING8; ofuns.output_byte oc len end
    else
      begin
        ofuns.output_byte oc code_STRING32;
        ofuns.output_binary_int oc len
      end;
    ofuns.output oc (Obj.magic x) 0 len;
    size_32 := !size_32 + 1 + (len + 4) / 4;
    size_64 := !size_64 + 1 + (len + 8) / 8
  else if Obj.tag x = Obj.double_tag || Obj.tag x = Obj.double_array_tag then
    failwith "Iovalue.output: floats not implemented"
  else if Obj.tag x = Obj.closure_tag then failwith "Iovalue.output <fun>"
  else if Obj.tag x = Obj.abstract_tag then
    failwith "Iovalue.output <abstract>"
  else if Obj.tag x = Obj.infix_tag then failwith "Iovalue.output: <infix>"
  else if Obj.tag x = Obj.custom_tag then failwith "Iovalue.output: <custom>"
  else if Obj.tag x = Obj.out_of_heap_tag then
    failwith "Iovalue.output: abstract value (outside heap)"
  else
    begin
      gen_output_block_header ofuns oc (Obj.tag x) (Obj.size x);
      (* last case of "for" separated, to make more tail recursive cases
         when last field is itself, to prevent some stacks overflows *)
      if Obj.size x > 0 then
        begin
          for i = 0 to Obj.size x - 2 do
            output_loop ofuns oc (Obj.field x i)
          done;
          output_loop ofuns oc (Obj.field x (Obj.size x - 1))
        end
    end

let out_channel_funs =
  {output_byte = output_byte; output_binary_int = output_binary_int;
   output = output_substring}

let output oc x = output_loop out_channel_funs oc (Obj.repr x)
let gen_output ofuns i x = output_loop ofuns i (Obj.repr x)
let output_block_header = gen_output_block_header out_channel_funs

(* Size *)

let size_funs =
  {output_byte = (fun r _ -> incr r);
   output_binary_int = (fun r _ -> r := !r + 4);
   output = fun r _ beg len -> r := !r + len - beg}

let size = ref 0

let size v = size := 0; gen_output size_funs size v; !size

(* Digest *)

let dbuf = ref (Bytes.create 256)
let dlen = ref 0
let dput_char c =
  if !dlen = Bytes.length !dbuf then dbuf := Bytes.extend !dbuf 0 !dlen;
  Bytes.set !dbuf !dlen c;
  incr dlen
let rec dput_int i =
  if i = 0 then ()
  else
    begin
      dput_char (Char.chr (Char.code '0' + i mod 10));
      dput_int (i / 10)
    end
let dput_string s = for i = 0 to String.length s - 1 do dput_char s.[i] done

let rec digest_loop v =
  if not (Obj.is_block v) then
    let n : int = Obj.magic v in dput_char 'I'; dput_int n
  else if Obj.tag v = Obj.closure_tag then
    invalid_arg "Iovalue.digest: closure"
  else if Obj.size v = 0 then begin dput_char 'T'; dput_int (Obj.tag v) end
  else if Obj.tag v = Obj.string_tag then
    let s : string = Obj.magic v in
    dput_char 'S'; dput_int (String.length s); dput_char '/'; dput_string s
  else
    begin
      dput_char 'O';
      dput_int (Obj.tag v);
      dput_char '/';
      dput_int (Obj.size v);
      digest_fields v 0
    end
and digest_fields v i =
  if i = Obj.size v then ()
  else begin digest_loop (Obj.field v i); digest_fields v (i + 1) end

let digest v =
  dlen := 0;
  digest_loop (Obj.repr v);
  Digest.to_hex (Digest.subbytes !dbuf 0 !dlen)

let output_value_header_size = 20
let array_header_size arr_len =
  if arr_len < 8 then 1
  else if Sys.word_size = 64 && arr_len lsl 10 >= 1 lsl 32 then 9
  else 5

let output_array_access oc arr_get arr_len pos =
  let rec loop pos i =
    if i = arr_len then pos
    else
      begin
        output_binary_int oc pos;
        loop (pos + size (arr_get i)) (i + 1)
      end
  in
  loop (pos + output_value_header_size + array_header_size arr_len) 0

(* *)

type header_pos = int * int

let intext_magic_number = [| 0x84; 0x95; 0xA6; 0xBE |]

let create_output_value_header oc =
  (* magic number *)
  for i = 0 to 3 do output_byte oc intext_magic_number.(i) done;
  let pos_header = pos_out oc in
  (* room for block length *)
  output_binary_int oc 0;
  (* room for obj counter *)
  output_binary_int oc 0;
  (* room for size_32 *)
  output_binary_int oc 0;
  (* room for size_64 *)
  output_binary_int oc 0;
  size_32 := 0;
  size_64 := 0;
  pos_header, pos_out oc

let patch_output_value_header oc (pos_header, pos_start) =
  let pos_end = pos_out oc in
  if Sys.word_size = 64 &&
     (pos_end >= 1 lsl 32 || !size_32 >= 1 lsl 32 || !size_64 >= 1 lsl 32)
  then
    failwith "Iovalue.output: object too big";
  (* block_length *)
  seek_out oc pos_header;
  output_binary_int oc (pos_end - pos_start);
  (* obj counter is zero because no_sharing *)
  output_binary_int oc 0;
  (* size_32 *)
  output_binary_int oc !size_32;
  (* size_64 *)
  output_binary_int oc !size_64;
  pos_end

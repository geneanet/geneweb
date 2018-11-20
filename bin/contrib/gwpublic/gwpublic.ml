open Geneweb.Def

let list_ind = ref ""
let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-everybody", Arg.Set everybody,
   "set flag public to everybody [slow option]";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-list-ind", Arg.String (fun s -> list_ind := s),
   "<file> file to the list of persons"]
let anonfun i = bname := i
let usage = "Usage: public [-everybody] [-ind key] [-list-ind file] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  if !everybody then Gwaccess.access_everybody Public !bname
  else if !list_ind = "" then Gwaccess.access_some Public !bname !ind
  else Gwaccess.access_some_list Public !bname !list_ind

let _ = main ()

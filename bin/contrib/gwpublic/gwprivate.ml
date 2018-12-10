open Geneweb

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
let usage = "Usage: private [-everybody] [-ind key] [-list-ind file] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Mutil.lock_file !bname)
    ~onerror:Lock.print_error_and_exit
    (fun () ->
       if !everybody then Gwaccess.access_everybody Def.Private !bname
       else if !list_ind = "" then
         Gwaccess.access_some Def.Private !bname !ind
       else Gwaccess.access_some_list Def.Private !bname !list_ind)

let _ = main ()

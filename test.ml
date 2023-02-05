(* 
ocamlfind ocamlopt -o test_fifo.exe -thread fifo.mli fifo.ml test.ml 
 *)
open Effect
open Effect.Deep
open Cancel_unified_interface

let m = MVar.create_empty ()

module F = Fifo.Make ()

let main () = 
  let comp () = 
     F.run (fun () ->
        let fiber1 = F.fork ( fun () ->
            Printf.printf "\nFiber 1 \n%!";
            let x = MVar.take m in
            Printf.printf "\nBack to Fiber 1 %d\n%!" x;
        ) in
        let fiber2 = F.fork (fun _ ->
          Printf.printf "\nFiber 2 \n%!";
          F.cancel fiber1;
          Printf.printf "\nBack to Fiber 2 \n%!";
        ) in
        let fiber3 = F.fork (fun _ ->
          Printf.printf "\nFiber 3 \n%!";
          (* MVar.put m 20; *)
          Printf.printf "\nBack to Fiber 3 \n%!";
        ) in
        ()

     )
      in
  match_with comp ()
  { retc = (fun () -> ());
    exnc = (function
      | Exit ->  Printf.printf "\nReached at Exit exception";()
      | e -> Printf.printf "\nReaching here in exception"; Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | e -> None
  }

let _ = main ()
    
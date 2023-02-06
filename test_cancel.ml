(* 
ocamlfind ocamlopt -o test_fifo.exe -thread fifo.mli fifo.ml test.ml 
 *)
 open Printf
open Effect
open Effect.Deep
open Cancel_unified_interface

let m = MVar.create_empty ()

module F = Fifo.Make ()

let main () = 

  let fifo_domain = Domain.spawn( fun () ->
     F.run (fun () ->
        let domain_no = (Domain.self () :> int) in
        let fiber1 = F.fork ( fun () ->
            Printf.printf "\nDomain Number: %d -> Fiber 1 \n%!" domain_no;
            Printf.printf "\nDomain Number: %d -> Back to Fiber 1 \n%!" domain_no;
        ) in
        let fiber2 = F.fork ( fun () ->
            Printf.printf "\nDomain Number: %d -> Fiber 2 \n%!" domain_no;
            Printf.printf "\nDomain Number: %d -> Back to Fiber 2 \n%!" domain_no;
        ) in
         let fiber3 = F.fork ( fun () ->
            Unix.sleep 2;
            Printf.printf "\nDomain Number: %d -> Fiber 3 putting value in MVar \n%!" domain_no;
            MVar.put m 20;
            Printf.printf "\nDomain Number: %d -> Back to Fiber 3 \n%!" domain_no;
        )
        in ()
     )
  ) in
  let comp () = 
     F.run (fun () ->
        let domain_no = (Domain.self () :> int) in
        let fiber1 = F.fork ( fun () ->
            Printf.printf "\nDomain Number: %d -> Fiber 1 taking value from mvar\n%!" domain_no;
            let x = MVar.take m in
            Printf.printf "\nDomain Number: %d -> Back to Fiber 1 with value taken as %d\n%!" domain_no x;
        ) in
        let fiber2 = F.fork (fun _ ->
          Printf.printf "\nDomain Number: %d -> Fiber 2 cancelling the fiber 1 \n%!" domain_no;
          F.cancel fiber1;
          Printf.printf "\nDomain Number: %d -> Back to Fiber 2 after cancelling fiber 1\n%!" domain_no;
        ) in
        ()
     ) in 
     comp ();
    let _ = Domain.join fifo_domain in 
    printf "\nBoth the domains are done completed%!"

let _ = main ()
    
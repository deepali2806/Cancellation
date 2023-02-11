(* 
ocamlfind ocamlopt -o test_fifo.exe -thread fifo.mli fifo.ml test.ml 
 *)
open Printf
open Effect
open Effect.Deep
open Cancel_unified_interface

exception Cancel


module S = Eio.Stream

let t = S.create 1 
(* let m = MVar.create_empty () *)

module F = Fifo.Make ()

let main () = 

  (* let eio_domain = Domain.spawn( fun () -> *)
     Eio_main.run @@ fun _env ->
      Eio.Switch.run (fun sw ->
        (* printf "\nEio: Running in domain %d%!" (Domain.self () :> int); *)
        try 
        (* Eio.Fiber.fork ~sw *)
        Eio.Fiber.both
        (fun () -> 
          printf "\nEio : Fiber 1 taking value from Mvar%!";
          let v = S.take t in
          (* let v = MVar.take m in *)
          printf "\nEio : Value taken %d%!" v
        )
        (* Eio.Fiber.fork ~sw *)
        (fun () -> 
          printf "\nEio : Inside Eio Fiber 2%!";
          raise Cancel
        );
        (* Eio.Fiber.fork ~sw
        (fun () -> 
          printf "\nEio : Inside Eio Fiber 3%!";
          (* raise Cancel *)
        ); *)
        (* assert false; *)
         with
        | Cancel -> printf "\nException occured in Eio due to cancellation%!"
      )
    (* ) in 
  let comp () = 
     F.run (fun () ->
        let domain_no = (Domain.self () :> int) in
        let fiber1 = F.fork ( fun () ->
            Unix.sleep 2;
            Printf.printf "\nFifo -> Fiber 1 putting value in the mvar\n%!" ;
            (* let _ = try  *)
                      MVar.put m 20 ;
                      (* with
                    | _ -> printf "Some error occured during putting"
            in *)
            Printf.printf "\nFifo -> Back to Fiber 1 \n%!";
        ) in
        let fiber2 = F.fork (fun _ ->
          Printf.printf "\nFifo-> Fiber 2 \n%!" ;
          (* F.cancel fiber1; *)
          Printf.printf "\nFifo -> Back to Fiber 2 \n%!" ;
        ) in
        ()
     ) in 
     comp ();
    let _ = Domain.join eio_domain in 
    printf "\nBoth the domains are done completed%!" *)

let _ = main ()
    
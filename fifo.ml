open Printf
open Effect
open Effect.Deep
(* open Unified_interface *)

let counter = Atomic.make 0
let create_ID () = Atomic.fetch_and_add counter 1


module type S = sig
  type state = Running | Cancelled | Terminated

(* Referring Affect library *)
  type fiber = E : t -> fiber
  and t = { fiber : fiber;
                tid : int ; 
                mutable state : state ;
                }

  val fork : (unit -> unit) -> t
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
end

module Make () : S = struct

  
 type state = Running | Cancelled | Terminated

(* Referring Affect library *)

  type fiber = E : t -> fiber
  and t = { fiber : fiber;
                tid : int ; 
                mutable state : state ;
                }
  type _ Effect.t += Fork  : (fiber * (unit -> unit)) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  type 'a record_check = {k:('a, unit) continuation; fiber: fiber}

  let make_fiber ?(state = Running) () =  
    let rec f = {fiber = E f; tid = create_ID (); state} in f

  let run main =
    let run_q = Queue.create () in
    let enqueue (t: 'a record_check) v =
      Queue.push  (t, v) run_q
    in
    let dequeue () =
        if (not (Queue.is_empty run_q)) then
          let (t, v) = Queue.pop run_q in
          continue t.k v
        else
          printf "Everything is done: Just addd logic for Suspend now"
    in
    let rec spawn ~new_fiber:fiber f =
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Yield -> Some (fun (k: (a,_) continuation) ->
              let t = { k; fiber } in 
              enqueue (Obj.magic t) (); 
              dequeue ())
          | Fork (new_fiber, f) -> Some (fun (k: (a,_) continuation) ->
              let t = { k; fiber } in
              enqueue (Obj.magic t) (); 
              spawn ~new_fiber f
              )
          (* | Sched.Suspend f -> Some (fun (k: (a,_) continuation) ->
              let resumer v = enqueue k v in
              f resumer; dequeue ())
           *)
          | _ -> None }
    in
    let nf =(make_fiber ()) in
    let new_fiber = nf.fiber in
    spawn ~new_fiber main

  let fork f = 
    let new_fiber = make_fiber () in 
    perform (Fork (new_fiber.fiber, f));
    new_fiber
  
  let yield () = perform Yield

end
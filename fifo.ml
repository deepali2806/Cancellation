open Printf
open Effect
open Effect.Deep
open Cancel_unified_interface

exception Abort

let counter = Atomic.make 0
let create_ID () = Atomic.fetch_and_add counter 1


let suspend_count = Atomic.make 0
let m = Mutex.create ()
let cv = Condition.create ()

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
  val cancel : t -> unit
end

module Make () : S = struct

  
 type state = Running | Cancelled | Terminated

(* Referring Affect library *)
  type fiber = E : t -> fiber
  and t = { fiber : fiber;
                tid : int ; 
                mutable state : state ;
                }
  type _ Effect.t += Fork  : (t * (unit -> unit)) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  type 'a record_check = {k:('a, unit) continuation; fiber: t}

  let make_fiber ?(state = Running) () =  
    let rec f = {fiber = E f; tid = create_ID (); state} in f

  let run main =
    let run_q = Queue.create () in
    let enqueue (t: 'a record_check) v =
      Queue.push  (t, v) run_q
    in
    let rec dequeue () =
    (* Check the value of v in case of exception *)
        if (not (Queue.is_empty run_q)) then
          let (t, v) = Queue.pop run_q in
          (if t.fiber.state = Running then
            continue t.k v
          else
            begin
              try discontinue t.k Abort with
              | Abort -> printf "\nSome fiber %d got aborted\n%!" t.fiber.tid;dequeue ()
            end
            )
        else
        begin
          printf "\nEverything is done; Just suspended tasks are yet to come \n%!";
          Mutex.lock m;
          while (Atomic.get suspend_count) <> 0 do
            Condition.wait cv m
          done;
          Mutex.unlock m;
        end
    in
    let rec spawn ~new_fiber:fiber f =
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Sched.Suspend f -> Some (fun (k: (a,_) continuation) ->
              Atomic.incr suspend_count;
              let t = { k; fiber } in
              (* If cancelled then increase the counter again so the scheduler wont wait *)
              let resumer v = 
                if fiber.state = Running then
                  begin
                  (match v with
                    | Ok x -> begin
                              Atomic.decr suspend_count;
                              (if(Atomic.get suspend_count = 0) then
                                Condition.signal cv);
                              Printf.printf "\nResumer is being executed and enqueued\n%!";
                              enqueue (Obj.magic t) x;
                              end
                    | Error ex -> enqueue (Obj.magic t) ex
                  );
                  Sched.Resume_success
                  end
                else
                  begin
                  printf "\n fiber %d got aborted\n%!" fiber.tid;
                  Atomic.decr suspend_count;
                  (if(Atomic.get suspend_count = 0) then
                      Condition.signal cv);
                  Sched.Resume_failure
                  end
              in
              if f (Obj.magic resumer) then
                dequeue ()
              else 
                discontinue k Exit
            )
          | Yield -> Some (fun (k: (a,_) continuation) ->
              let t = { k; fiber } in 
              enqueue (Obj.magic t) (); 
              dequeue ())
          | Fork (new_fiber, f) -> Some (fun (k: (a,_) continuation) ->
              let t = { k; fiber } in
              enqueue (Obj.magic t) (); 
              spawn ~new_fiber f
              )
          | _ -> None }
    in
    let new_fiber = (make_fiber ()) in
    spawn ~new_fiber main

  let fork f = 
    let new_fiber = make_fiber () in 
    perform (Fork (new_fiber, f));
    new_fiber
  
  let yield () = perform Yield


  let cancel fiber = 
    match fiber.state with
    | Running -> fiber.state <- Cancelled
    | Cancelled | Terminated -> Printf.printf "Already cancelled/done no!"
end
(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2018
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)

let has_cycle (lst : 'a mlist) : bool =
  let rec aux l v =
   print_string "aux";
   match l with
   | Nil -> false
   | Cons(h, { contents = t }) ->
    if List.memq l v then
      true
    else
      aux t (l::v)
  in
  aux lst []

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)
let flatten (lst : 'a mlist) : unit =
  let rec aux l v =
     match l with
     | Nil -> ()
     | Cons(h, t) ->
     if List.memq l v then
       let _ = t := Nil in
       ()
     else
       aux !t (l::v)
  in
  aux lst []
(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  let rec aux l v i =
     match l with
     | Nil -> i
     | Cons(h, t) ->
     if List.memq l v then
       (i+1)
     else
       aux !t (l::v) (i+1)
  in
  aux lst [] 0
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 70 ;;

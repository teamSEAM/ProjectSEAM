(* Stack Implementation
 * From slide 68 of class notes:
 * http://www.cs.columbia.edu/~sedwards/classes/2015/4115-fall/ocaml.pdf
 *)
type 'a t =
  { mutable c : 'a list }

exception Empty

let create () = { c = [] }
let clear s = s.c <- []
let copy s = { c = s.c }
let push x s = s.c <- x :: s.c
let pop s =
  match s.c with
    hd::tl -> s.c <- tl; hd
  | []     -> raise Empty
let top s =
  match s.c with
    hd::_ -> hd
  | []    -> raise Empty
let is_empty s = (s.c = [])
let length s = List.length s.c
let iter f s = List.iter f s.c

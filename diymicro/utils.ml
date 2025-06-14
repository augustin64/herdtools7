(** Print something if verbose *)
let verbose_print string = if !Config.verbose then output_string stderr string

(** Removes element from the option monad, raises Not_found if appropriate *)
let unsome = function None -> raise Not_found | Some v -> v

let rec list_last = function
  | _ :: x :: q -> list_last (x :: q)
  | [x] -> x
  | _ -> Warn.fatal "Can't get last element of empty list"

(** Rotates list by one step: `l@[x]` becomes `x::l` *)
let list_rot1 l =
  let rec rotate1 acc = function
    | [] -> []
    | [e] -> e :: List.rev acc
    | e :: l -> rotate1 (e :: acc) l
  in
  rotate1 [] l

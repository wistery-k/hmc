type t = 
  | Id of int
  | T of string * t list
  | App of int * t array

let rec to_string = function
  | Id id -> "_"
  | T (label, []) -> label
  | T (label, children) -> "(" ^ label ^ " " ^ (String.concat " " (List.map to_string children)) ^ ")"
  | App (fn, args) -> "_"

type sort =
  | O
  | F of sort * sort

let rec substitute args = function
  | Id id -> args.(id)
  | T (label, children) -> 
    T (label, List.map (substitute args) children)
  | App (fn, trees) -> 
    App (fn, Array.map (substitute args) trees)

let rec expand r n tree =
  if n = 0 then 
    tree
  else
    match tree with
      | Id id -> assert false
      | T (label, children) ->
        T (label, List.map (expand r n) children)
      | App (fn, trees) -> 
        expand r (n-1) (substitute trees r.(fn))

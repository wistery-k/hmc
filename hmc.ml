open Batteries_uni

type tree = 
  | Id of int
  | T of string * tree list
  | App of int * tree array

let rec string_of_tree = function
  | Id id -> "_"
  | T (label, []) -> label
  | T (label, children) -> "(" ^ label ^ " " ^ (String.join " " (List.map string_of_tree children)) ^ ")"
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

type atype = P of int | A of itype * atype
and  itype = I of atype list

let rec string_of_atype = function
  | P(i) -> "q" ^ (string_of_int i)
  | A(x, y) -> "(" ^ (string_of_itype x) ^ " -> " ^ (string_of_atype y) ^ ")"

and string_of_itype = function
  | I [] -> "T"
  | I lst -> String.join " /\\ " (List.map string_of_atype lst)

let string_of_env env =
  String.join ", " (List.map (fun (i, atyp) -> "F" ^ (string_of_int i) ^ " : " ^ (string_of_atype atyp)) env)

let rec powerset = function
  | [] -> [[]]
  | x :: xs -> 
    let yss = (powerset xs) in
    yss @ List.map (fun ys -> x :: ys) yss

let iota n = List.init n identity

let rec get_args = function
  | A (x, xs) -> x :: (get_args xs)
  | typ -> []

let rec get_ret = function
  | A (x, xs) -> get_ret xs
  | P(i) -> i

module type Model = sig
  val size : int
  val n : sort array
  val r : tree array
  val q_size : int
  val d : (int * string * int list) list
end

module Checker = functor
    (M : Model) -> struct

      let rec valid' env (tree, q) arglst = (* envとarglstの下で, treeがqという型を持つかどうか *)
        match tree with
          | Id id -> 
            let I(typs) = arglst.(id) in 
            List.mem (P(q)) typs
          | T (label, children) -> 
            List.exists (fun (s, l, crt) -> s = q && l = label && List.for_all2 (fun c ct -> valid' env (c, ct) arglst) children crt) M.d
          | App (fn, trees) -> 
            flip List.exists env
              (fun (i, atyp) -> i = fn && get_ret atyp = q && List.for_all2 (fun tree (I(lst)) -> List.for_all (function P(q) -> valid' env (tree, q) arglst | A(_) -> false) lst) (Array.to_list trees) (get_args atyp))

      let valid env (fn, atyp) =  (* envの下で, fnがatypという型を持つかどうか *)
        valid' env (M.r.(fn), get_ret atyp) (Array.of_list (get_args atyp))

      let rec reduce env =
        let env' = List.filter (valid env) env in
        if List.length env = List.length env' then
          env
        else
          reduce env'

      let rec atypes = function
        | O -> List.init M.q_size (fun i -> P(i))
        | F (x, y) -> List.map (fun (x, y) -> A(x, y)) (List.cartesian_product (itypes x) (atypes y))

      and itypes sort = 
        List.map (fun set -> I(set)) (powerset (atypes sort))

      let check () =
        let g_max = List.flatten (List.map2 (fun i -> List.map (fun typ -> (i, typ))) (iota M.size) (List.map atypes (Array.to_list M.n)))
        in
        let g_min = reduce g_max in
        Format.eprintf "%s@." (string_of_env g_max);
        Format.eprintf "%s@." (string_of_env g_min);
        List.mem (0, P(0)) g_min

    end

module Example = struct

  (* helper functions *)
  let a x y = T ("a", [x; y])
  let b x   = T ("b", [x])
  let c     = T ("c", [])
    
  let size = 2
    
  let n = [|
    O;
    F(O, O)
  |]
    
  let r = [|
    (App(1, [|c|]));
    (a (Id(0)) (App(1, [|(b (Id(0)))|])))
  |]

  let q_size = 2

  let d = [
    (0, "a", [0; 0]);
    (0, "b", [1]);
    (1, "b", [1]);
    (0, "c", []);
    (1, "c", [])
  ]

end

module ExampleChecker = Checker(Example)

let () =
  print_endline (if ExampleChecker.check () then "safe!" else "unsafe!")





















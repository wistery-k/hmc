open Batteries_uni

open Mystd
open Tree

type atype = P of int | A of itype * atype
and  itype = I of atype list

let rec string_of_atype = function
  | P(i) -> "q" ^ (string_of_int i)
  | A(x, y) -> "(" ^ (string_of_itype x) ^ " -> " ^ (string_of_atype y) ^ ")"

and string_of_itype = function
  | I [] -> "T"
  | I lst -> String.concat " /\\ " (List.map string_of_atype lst)

let string_of_env env =
  String.concat ", " (List.map (fun (i, atyp) -> "F" ^ (string_of_int i) ^ " : " ^ (string_of_atype atyp)) env)

let rec get_args = function
  | A (x, xs) -> x :: (get_args xs)
  | typ -> []

let rec get_ret = function
  | A (x, xs) -> get_ret xs
  | P(i) -> i

module type Model = sig
  val size : int
  val n : sort array
  val r : Tree.t array
  val q_size : int
  val d : (int * string * int list) list
end

module Checker = functor
    (M : Model) -> struct

      let rec valid' env (tree, q) arglst = (* envとarglstの下で, treeがP(q)という型を持つかどうか *)
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
        List.map (fun set -> I(set)) (List.powerset (atypes sort))

      let check () =
        let g_max = List.flatten (List.map2 (fun i -> List.map (fun typ -> (i, typ))) (List.iota M.size) (List.map atypes (Array.to_list M.n)))
        in
        let g_last = reduce g_max in
        Format.eprintf "g_max:@.%s@." (string_of_env g_max);
        Format.eprintf "g_last:@.%s@." (string_of_env g_last);
        List.mem (0, P(0)) g_last

    end

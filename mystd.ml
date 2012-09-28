open Batteries_uni

module List = struct

  include List
    
  let rec powerset = function
    | [] -> [[]]
    | x :: xs -> 
      let yss = (powerset xs) in
      yss @ List.map (fun ys -> x :: ys) yss
        
  let iota n = List.init n (fun x -> x)

end











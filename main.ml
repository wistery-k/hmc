open Modelcheck

module Example = struct

  open Tree

  (* helper functions *)
  let a x y = T ("a", [x; y])
  let b x   = T ("b", [x])
  let c     = T ("c", [])
    
  let size = 2
    
  let n = [|
    O;
    F(O, O)
  |]
  
  
  (* safe! *)
  let r = [|
    (App(1, [|c|]));
    (a (Id(0)) (App(1, [|(b (Id(0)))|])))
  |]
  

  (*
  (* unsafe! *)
  let r = [|
    (App(1, [|c|]));
    (a (Id(0)) (b (App(1, [|Id(0)|]))))
  |]
  *)

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
  Format.printf "expand 7 times:@.%s@." (Tree.to_string (Tree.expand Example.r 7 Example.r.(0)));
  print_endline (if ExampleChecker.check () then "safe!" else "unsafe!")





















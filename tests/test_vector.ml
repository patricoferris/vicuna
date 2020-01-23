open Vicuna.Vector 
open Alcotest

let vector_testable = testable pp_vec Vicuna.Vector.equal

let v1 = vector 1. 2. 3. 1. 
let v2 = vector 4. 5. 6. 1. 

let dot () = 
  Alcotest.(check (float 0.001)) "Dot product:" 33. (dot v1 v2)

let add () =
  Alcotest.(check (vector_testable)) "Vector addition:" (vector 5. 7. 9. 1.) (add v1 v2)


let tests  = [
  Alcotest.test_case "Dot product" `Quick dot;
  Alcotest.test_case "Vector addition" `Quick add;
]


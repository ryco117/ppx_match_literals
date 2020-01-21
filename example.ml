let () = print_endline ( string_of_int (
  let str = "hello" in
  let hello = "hello" in
  let hi = "hi" in
  match%literals str with
  | "hey" -> 1
  | [%lit hi] -> 2
  | [%lit hello] when 1 = 2 -> 3
  | [%lit hello] when 1 = 1 -> 4
  | _ -> 5
))

let () = print_endline ( string_of_int (
  let hello = "hello" in
  let hi = "hi" in
  let one = 1 in
  match%literals (1, ("hello", "hi")) with
  | (1, ("a", "b")) -> 1
  | (1, ([%lit hi], [%lit hello])) -> 2
  | (1, ([%lit hello], "b")) -> 3
  | (1, ([%lit hello], [%lit hi])) when false -> 4
  | ([%lit one], ([%lit hello], [%lit hi])) when true && 1 = 2 -> 5
  | ([%lit one], ([%lit hello], [%lit hi])) when true && 1 = 1 -> 6
  | _ -> 7
))


let () = print_endline ( string_of_int (
  let hello_hi = ("hello", "hi") in
  let hi = "hi" in
  let one = 1 in
  match%literals (1, ("hello", "hi")) with
  | ([%lit one], [%lit hello_hi]) as thing when 1 = 2 -> (match thing with (x, _) -> x)
  | ([%lit one], [%lit hello_hi]) when true && 1 = 2 -> 2
  | (1, ("a", [%lit hi])) | (1, ("hello", [%lit hi]))-> 3
  | _ -> 4
))


let () = print_endline ( string_of_int (
  let one = 1 in
  let three = 3 in
  let four = 4 in
  let hello = "hello" in
  match%literals ([1;2;3;4], "hello", ("hello", 1)) with
  | ([%lit one]::_::[%lit three]::[%lit four]::[], "hello", ([%lit hello], 1)) when false && 1 = 1 -> 1 
  | ([%lit one]::_::[%lit three]::[%lit four]::[], "hello", ([%lit hello], 1)) when true && 1 = 1 -> 2 
  | _ -> 3
))

type alpha_eg = {
  x: string;
  y: (int * int) option;
}

type beta_g = {
  z: alpha_eg list;
  w: string;
}

let () = print_endline ( string_of_int (
  let _one = 1 in
  let _two = 2 in
  let _three = 3 in 
  let hello = "hello" in
  let hi = "hi" in
  let value = {z = [{x = "hello"; y = None}; {x = "hey"; y = Some (1,2)}; {x = ""; y = None}]; w = "hi"} in
  match%literals value with
  | {z = {x = [%lit hello]; y = None}::{x = "hey"; y = Some ([%lit _one], 2)}::_; w = [%lit hi]} when false -> 1
  | {z = {x = [%lit hello]; y = None}::{y = Some ([%lit _one], 2); _}::_; w = [%lit hi]} when true -> 2
  | _ -> 3
))

(*let () = print_endline ( string_of_int (
  let hello = "hello" in
  match [| "hello" ; "hi" |] with 
  | [| [%lit hello] ; "hi" |] -> 1
  | _ -> 2
  ))*)

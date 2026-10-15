type t = Red | Yellow
let to_string = function
  | Red -> "Red (X)"
  | Yellow -> "Yellow (O)"

let first = Red
let other = function
  | Red -> Yellow
  | Yellow -> Red

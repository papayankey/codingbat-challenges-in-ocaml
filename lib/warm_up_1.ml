let sleep_in ~weekday ~vacation = match weekday, vacation with
| (false, _) | (_, true) -> true
| _ -> false

let%test _ = sleep_in ~weekday:false ~vacation:false = true
let%test _ = sleep_in ~weekday:true ~vacation:false = false
let%test _ = sleep_in ~weekday:false ~vacation:true = true


let monkey_trouble ~aSmile ~bSmile = match aSmile, bSmile with
| (false, false) | (true, true) -> true
| _ -> false 

let%test _ = monkey_trouble ~aSmile:true ~bSmile:true = true
let%test _ = monkey_trouble ~aSmile:false ~bSmile:false = true
let%test _ = monkey_trouble ~aSmile:true ~bSmile:false = false

let sum_double a b = match a, b with
| (x, y) when x = y -> 2 * (x + y)
| (x, y) -> x + y

let%test _ = sum_double 1 2 = 3
let%test _ = sum_double 3 2 = 5
let%test _ = sum_double 2 2 = 8

let diff_21 n = if n > 21 then 2*abs(21-n) else 21-n

let%test _ = diff_21 19 = 2
let%test _ = diff_21 10 = 11
let%test _ = diff_21 21 = 0

let parrot_trouble ~talking ~hour = match talking, hour with
| (true, h) when h < 7 || h > 20 -> true
| _ -> false

let%test _ = parrot_trouble ~talking:true ~hour:6 = true
let%test _ = parrot_trouble ~talking:true ~hour:7 = false
let%test _ = parrot_trouble ~talking:false ~hour:6 = false

let makes_10 a b = match a, b with
| (10, _) | (_, 10) -> true
| (x, y) when x + y = 10 -> true
| _ -> false

let%test _ = makes_10 9 10 = true
let%test _ = makes_10 9 9 = false
let%test _ = makes_10 1 9 = true

let near_hundred n = abs(100-n) <= 10 || abs(200-n) <= 10

let%test _ = near_hundred 93 = true
let%test _ = near_hundred 90 = true
let%test _ = near_hundred 89 = false

let pos_neg a b c = match a, b, c with 
| (x, y, true) when x < 0 && y < 0 -> true
| (x, y, false) when x < 0 || y < 0 -> true
| _ -> false

let%test _ = pos_neg 1 (-1) false = true
let%test _ = pos_neg (-1) 1 false = true
let%test _ = pos_neg (-4) (-5) true = true
let%test _ = pos_neg (-4) 5 true = false

let not_string s = 
  if String.starts_with ~prefix:"not" s then s 
  else Printf.sprintf "not %s" s

let%test _ = not_string "candy" = "not candy"
let%test _ = not_string "not x" = "not x"
let%test _ = not_string "not bad" = "not bad"

let missing_char s i =  
  let rec loop acc n l = match l with
  | [] -> acc
  | h :: t -> loop (if n = 0 then acc else h :: acc) (n-1) t
  in
  loop [] i (List.of_seq (String.to_seq s))
  |> List.rev
  |> List.to_seq
  |> String.of_seq

let%test _ = missing_char "kitten" 1 = "ktten"
let%test _ = missing_char "kitten" 0 = "itten"
let%test _ = missing_char "kitten" 4 = "kittn"
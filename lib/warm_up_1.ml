open Printf

(* 1. sleepIn *)
let sleep_in ~weekday ~vacation =
  match weekday, vacation with
  | false, _ | _, true -> true
  | _ -> false
;;

let%test _ = sleep_in ~weekday:false ~vacation:false = true
let%test _ = sleep_in ~weekday:true ~vacation:false = false
let%test _ = sleep_in ~weekday:false ~vacation:true = true

(* 2. monkeyTrouble *)
let monkey_trouble ~aSmile ~bSmile =
  match aSmile, bSmile with
  | false, false | true, true -> true
  | _ -> false
;;

let%test _ = monkey_trouble ~aSmile:true ~bSmile:true = true
let%test _ = monkey_trouble ~aSmile:false ~bSmile:false = true
let%test _ = monkey_trouble ~aSmile:true ~bSmile:false = false

(* 3. sumDouble *)
let sum_double a b =
  match a, b with
  | x, y when x = y -> 2 * (x + y)
  | x, y -> x + y
;;

let%test _ = sum_double 1 2 = 3
let%test _ = sum_double 3 2 = 5
let%test _ = sum_double 2 2 = 8

(* 4. diff21 *)
let diff_21 n = if n > 21 then 2 * abs (21 - n) else 21 - n
let%test _ = diff_21 19 = 2
let%test _ = diff_21 10 = 11
let%test _ = diff_21 21 = 0

(* 5. parrotTrouble *)
let parrot_trouble ~talking ~hour =
  match talking, hour with
  | true, h when h < 7 || h > 20 -> true
  | _ -> false
;;

let%test _ = parrot_trouble ~talking:true ~hour:6 = true
let%test _ = parrot_trouble ~talking:true ~hour:7 = false
let%test _ = parrot_trouble ~talking:false ~hour:6 = false

(* 6. makes10 *)
let makes_10 a b =
  match a, b with
  | 10, _ | _, 10 -> true
  | x, y when x + y = 10 -> true
  | _ -> false
;;

let%test _ = makes_10 9 10 = true
let%test _ = makes_10 9 9 = false
let%test _ = makes_10 1 9 = true

(* 7. nearHundred *)
let near_hundred n = abs (100 - n) <= 10 || abs (200 - n) <= 10
let%test _ = near_hundred 93 = true
let%test _ = near_hundred 90 = true
let%test _ = near_hundred 89 = false

(* 8. posNeg *)
let pos_neg a b c =
  match a, b, c with
  | x, y, true -> x < 0 && y < 0
  | x, y, false -> x < 0 || y < 0
;;

let%test _ = pos_neg 1 (-1) false = true
let%test _ = pos_neg (-1) 1 false = true
let%test _ = pos_neg (-4) (-5) true = true
let%test _ = pos_neg (-4) 5 true = false

(* 9. notString *)
let not_string s =
  if String.starts_with ~prefix:"not" s then s else Printf.sprintf "not %s" s
;;

let%test _ = not_string "candy" = "not candy"
let%test _ = not_string "not x" = "not x"
let%test _ = not_string "not bad" = "not bad"

(* 10. missingChar *)
let missing_char s i =
  let rec loop acc n l =
    match l with
    | [] -> acc
    | h :: t -> loop (if n = 0 then acc else h :: acc) (n - 1) t
  in
  loop [] i (List.of_seq (String.to_seq s)) |> List.rev |> List.to_seq |> String.of_seq
;;

let%test _ = missing_char "kitten" 1 = "ktten"
let%test _ = missing_char "kitten" 0 = "itten"
let%test _ = missing_char "kitten" 4 = "kittn"

let swap i j arr =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp
;;

(* 11. frontBack *)
let front_back s =
  let arr = Array.of_seq (String.to_seq s) in
  let last = Array.length arr - 1 in
  swap 0 last arr;
  String.of_seq (Array.to_seq arr)
;;

let%test _ = front_back "code" = "eodc"
let%test _ = front_back "ab" = "ba"
let%test _ = front_back "a" = "a"

(* 12. front3 *)
let front_3 s =
  let len = String.length s in
  let front = String.sub s 0 (if len < 3 then len else 3) in
  front ^ front ^ front
;;

let%expect_test _ =
  printf "%s" (front_3 "Java");
  [%expect {|JavJavJav|}];
  printf "%s" (front_3 "Chocolate");
  [%expect {|ChoChoCho|}];
  printf "%s" (front_3 "abc");
  [%expect {|abcabcabc|}]
;;

(* 13. backAround *)
let back_around s =
  let last = sprintf "%c" s.[String.length s - 1] in
  last ^ s ^ last
;;

let%expect_test _ =
  printf "%s" (back_around "cat");
  [%expect {|tcatt|}];
  printf "%s" (back_around "Hello");
  [%expect {|oHelloo|}];
  printf "%s" (back_around "a");
  [%expect {|aaa|}]
;;

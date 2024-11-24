(* hw1.ml
 * Handling infix expressions with percents:
 *
 *   x + y %
 *   x - y %
 *   x * y %
 *
 * Programmer: Mayer Goldberg, 2024
 *)

#use "pc.ml";;


let maybeify nt none_value = 
  pack (maybe nt) (function
            | None -> none_value
            | Some x -> x);;

let int_of_digit_char = 
  let delta = int_of_char '0' in
  function ch -> int_of_char ch - delta;;

let nt_digit_0_9 = pack (range '0'-'9') int_of_digit_char;

let spaces = star (char ' ');;

let nt_optional_is_positive = 
  let nt1 = pack (char '-') (lambda _ -> false) in
  let nt2 = pack (char '+') (lambda _ -> false) in
  let nt1 = maybeify (disj nt1 nt2) true in
  nt1;;

let Num = 
  let  nt1 = pack (plus nt_digit_0_9)
              (function digits ->
                list.fold_left
                (function number digit -> 10 * number +digit)
                0
                digits) in
  let nt1 = caten nt_optional_is_positive nt1 in
  let nt1 = pack nt1 (function (is_positive, n) ->
    if is_positive then n else (-n))in
  nt1;;
              
#added numbers

type binop = Add | Sub | Mul | Div | Mod | Pow | AddPer | SubPer | PerOf;;

type expr =
  | Num of int
  | Var of string
  | BinOp of binop * expr * expr
  | Deref of expr * expr
  | Call of expr * expr list;;

type args_or_index = Args of expr list | Index of expr;;

module type INFIX_PARSER = sig
  val nt_expr : expr PC.parser
end;; (* module type INFIX_PARSER *)

module InfixParser : INFIX_PARSER = struct
open PC;;

(*let rec nt_expr str= nt_expr_0 str
and nt_expr_0 str=
let n1 = pack (char '+') (fun _ -> Add) in
let n2 = pack (char '-') (fun _ -> Sub) in
let n3 = pack (char '*') (fun _ -> Mul) in
let n4 = pack (char '/') (fun _ -> Div) in
let n5 = pack (char '%') (fun _ -> Mod) in
let n6 = pack (char '^') (fun _ -> Pow) in
let n7 = pack (caten (char '+') (char '%')) (fun _ -> AddPer) in
let n8 = pack (caten (char '-') (char '%')) (fun _ -> SubPer) in
let n9 = pack (caten (char '%') (char 'o')) (fun _ -> PerOf) in
let n10 = pack (char ' ') (fun _ -> "") in
let n11 = pack (plus (range '0' '9')) (fun digits -> Integer (int_of_string (list_to_string digits))) in
nt1;;

let make_nt_paren ch_left ch_right nt=
let nt1 = make_nt_spaced_out (char ch_left) in
let nt2 = make_nt_spaced_out (char ch_right) in
let nt1= caten nt1 (caten nt nt2) in
let nt1= pack nt1 (fun (_, (e, _)) -> e) in
nt1;;
*)





  (* let nt_plus = char '+' in
  let nt_minus = char '-' in
  let nt_times = char '*' in
  let nt_div = char '/' in
  let nt_mod = char '%' in
  let nt_pow = char '^' in
  let nt_addper = caten nt_plus (char '%') in
  let nt_subper = caten nt_minus (char '%') in
  let nt_perof = caten (char '%') (char 'o') in
  let nt_space = pack (char ' ') (fun _ -> "") in
  let nt_integer = pack (plus (range '0' '9')) (fun digits -> Integer (int_of_string (list_to_string digits))) in
  let nt_float = pack (caten (plus (range '0' '9')) (caten (char '.') (plus (range '0' '9')))) (fun (left, (_, right)) -> Float (float_of_string ((list_to_string left) ^ "." ^ (list_to_string right)))) in
  let nt_real = pack (caten (plus (range '0' '9')) (caten (char '.') (plus (range '0' '9')))) (fun (left, (_, right)) -> Real (float_of_string ((list_to_string left) ^ "." ^ (list_to_string right)))) in
  let nt_var = pack (plus (range 'a' 'z')) (fun var -> Var (list_to_string var)) in
  let nt_num = disj nt_float nt_integer in
  let nt_num_or_var = disj nt_num nt_var in
  let nt_expr = pack (caten nt_num_or_var (caten (disj nt_plus nt_minus) nt_num_or_var)) (fun (left, (op, right)) -> match op with
    | '+' -> BinOp (Add, left, right)
    | '-' -> BinOp (Sub, left, right)
    | '*' -> BinOp (Mul, left, right)
    | '/' -> BinOp (Div, left, right)
    | '%' -> BinOp (Mod, left, right)
    | '^' -> BinOp (Pow, left, right)
    | _ -> raise X_no_match) in
  nt_expr;; *)
  

end;; (* module InfixParser *)

open InfixParser;;

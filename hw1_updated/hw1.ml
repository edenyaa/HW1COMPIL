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

 type num=
 |Integer of int;;


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

let nt_whitespace = const (fun ch -> ch <= ' ');; 
let make_nt_spaced_out nt= 
  let nt1=star nt_whitespace in
  let nt2=pack (caten nt1(caten nt nt1))(fun (_, (e, _))->e) in
  nt2;;
  

let make_nt_paren ch_left ch_right nt=
  let nt1 = make_nt_spaced_out (char ch_left) in
  let nt2 = make_nt_spaced_out (char ch_right) in
  let nt1= caten nt1 (caten nt nt2) in
  let nt1= pack nt1 (fun (_, (e, _)) -> e) in
  nt1;;

let maybeify nt none_value = 
  pack (maybe nt) (function
            | None -> none_value
            | Some x -> x);;

let int_of_digit_char = 
   let delta = int_of_char '0' in
   function ch -> int_of_char ch - delta;;            
let nt_digit_0_9 = pack (range '0' '9') int_of_digit_char;;

let nt_optional_is_positive = 
    let nt1 = pack (char '-') (fun _ -> false) in
    let nt2 = pack (char '+') (fun _ -> true) in
    let nt1 = maybeify (disj nt1 nt2) true in
    nt1;;

let nt_var =
    let nt1=  range_ci 'a' 'z' in  
    let nt2 = disj_list [range_ci 'a' 'z'; range_ci '0' '9'; char '_'; char '$'] in 
    let nt1=caten nt1 (star (disj nt1 nt2)) in
    let nt1=pack nt1 (fun (ch1, chs)->string_of_list(ch1::chs)) in
    let nt1=pack nt1 (fun name->Var name) in
    nt1;;
   
let nt_number = 
    let  nt1 = pack (plus nt_digit_0_9)
                (fun digits ->
                  List.fold_left
                   (fun number digit -> 10 * number +digit)
                   0
                   digits) in
    let nt1 = caten nt_optional_is_positive nt1 in
    let nt1 = pack nt1 (function (is_positive, n) ->
      if is_positive then n else (-n)) in
    nt1;;
   
    
let rec nt_expr str= nt_expr_0 str

  and nt_expr_0 str= 
        let nt1= pack(char '+')(fun _->Add) in
        let nt2= pack(char '-')(fun _->Sub) in
        let nt1=disj nt1 nt2 in
        let nt1=star(caten nt1 nt_expr_1) in
        let nt1=pack(caten nt_expr_1 nt1)
        (fun (expr1, binop_expr1s)->
          List.fold_left 
          (fun expr1 (binop, expr1')->
            BinOp (binop, expr1, expr1'))
          expr1 binop_expr1s) in
        let nt1=make_nt_spaced_out nt1 in
        nt1 str  
  and nt_expr_1 str= 
        let nt1= pack(char '*')(fun _->Mul) in
        let nt2= pack(char '/')(fun _->Div) in
        let nt3= pack(word "mod")(fun _->Mod) in
        let nt2=disj nt2 nt3 in
        let nt1=disj nt1 nt2 in
        let nt1=star(caten nt1 nt_expr_2) in
        let nt1=pack(caten nt_expr_2 nt1)
        (fun (expr2, binop_expr2s)->
          List.fold_left 
          (fun expr2 (binop, expr2')-> BinOp (binop, expr2, expr2'))
           expr2
           binop_expr2s) in
        let nt1=make_nt_spaced_out nt1 in
        nt1 str        
        
    (*pow*)   
  (*and nt_expr_2 str=
    let nt1= pack (char '^') (fun _->Pow) in
    let nt1=star(caten nt1 nt_expr_3) in
    let nt1=pack(caten nt_expr_3 nt1)
    (fun (expr3, binop_expr3s)->
      List.fold_right 
      (fun  (binop, expr3') acc -> BinOp (binop, acc, expr3'))
      binop_expr3s
      expr3) in
    let nt1=make_nt_spaced_out nt1 in
    nt1 str*)
    (*and nt_expr_2 str =
      let nt1 = pack (char '^') (fun _ -> Pow) in
      let nt1 = star (caten nt1 nt_expr_3) in
      let nt1 = pack (caten nt_expr_3 nt1)
      (fun (expr3, binop_expr3s) ->
        List.fold_left
          (fun acc (binop, expr3') -> BinOp (binop,  expr3', acc))
          expr3
           binop_expr3s) in
      let nt1 = make_nt_spaced_out nt1 in
      nt1 str*)
  and nt_expr_2 str=
    let nt1= pack (char '^') (fun _->Pow) in
    let nt1=star(caten nt1 nt_expr_3) in
    let nt1=pack(caten nt_expr_3 nt1)
    (fun (expr3, binop_expr3s)->
      List.fold_right 
      (fun  (binop, expr3') acc -> BinOp (binop, expr3', acc))
      binop_expr3s
      expr3) in
    let nt1=make_nt_spaced_out nt1 in
    nt1 str
    
  and nt_expr_3 str=
      let nt1=disj_list[ pack nt_number (fun num->Num num) ;
                   nt_var;
                    nt_paren] in
      let nt1=disj nt1 nt_var in
      let nt1=disj nt1 nt_paren in
      let nt1=make_nt_spaced_out nt1 in
      nt1 str
  and nt_paren str=
           disj_list [make_nt_paren '(' ')' nt_expr;
                              make_nt_paren '[' ']' nt_expr;
                              make_nt_paren '{' '}' nt_expr] str
                            ;;
     
         
 end;; (* module InfixParser *)
 
 open InfixParser;;
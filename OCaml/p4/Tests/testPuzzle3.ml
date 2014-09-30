(* Test Puzzle Functions 3  *)

#use "puzzle.ml";;

let a = [0;1;2;3];;
let b = [1;0;2;3];;

print_endline "single_move [[a]]" ;;
List.map prt_int_list_list (List.sort compare (single_move [[a]])) ;;

print_endline "single_move [[a;b]]" ;;
List.map prt_int_list_list (List.sort compare (single_move [[a;b]])) ;;

print_endline "single_move [[b;a]]" ;;
List.map prt_int_list_list (List.sort compare (single_move [[b;a]])) ;;

print_endline "single_move [[a];[b]]" ;;
List.map prt_int_list_list (List.sort compare (single_move [[a];[b]])) ;;

print_endline "single_move [[b];[a]]" ;;
List.map prt_int_list_list (List.sort compare (single_move [[b];[a]])) ;;

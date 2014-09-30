(* Test Puzzle Solver 2  *)

#use "puzzle.ml";;

let c = [1;0;2;3;4;5;6;7;8];;

(* Test solve_board b n *)

print_endline ("solve_board c 1") ;; 
List.map prt_int_list_list (solve_board c 1) ;;

print_endline ("solve_board c 10") ;; 
List.map prt_int_list_list (solve_board c 10) ;;

print_endline ("solve_board c 11") ;; 
List.map prt_int_list_list (List.sort compare (solve_board c 11)) ;;

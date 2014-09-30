(* CMSC 330 / Project 4 *)
(* Student: MY NAME *)

(* Fill in the implementation and submit puzzle.ml *)

#use "testUtils.ml";;

(*-------------------------------------------------------------------------*)
(* functions from lecture you may use *)

let rec map f l = match l with
    [] -> []
  | (h::t) -> (f h)::(map f t)
;;

let rec fold f a l = match l with
    [] -> a
  | (h::t) -> fold f (f a h) t
;;

let length x = fold (fun a y -> a+1) 0 x
;;

let rev x = fold (fun a y -> y::a) [] x
;;

(*-------------------------------------------------------------------------*)
(* Part 1: Recursion *)

(* 
get_val x n 	
int list -> int -> int 	
element of list x at index n, or -1 if not found
  (indexes start at 0)
Example: get_val [5;6;7;3] 1 => 6
*)

(*let finder a y = (*print_string "hi\n";*)
	if a > 0 then begin 
		a - 1;
		print_int a;
		print_string "\n";
		a -1
		end
	else y
;;

let get_val n x = fold finder n x 
;;

get_val 3 [1;2;3;7];;
print_string "break!\n";;
get_val 4 [1;2;3;7];;
*)

(* 
get_vals x y 	
int list -> int list -> int list 	
list of elements of list x at indexes in list y, 
[] if any indexes in y are outside the bounds of x;
elements must be returned in order listed in y 
Example: get_vals [5;6;7;3] [2;0] => [7;5]
*)

(*let get_vals x y = match y with
	[] ->
	| h::t -> fold gv_helper h x
;;*)

let rec get_val x n = match x,n with 
  | [],_ -> -1
  | (h :: t),0 -> h
  | (h :: t),i -> get_val t (n-1);;


let rec get_vals x y = match y with 
	(h::[]) -> [get_val x h]
	|(h::t) -> get_val x h::get_vals x t
	|[] -> []
;;

get_vals [5;6;7;3] [2;0];;
	
(* 
set_n x n v 	
'a list -> int -> 'a -> 'a list 	
list produced by setting n'th element of list x to value v 	
no effect if n is outside the bounds of the list 
Example: set_n [5;6;7;3] 1 9 => [5;9;7;3]
*)

let rec set_n x n v = match x,n with
	(h::t),0 -> v::set_n t (-1) v
	|(h::t),k -> h::set_n t (k-1) v
	|[],k -> []
;;


(* 
list_swap_val b u v
'a list -> 'a -> 'a -> 'a list 	
list b with values u,v swapped 	
(change value of multiple occurrences of u and/or v, if found, and
change value for u even if v not found in list, and vice versa )
Example: list_swap_val [5;6;7;3] 7 5 => [7;6;5;3]
*)

let rec list_swap_val b u v = match b with
	[] -> []
	|(h::t) -> if h=u then v::list_swap_val t u v 
	else if h=v then u::list_swap_val t u v 
	else h::list_swap_val t u v
;;

(* 
index x v
'a list -> 'a -> int 	
index of value v in list x, or -1 if not found
  (indexes start at 0)
Example: index [5;6;7;3] 7 => 2
*)

let rec index x v = match x with 
	[] -> -1
	|(h::t) -> if h=v then 0 else if (index t v) = -1 then -1 else ((index t v) + 1)
;;
	
(* 
uniq x 	
'a list -> 'a list 	
list of uniq elements in x 	
order of unique elements does not matter
Example: uniq [5;6;5;3] => [6;5;3]
*)

let contains x v = 
	if index x v = -1 then false else true
;;

let rec checker x n = match x with
	(h::t) -> if h=n then false else checker t n
	|[] -> true
;;

let rec uniq x = match x with
	(h::[]) -> [h]
	|(h1::t) -> if contains t h1 then uniq(t) else h1::(uniq t)
	|[]->[]
;;


(* 
find_new x y
'a list -> 'a list -> 'a list 	
list of members of list x not found in list y 	
maintain relative order of elements in result
Example: find_new [4;3;7] [5;6;5;3] => [4;7]
*)

let rec find_new x y = match x with
	[] -> []
	|[h] -> if contains y h then [] else [h]
	|(h::t) -> if contains y h then find_new t y else h::find_new t y
;;

(* 
is_sorted x 	
'a list -> bool 	
true if elements in x are in sorted order, false otherwise 	
  (return true for [])
Example: is_sorted [5;5;7;9] => true 
*)

let rec is_sorted x = match x with
	[] -> true
	|(h1::[]) -> true
	|h1::h2::[] -> if h1>h2 then false else true
	|(h1::h2::t) -> if h1>h2 then false else is_sorted(h2::t)
	
;;


(*-------------------------------------------------------------------------*)
(* Part 2: Higher order functions *)


(* 
grow_lists x y 	 
'a list -> 'a list -> 'a list list 	 
return a list of lists, where each element of x is prepended to y
resulting lists must be in same order as in x
Example: grow_lists [1;2] [3;4] => [[1;3;4]; [2;3;4]]
*)

(*let grow_lists x y = fold (fun a b -> if a=[[]] then [b::y] else a@[b::y]) [[]] x
;;*)

let grow_lists x y = map (fun z -> z::y) x
;;

(* 
concat_lists x 	
'a list list -> 'a list 	
return a list consisting of the lists in x concatenated together
(note just top level of lists is concatenated, unlike List.flatten)
Examples: concat_lists [[1;2];[7];[5;4;3]] => [1;2;7;5;4;3]
concat_lists [[[1;2;3];[2]];[[7]]] => [[1;2;3];[2];[7]]
*)

let concat_lists x = fold (fun a z -> a@z) [] x
;;
(*-------------------------------------------------------------------------*)
(* Part 3: Puzzle functions *)

(* 
find_board_size b 	 
'a list -> int 	 
return size (that is, the length/width of a side) of board b 
represented as a list (assume board is square)
Example: find_board_size [1;0;2;3;4;5;6;7;8] => 3
*)

let find_board_size b = 
    int_of_float(sqrt(float_of_int (length b)))
;;


(* 
pos_of_xy (x, y) s 	
(int * int) -> int -> int 	
index of x, y coordinate in a list representing a board of size s
return -1 if x or y is out of bounds (i.e., less than 0 or greater than s-1)
Example: pos_of_xy (1, 2) 3 => 5
*)

let pos_of_xy (x,y) s =
x*s+y
;;


(* 
xy_of_pos p s 	
int -> int -> (int * int)
x, y coordinate of index p in a list representing a board of size s
may assume p is a legal position between 0..s-1
Example: xy_of_pos 5 3 => (1, 2)
*)

let xy_of_pos p s =
(p/s,p mod s)
;;
(* 
move_pos b 	
int list -> int list 	
list of positions in board that can move to space in board
positions must be in sorted order, from smallest to largest
Example: move_pos [0;1;2;3;4;5;6;7;8] => [1;3]
*)



let move_pos b = 
	let s = find_board_size(b) in
	let s2 = int_of_float((float_of_int(s))**2.) in
	let x = index b 0 in
	if x = 0 then [1;s]
	else if x =(s-1) then [s-2;(2*s)-1]
	else if x = s2-s then [s2-(2*s);s2-s+1]
	else if x = (s2-1) then [s2-s-1;s2-2]
	else if x < s-1 && x > 0 then [x-1;x+1;x+s]
	else if x<s2-1 && x>s2-s then [x-s;x-1;x+1]
	else if x mod s = 0 then [x-s;x+1;x+s]
	else if x mod s = s-1 then [x-s;x-1;x+s]
	else [x-s;x-1;x+1;x+s]
;;

(* 
make_move b x 	
int list -> int -> int list 	
configuration of board after moving number at position x to space
may assume position x is adjacent to space
Example: make_move [0;1;4;5;2;3;6;7;8] 3 => [5;1;4;0;2;3;6;7;8] 
*)

let make_move b x = 
	let n = get_val b x in
	list_swap_val b 0 n;;
(* 
make_moves b 	
int list -> int list list 	
boards produced after all possible 1-step moves for board b
boards must be in sorted order, with space in smallest position to largest
Example: make_moves [0;1;2;3;4;5;6;7;8] => 
	[[1;0;2;3;4;5;6;7;8];[3;1;2;0;4;5;6;7;8]] 
*)

let rec mm_helper b x = match x with
	[] -> []
	|(h::t) -> make_move b h::mm_helper b t
;;

let make_moves b = 
	let a = move_pos b in
	match a with
	[] -> []
	| (h::t) -> make_move b h::mm_helper b t
;;


(* 
single_move x
int list list list -> int list list list
Given list of list of boards, return list of list of boards,
with 1-step move (for head of each list of boards L) prepended to L.
If n 1-step moves (M_1,M_2,...M_n) are possible for the head 
of board list L, then M_1::L, M2_2::L,...M_n::L should all
be added to the result.

Example: single_move [[[0;1;2;3]]] => 
	[ [[1;0;2;3];[0;1;2;3]] ; [[2;1;0;3];[0;1;2;3]] ]
*)


let helper1 x = match x with
	[] -> []
	|(h::t)->grow_lists (make_moves h) x
;;

let rec helper2 x = match x with
	[] -> []
	|(h::t) -> helper1(h)::helper2(t)
;;

let single_move x = concat_lists (helper2 x )
;;

single_move [[[1;0;2;3;4;5;6;7;8]]];;



(*-------------------------------------------------------------------------*)
(* Part 4: Puzzle solver *)

(* 
solve_board b n
int list -> int -> int list list list

Given board b, return all solutions of length n, or [] if none exists.
A solution to solve_board is a list of boards produced by moves
starting from b until the solved board is reached.  The list is in
reverse order: solved board first, b last.  

Example: solve_board [1;2;0;3;4;5;6;7;8] 2 => 
	[[[0;1;2;3;4;5;6;7;8];[1;0;2;3;4;5;6;7;8];[1;2;0;3;4;5;6;7;8]]]

The length L of each solution is the number of moves (i.e., one less
than the length of the list that represents the solution).  Solutions
are not permitted to contain the same intermediate board twice.  For
example, [[[0;1;2;3;4;5;6;7;8];[1;0;2;3;4;5;6;7;8];[0;1;2;3;4;5;6;7;8];
[1;0;2;3;4;5;6;7;8];[1;2;0;3;4;5;6;7;8]]] is not a legal length-4 
solution to [1;2;0;3;4;5;6;7;8]. The order of possible solutions does 
not matter.
 
Hints: as you are required to produce *all* solutions up to length n,
you are essentially doing an exhaustive search with a bit of smarts to
prune out paths containing duplicate boards.  That is, at each step
you will want to enumerate all possible boards produced by legal moves
from the current board of each path produced by prior steps.  You will
prune out paths that would be produced by repeating a previous board
position.  You should be making good use of the functions you have
already defined above.  If your solution is not using many of these
functions, you are doing too much work! *)


let rec scraper b = match b with 
	[]->[] 
	|(h::t)->match h with 
		[]->[] 
		|(h2::h3)-> if is_sorted h2 then h::(scraper t) else scraper t 
;; 
(*so this guy basically grabs the sequences that don't belong. i couldn't think
of a less crappy name...haha*)
	
let rec helper3 b = 
	match b with 
		[]->[] 
		|(h::t)-> match h with 
			[]->[] 
			|(h2::t2)-> if index t2 h2 = -1 
						then h::(helper3 t) 
						
						else (helper3 t)
;;



let rec helper4 b n = if n = 0 
						then helper3(single_move b) 

						else concat_lists [helper3(single_move b);(helper4(helper3(single_move b))(n-1))];;



let solve_board b n = 
	if n = 0 
	then [] 

	else scraper(helper4 [[b]] (n-1))
;;

solve_board [1;2;0;3;4;5;6;7;8] 2
;;

(*-------------------------------------------------------------------------*)
(* Bonus debugging utilities *)

(* print board as nXn square, with space @ 0 *)
let print_board b = 
    let board_size = (find_board_size b) in 
    let rec print_board_helper (b, x) = match b with
	  [] -> print_endline "------------------------"
	| (h::t) -> 
		print_string (if (h < 10) then "   " 
			else if (h < 100) then "  " else " ");
		if (h = 0) then print_string " " else (print_int h) ; 
		(if ((x mod board_size) = 0) then print_endline "") ; 
		print_board_helper (t, x+1)
    in print_board_helper (b, 1)
;;

(* print list of boards *)
let print_boards x = 
	print_endline "------------------------" ;
	ignore (map print_board x) ;
	print_endline ""
;;

(* print some boards & lists of boards *)

(* uncomment following to test print! *)
(*
let try_out_print =
	let b1 = [0;1;2;3;4;5;6;7;8] in
	let b2 = [1;0;2;3;4;5;6;7;8] in
	let blist = [b1;b2] in
		prt_int_list b1;
		print_board b1;
		prt_int_list b2;
		print_board b2;
		prt_int_list_list blist;
		print_boards blist
;;
*)


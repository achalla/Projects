/* return n'th element of list, or fail if not found */

get_val_test(L,N) :- findall(V, get_val(L,N,V), R), write(R), nl.
get_val_public :-
	get_val_test([5,6,7,3],0),
	get_val_test([5,6,7,3],1),
	get_val_test([5,6,7,3],2),
	get_val_test([5,6,7,3],3),
	get_val_test([5,6,7,3],4).

/* list of values at list of indices */
	
get_vals_test(L,N) :- findall(V, get_vals(L,N,V), R), write(R), nl.
get_vals_public :-
	get_vals_test([5,6,7,3],[0]),
	get_vals_test([5,6,7,3],[1]),
	get_vals_test([5,6,7,3],[3]),
	get_vals_test([5,6,7,3],[0,2]),
	get_vals_test([5,6,7,3],[0,2,1]),
	get_vals_test([5,6,7,3],[0,2,1,3]),
	get_vals_test([5,6,7,3],[4]).

/* set n'th element of list x to value v, return resulting list */
	
set_n_test(L,N,X) :- findall(V, set_n(L,N,X,V), R), write(R), nl.
set_n_public :-
	set_n_test([5,6,7,3],0,9),
	set_n_test([5,6,7,3],1,9),
	set_n_test([5,6,7,3],2,9),
	set_n_test([5,6,7,3],3,9),
	set_n_test([5,6,7,3],4,9).

/* swap values u,v in list b, assume all list elements are unique */

list_swap_test(L,X,Y) :- findall(V, list_swap_val(L,X,Y,V), R), write(R), nl.
list_swap_public :-
	list_swap_test([5,6,7,3],7,5),
	list_swap_test([5,6,7,3],6,5),
	list_swap_test([5,6,7,3],3,5),
	list_swap_test([5,6,7,3],7,3).

/* returns index of value v in list x, if found */

index_test(L,X) :- findall(V, index(L,X,V), R), write(R), nl.
index_public :-
	index_test([5,6,7,3],5),
	index_test([5,6,7,3],6),
	index_test([5,6,7,3],7),
	index_test([5,6,7,3],3),
	index_test([7,5,6,5],5),
	index_test([7,5,6,5],4).
/* list of positions that can move to space */

move_pos_test(B) :- findall(V, move_pos(B,V), R), write(R), nl.
move_pos_public :-
	move_pos_test([0,1,2,3]),
	move_pos_test([1,2,0,3]),
	move_pos_test([0,1,2,3,4,5,6,7,8]),
	move_pos_test([1,2,3,4,0,5,6,7,8]),
	move_pos_test([1,2,3,4,5,0,6,7,8]).

/* make move given (board, position of value to be moved) */

make_move_test(B,P) :- findall(V, make_move(B,P,V), R), write(R), nl.
make_move_public :-
	make_move_test([0,1,2,3],1),
	make_move_test([0,1,2,3],2),
	make_move_test([1,2,0,3],0),
	make_move_test([1,2,0,3],3),
	make_move_test([0,1,2,3,4,5,6,7,8],1),
	make_move_test([0,1,2,3,4,5,6,7,8],3),
	make_move_test([1,2,3,4,0,5,6,7,8],1),
	make_move_test([1,2,3,4,0,5,6,7,8],3),
	make_move_test([1,2,3,4,0,5,6,7,8],5),
	make_move_test([1,2,3,4,0,5,6,7,8],7),
	make_move_test([1,2,3,4,5,0,6,7,8],4),
	make_move_test([1,2,3,4,5,0,6,7,8],8).

/* make all possible moves for given board, return resulting boards */

make_moves_test(B) :- setof(V, make_moves(B,V), R), write(R), nl.
make_moves_public :-
	make_moves_test([0,1,2,3]),
	make_moves_test([1,2,0,3]),
	make_moves_test([0,1,2,3,4,5,6,7,8]),
	make_moves_test([1,2,3,4,0,5,6,7,8]),
	make_moves_test([1,2,3,4,5,0,6,7,8]).

/* given list of board positions, add new position of head of list */
 
single_move_test(X) :- setof(V, single_move(X,V), R), write(R), nl.
single_move_public :-
        single_move_test([[0,1,2,3]]),
        single_move_test([[0,1,2,3],[2,1,0,3]]),
        single_move_test([[2,1,0,3],[0,1,2,3]]).

/* find solutions for board x within n steps */

solve_board_test(B,N) :- setof(V, solve_board(B,N,V), R), write(R), nl.
solve_board_public :-
	solve_board_test([1,0,2,3],3),
	solve_board_test([1,3,2,0],3),
	solve_board_test([1,2,0,3,4,5,6,7,8],4),
	solve_board_test([3,1,2,6,4,5,7,8,0],4),
	solve_board_test([1,0,2,3,4,5,6,7,8],2),
	solve_board_test([1,0,2,3,4,5,6,7,8],12).

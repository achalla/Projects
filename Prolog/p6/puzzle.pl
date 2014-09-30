/*--------------------------------------------------------------*/
/* HELPER FUNCTIONS */

/* len - find length of list */

len([],0).
len([_|T],Result) :-
	len(T,R),
	Result is R+1.
	
/* is_sorted - whether elements of list are in sorted order */

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|Z]) :- 
	(X<Y),
	is_sorted([Y|Z]).

/* find_board_size - find height/width of board in list form */

find_board_size(B,Result) :- 
	len(B,L),
	fHelper(L,Result).
fHelper(4,2).
fHelper(9,3).
fHelper(16,4).
fHelper(25,5).
fHelper(36,6).
fHelper(49,7).
fHelper(64,8).

/* pos_of_xy */

pos_of_xy(X,Y,S,Result) :-
	T1 is X*S,
	Result is T1+Y.
	
/* xy_of_pos */

xy_of_pos(P,S,X,Y) :-
	X is P div S,
	Y is P mod S.

/*--------------------------------------------------------------*/
/* PROJECT FUNCTIONS */

/* get_val - return N'th element of list L, or fail if not found */

get_val([H|_],0,H).
get_val([_|T],N,R) :- 
	N > 0, 
	N1 is N-1,
	get_val(T,N1,R1),
	R = R1. 

/* ASK get_vals - return list of values in L at list of indices N */


get_vals(_,[],[]).
get_vals(X,[H|T],R) :- 
	get_val(X,H,R1),
	get_vals(X,T,R2),
	R = [R1|R2]. 

/* set_n - set N'th element of list N to value V, return result */

set_n([],_,_,[]).
set_n([_|T],0,V,R) :- 
	set_n(T,-1,V,R2),!,
	R = [V|R2].
	
set_n([H|T],N,V,R) :- 
	N1 is N-1,
	set_n(T,N1,V,R1),
	R = [H|R1].



/* list_swap_val - swap values U, V in list B */

list_swap_val([],_,_,[]).
list_swap_val([H|T],U,V,R) :- 
	H = U,!,
	list_swap_val(T,U,V,R2),
	R = [V|R2].
list_swap_val([H|T],U,V,R) :- 
	H = V,!,
	list_swap_val(T,U,V,R2),
	R = [U|R2].
list_swap_val([H|T],U,V,R):-
	list_swap_val(T,U,V,R2),
	R = [H|R2].
	

/* returns index of value V in list X, if found */


index(X,V,R) :- 
	index_helper(X,0,V,R).
	
index_helper([],_,_,_) :- fail.
index_helper(-1,N,_,N). /*sentinel case*/

index_helper([H|_],N,V,R) :-
	H = V,
	index_helper(-1,N,V,R2),
	R = R2.
	
index_helper([_|T],N,V,R) :-
	N1 is N+1,
	index_helper(T,N1,V,R).
	
	
	

/* position in board B that can move to space */

move_pos(B,R) :- 
	find_board_size(B,S),
	index(B,0,X),
	mph(S,X,R).

/*if x = 0 then [1;s]*/
	
mph(_,X,R) :- 
	X = 0,
	R = 1.
	
mph(S,X,R) :- 
	X = 0,!,
	R = S.

/*else if x =(s-1) then [s-2;(2*s)-1]*/

mph(S,X,R) :- 
	X is S-1,
	R is S-2.
	
mph(S,X,R) :- 
	X is S-1,!,
	R is (2*S)-1.
	
/*else if x = s2-s then [s2-(2*s);s2-s+1] */

mph(S,X,R) :- 
	X is (S*S)-S,
	R is (S*S)-(2*S).
	
mph(S,X,R) :- 
	X is (S*S)-S,!,
	R is (S*S)+1-S.
	
/*else if x = (s2-1) then [s2-s-1;s2-2]*/

mph(S,X,R) :- 
	X is (S*S)-1,
	R is (S*S)-S-1.
	
mph(S,X,R) :- 
	X is (S*S)-1,!,
	R is(S*S)-2.

/*else if x < s-1 && x > 0 then [x-1;x+1;x+s]*/	

mph(S,X,R) :- 
	X < S-1,
	X > 0,
	R is X-1.
	
mph(S,X,R) :- 
	X < S-1,
	X > 0,
	R is X+1.

mph(S,X,R) :- 
	X < S-1,
	X > 0,!,
	R is X+S.
	
/*else if x<s2-1 && x>s2-s then [x-s;x-1;x+1]*/

mph(S,X,R) :- 
	X < (S*S)-1,
	X > (S*S)-S,
	R is X-S.
	
mph(S,X,R) :- 
	X < (S*S)-1,
	X > (S*S)-S,
	R is X-1.
	
mph(S,X,R) :- 
	X < (S*S)-1,
	X > (S*S)-S,!,
	R is X+1.

/*else if x mod s = 0 then [x-s;x+1;x+s]*/

mph(S,X,R) :- 
	0 is mod(X,S),
	R is X-S.

mph(S,X,R) :- 
	0 is mod(X,S),
	R is X+1.
	
mph(S,X,R) :- 
	0 is mod(X,S),!,
	R is X+S.

/*else if x mod s = s-1 then [x-s;x-1;x+s]*/

mph(S,X,R) :- 
	mod(X,S,M),
	M is S-1,
	R is X-S.
	
mph(S,X,R) :- 
	mod(X,S,M),
	M is S-1,
	R is X-1.
	
mph(S,X,R) :- 
	mod(X,S,M),
	M is S-1,!,
	R is X+S.
	
/*else [x-s;x-1;x+1;x+s]*/
	
mph(S,X,R) :- 
	R is X-S.

mph(_,X,R) :- 
	R is X-1.
	
mph(_,X,R) :- 
	R is X+1.
	
mph(S,X,R) :- 
	R is X+S.	
	

/*modderf'er:*/	
	
mod(X,S,R) :-
	R is mod(X,S).
	
	
/*	let s = find_board_size(b) in
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
*/


	

/* make move given board B and position X of value to be moved */

make_move(B,X,R) :- 
	get_val(B,X,V),
	list_swap_val(B,0,V,R).

/* make move for given board B, return resulting board */

make_moves(B,R) :- 
	move_pos(B,P),
	make_move(B,P,R).
	
/* make move for board at beginning of list X, return new list of boards */

single_move([H|T],R) :-
	make_moves(H,R2),
	R = [R2,H|T].

/* find solution for board B within N steps */



solve_board(B,N,R) :- 
	single_move([B],X), /*brackets?*/
	N1 is N-1,
	sbh(X,N1,R).
	
sbh([H|T],_,R) :-
	is_sorted(H),!,
	R = [H|T].
	
sbh(X,N,R) :-
	N>0,
	single_move(X,R2),
	weeder(R2),
	N1 is N-1,
	sbh(R2,N1,R).

/*takes a list of boards and invalidates the list if there are duplicates*/
weeder([]) :- true.
weeder([H|T]) :- 
	outter(H,T),
	weeder(T).
	
	
/*takes a board and a board list*/
outter(_,[]) :- true.
outter(B,[H|T]) :-
	not(same_boards(B,H)),
	outter(B,T).

/*takes two boards*/
same_boards([],[]) :- true.
same_boards([],_) :- true.
same_boards(_,[]) :- true.
same_boards([H1|T1],[H2|T2]) :-
	H1 = H2,
	same_boards(T1,T2).
	
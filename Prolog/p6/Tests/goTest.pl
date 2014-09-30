:- initialization(
    [ 'puzzle.pl'
    , 'publicRecursion1.pl'
    , 'publicRecursion2.pl'
    , 'publicPuzzle.pl'
    , 'publicSolve.pl'
    ]).
run_f(Func) :-
    name(Func,FunName),
    writef("Testcase: %s\n",[FunName]),
    Func.
testcases(
    [ get_val_public
    , get_vals_public
    , set_n_public
    , list_swap_public
    , index_public
    , move_pos_public
    , make_move_public
    , make_moves_public
    , single_move_public
    , solve_board_public
    ]).
run :-
    testcases(TC),
    maplist(run_f,TC).

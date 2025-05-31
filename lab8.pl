incomplete_tree(t(7, t(5, t(3, _, _), t(6, _, _)), t(11, _, _))).
complete_tree(t(7, t(5, t(3, nil, nil), t(6, nil, nil)), t(11, nil, nil))).

% transform an incomplete list into a complete list
convertIL2CL([H|_], []) :- var(H), !.
convertIL2CL([H|T], [H|R]) :- convertIL2CL(T, R).

% transform a complete list into an incomplete list
convertCL2IL([], [_]).
convertCL2IL([H|T], [H|R]) :- convertCL2IL(T, R).

/*
convertCL2IL([H|[]], [H|_]) :- !.
convertCL2IL([H|T], [H|R]) :- convertCL2IL(T, R).
*/

% append 2 incomplete lists (the result is an incomplete list)
append_il(L1, L2, L2) :- var(L1), !.
append_il([H|T], L2, [H|R]) :- append_il(T, L2, R).

% reverse an incomplete list
reverse_il_bwd(L, L) :- var(L), !.
reverse_il_bwd([H|T], R) :- 
    reverse_il_bwd(T, R1),
    append_il(R1, [H|_], R). 

reverse_il_fwd(L, L) :- var(L), !.
reverse_il_fwd(List, Reversed) :- 
    List = [H|T],
    reverse_il_fwd_acc(T, [H|_], Reversed).
reverse_il_fwd_acc([], Acc, Acc).
reverse_il_fwd_acc([H|T], Acc, Result) :- 
    reverse_il_fwd_acc(T, [H|Acc], Result).

% flatten a deep incomplete list
flat_il(L, L) :- var(L), !.
flat_il([H|T], [H|R]) :- atomic(H), !, flat_il(T, R).
flat_il([H|T], R) :- flat_il(H, R1), flat_il(T, R2), 
	append_il(R1, R2, R).  % or append(R1, R2, R), !.

preorder_it(T, _) :- var(T), !.
preorder_it(t(K, L, R), List) :- 
	preorder_it(L, LL),
	preorder_it(R, LR),
	append_il([K|LL], LR, List).

% transform an incomplete tree into a complete tree
convertIT2CT(t(K, _, _), nil) :- var(K), !.
convertIT2CT(t(K, L, R), t(K, RL, RR)) :- 
	convertIT2CT(L, RL), 
	convertIT2CT(R, RR).

% transform a complete tree into an incomplete tree
convertCT2IT(nil, _) :- !.
convertCT2IT(t(K, L, R), t(K, RL, RR)) :- 
	convertCT2IT(L, RL), 
	convertCT2IT(R, RR).

max(A, B, A) :- A > B, !.
max(_, B, B).

% compute the height of an incomplete binary tree
height_it(T, 0) :- var(T), !.
height_it(t(_, L, R), H) :-
    height_it(L, H1),
    height_it(R, H2),
    max(H1, H2, H3),
    H is H3 + 1.

% compute the diameter of an incomplete binary tree
diam_it(T, 0) :- var(T), !.
diam_it(t(_, L, R), D) :- 
		diam_it(L, DL),
		diam_it(R, DR),
		height_it(L, HL),
		height_it(R, HR),
		D1 is HL + HR + 1,
		D is max(D1, max(DL, DR)).


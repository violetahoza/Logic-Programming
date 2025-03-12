member1(X, [X|_]).
member1(X, [_|T]) :- member1(X, T).

append1([], L2, L2).
append1([H|T], L2, [H|TailR]) :- append1(T, L2, TailR).

delete_all(X, [X|T], R) :- delete_all(X, T, R).
% if the first occurence was deleted, it continues on the rest of the elements
delete_all(X, [H|T], [H|R]) :- delete_all(X, T, R).
delete_all(_, [], []).

add_first(X, L, [X|L]).

append3([], [], L3, L3).
append3([], [H|T], L3, [H|R]) :- append3([], T, L3, R).
append3([H|T], L2, L3, [H|R]) :- append3(T, L2, L3, R).

sum_bwd([], 0).
sum_bwd([H|T], R) :- sum_bwd(T, R1), R is R1 + H.

sum_fwd([], Acc, Acc).
sum_fwd([H|T], Acc, R) :- Acc1 is Acc + H, sum_fwd(T, Acc1, R).
sum_fwd([H|T], R) :- sum_fwd([H|T], 0, R).

separate_parity([], [], []).
separate_parity([H|T], [H|E], O) :- 0 is H mod 2,
    separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :- 1 is H mod 2,
    separate_parity(T, E, O).

sep_par_bwd([], [], []).
sep_par_bwd([H|T], E, O) :- 0 is H mod 2,
	sep_par_bwd(T, E1, O), 
    E = [H|E1].
sep_par_bwd([H|T], E, O) :- 1 is H mod 2,
	sep_par_bwd(T, E, O1), 
    O = [H|O1].

sep_par_fwd([], E, O, E, O).
sep_par_fwd([H|T], E, O, AccE, AccO) :- 1 is H mod 2,
    AccO1 = [H|AccO],
    sep_par_fwd(T, E, O, AccE, AccO1).
sep_par_fwd([H|T], E, O, AccE, AccO) :- 0 is H mod 2,
    AccE1 = [H|AccE],
    sep_par_fwd(T, E, O, AccE1, AccO).
sep_par_fwd(L, E, O) :- sep_par_fwd(L, E1, O1, [], []),
    reverse(E1, E), reverse(O1, O).

remove_duplicates([], []).
remove_duplicates([H|T], [H|R]) :- delete_all(H, T, R1), 
    remove_duplicates(R1, R).

remove_duplicates1([], []).
remove_duplicates1([H|T], R) :- member(H, T), 
    remove_duplicates1(T, R).
remove_duplicates1([H|T], [H|R]) :- \+ member(H, T), 
    remove_duplicates1(T, R).

replace_all(_, _, [], []).
replace_all(X, Y, [X|T], [Y|R]) :- replace_all(X, Y, T, R).
replace_all(X, Y, [H|T], [H|R]) :- replace_all(X, Y, T, R).

drop_k(L, N, R) :- drop_k_helper(L, N, 1, R).
drop_k_helper([], _, _, []).
drop_k_helper([_|T], N, Pos, R) :- Pos = N,
    drop_k_helper(T, N, 1, R).
drop_k_helper([H|T], N, Pos, [H|R]) :- Pos \= N, NewPos is Pos + 1,
    drop_k_helper(T, N, NewPos, R).

remove_consecutive_duplicates([], []).
remove_consecutive_duplicates([X], [X]).
remove_consecutive_duplicates([X, X|T], Result) :- 
        remove_consecutive_duplicates([X|T], Result).
remove_consecutive_duplicates([X, Y|T], [X|Result]) :- 
        X\=Y, 
        remove_consecutive_duplicates([Y|T], Result).

pack_consecutive_duplicates([], []).
pack_consecutive_duplicates([X], [[X]]).
pack_consecutive_duplicates([X, X|T], [[X|Result]|Result1]) :- 
        pack_consecutive_duplicates([X|T], [Result|Result1]).
pack_consecutive_duplicates([X, Y|T], [[X]|Result]) :-
        X\=Y, 
        pack_consecutive_duplicates([Y|T], Result).

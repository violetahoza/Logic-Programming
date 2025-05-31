tree1(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))).
ternary_tree(t(6, t(4, t(2, nil, nil, nil), nil, t(7, nil, nil, nil)), t(5, nil, nil, nil), t(9,
t(3, nil, nil, nil), nil, nil))).

ternary_preorder(t(K, L, M, R), List) :-
    ternary_preorder(L, LL),
    ternary_preorder(M, LM),
    ternary_preorder(R, LR),
    append([K|LL], LM, Aux),
    append(Aux, LR, List).
ternary_preorder(nil, []).

ternary_inorder(t(K, L, M, R), List) :- 
		ternary_inorder(L, LL),
		ternary_inorder(M, LM),
		ternary_inorder(R, LR),
		append(LL, [K|LM], Aux),
		append(Aux, LR, List).
ternary_inorder(nil, []).

ternary_postorder(t(K, L, M, R), List) :-
    ternary_postorder(L, LL),
    ternary_postorder(M, LM),
    ternary_postorder(R, LR),
    append(LL, LM, Aux),
    append(Aux, LR, Aux2),
    append(Aux2, [K], List).
ternary_postorder(nil, []).

print_key(K, D) :- D > 0, !, D1 is D - 1, 
    tab(8), print_key(K, D1).
print_key(K, _) :- write(K), nl.

pretty_print_ternary(T) :- pretty_print_ternary(T,0).
pretty_print_ternary(nil, _).
pretty_print_ternary(t(K, L, M, R), D) :- 
    D1 is D + 1,
    print_key(K, D),
    pretty_print_ternary(L, D1),
    pretty_print_ternary(M, D1),
    pretty_print_ternary(R, D1).

ternary_height(nil, 0).
ternary_height(t(_, L, M, R), H) :-
    ternary_height(L, H1),
    ternary_height(M, H2),
    ternary_height(R, H3),
    HMax is max(max(H1, H2), H3),
    H is HMax + 1.

leaf_list(nil, []).
leaf_list(t(K, nil, nil), [K]) :- !.
leaf_list(t(_, L, R), Res) :- 
    leaf_list(L, LL),
    leaf_list(R, LR),
    append(LL, LR, Res).

%preorder
internal_list(nil, []).
internal_list(t(_, nil, nil), []) :- !.
internal_list(t(K, L, R), [K|Res]) :-
    internal_list(L, LL),
    internal_list(R, LR),
    append(LL, LR, Res).

%inorder
internal_list1(nil, []).
internal_list1(t(_, nil, nil), []) :- !.
internal_list1(t(K, L, R), Res) :-
    internal_list1(L, LL),
    internal_list1(R, LR),
    append(LL, [K|LR], Res).

/*
internal_list(t(_, nil, nil), []).
internal_list(t(K, L, nil), Res) :-
    internal_list(L, R1),
    append([K], R1, Res).
internal_list(t(K, nil, R), Res) :-
    internal_list(R, R1),
    append([K], R1, Res).
internal_list(t(K, L, R), Res) :-
    internal_list(L, R1),
    internal_list(R, R2),
    append(R1, [K|R2], Res).
*/

height(nil, 0).
height(t(_, L, R), H):-
   height(L, H1),
   height(R, H2),
   max(H1, H2, H3),
   H is H3+1.
max(A, B, A):-A>B, !.
max(_, B, B).

same_depth(t(K, _, _), 0, [K]).
same_depth(t(_, L, R), D, Res) :-
    D > 0, D1 is D - 1,
    same_depth(L, D1, LL),
    same_depth(R, D1, LR),
    append(LL, LR, Res).
same_depth(nil, _, []).
same_depth(_, _, []) :- !.

diam(t(_, L, R), D) :-
    diam(L, DL), 
    diam(R, DR),
    height(L, HL),
    height(R, HR), 
    DM is HL + HR + 1,
    max_list([DM, DL, DR], D).
diam(nil, 0).

diam1(T, D) :- diam1(T, _, D).
diam1(t(_, L, R), H, D) :-
    diam1(L, HL, DL), 
    diam1(R, HR, DR),
    max_list([HL, HR], HMax),
    H is HMax + 1,
    DM is HL + HR + 1,
    max_list([DM, DL, DR], D).
diam1(nil, 0, 0).

mirror(nil, nil).
mirror(t(_, L1, R1), t(_, L2, R2)) :-
    mirror(L1, R2),
    mirror(R1, L2).

symmetric(nil).
symmetric(t(_, L, R)) :- 
		mirror(L, R).

get_pred(t(Pred, L, nil), Pred, L) :- !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :- get_pred(R, Pred, NR).

get_succ(t(Succ, nil, R), Succ, R):- !.
get_succ(t(Key, L, R), Succ, t(Key, NL, R)) :- get_succ(L, Succ, NL).

delete_key(Key, t(Key, L, nil), L):- !.
delete_key(Key, t(Key, nil, R), R):- !.
delete_key(Key, t(Key, L, R), t(Succ, L, NR)) :- !, get_succ(R, Succ, NR).
delete_key(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, delete_key(Key, L, NL).
delete_key(Key, t(K, L, R), t(K, L, NR)) :- delete_key(Key, R, NR).

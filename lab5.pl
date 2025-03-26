sel_sort(L, [M|R]):- min1(L, M), 
    delete1(M, L, L1), sel_sort(L1, R).
sel_sort([], []).

delete1(X, [X|T], T) :- !.
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).
delete1(_, [], []).

min1([H|T], M) :- min1(T, M), M < H, !.
min1([H|_], H).

max2([H|T], M) :- max2(T, M), M > H, !.
max2([H|_], H).

sel_sort_max(L, [M|R]):- max2(L, M),
    delete1(M, L, L1), sel_sort_max(L1, R).
sel_sort_max([], []).

ins_sort([H|T], R) :- ins_sort(T, R1),
    insert_ord(H, R1, R).
ins_sort([], []).
insert_ord(X, [H|T], [H|R]) :- X > H, !,
    insert_ord(X, T, R).
insert_ord(X, T, [X|T]).

insert_ord2(X, [H|T], Acc, R) :- 
    X > H, !, insert_ord2(X, T, [H|Acc], R).
insert_ord2(X, T, Acc, R) :- reverse([X|Acc], Rev),
    append(Rev, T, R).

ins_sort_fwd([H|T], Acc, R) :- 
    insert_ord2(H, Acc, [], NewAcc),
    ins_sort_fwd(T, NewAcc, R).
ins_sort_fwd([], R, R).
ins_sort_fwd(L, R) :- ins_sort_fwd(L, [], R).

bubble_sort(L,R):- one_pass(L,R1,F),
    nonvar(F), !, bubble_sort(R1,R).
bubble_sort(L,L).

one_pass([H1,H2|T], [H2|R], F):- H1 > H2, !, F = 1, 
    one_pass([H1|T],R,F).
one_pass([H1|T], [H1|R], F):- one_pass(T, R, F).
one_pass([], [] ,_).

bubble_sort_fixed(L, 0, L).
bubble_sort_fixed(L, K, R) :- one_pass2(L, R1), 
   K1 is K - 1, K > 0, !, bubble_sort_fixed(R1, K1, R).

one_pass2([H1,H2|T], [H2|R]):- H1 > H2, !,
    one_pass2([H1|T],R).
one_pass2([H1|T], [H1|R]):- one_pass2(T, R).
one_pass2([], []).

ord_chars(X, [H|T], [H|R]) :-
    char_code(X, X_code), char_code(H, H_code),
    X_code > H_code, !, ord_chars(X, T, R).
ord_chars(X, T, [X|T]).
    
sort_chars([H|T], R) :- sort_chars(T, R1),
    ord_chars(H, R1, R).
sort_chars([], []).

ord_lens(X, [H|T], [H|R]) :-
    length(X, Len1), length(H, Len2),
    Len1 > Len2, !, ord_lens(X, T, R).
ord_lens(X, T, [X|T]).
    
sort_lens([H|T], R) :- sort_lens(T, R1),
    ord_lens(H, R1, R).
sort_lens([], []).

perm_sort(L,R):- perm(L, R), is_ordered(R), !.
perm(L, [H|R]):- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).
is_ordered([H1, H2|T]):- H1 =< H2, is_ordered([H2|T]).
is_ordered([_]). % if only one element remains, the list is already ordered

delete1c(X, [X|T], T) :- !. % șterge prima apariție și se oprește
delete1c(X, [H|T], [H|R]) :- delete1c(X, T, R). % altfel iterează peste elementele listei
delete1c(_, [], []). % daca a ajuns la lista vida înseamnă că elementul nu a fost găsit și putem returna lista vidă

perm2([], []).
perm2(L, [H|R]) :- member(H, L), delete1c(H, L, R1), perm2(R1, R). 


% Arithmetic Operations

gcd(A, 0, A).
gcd(A, B, R) :- B \= 0, Rest is A mod B, gcd(B, Rest, R).
lcm(A, B, R) :- gcd(A, B, R1), R is (A * B) / R1.

to_binary(0, Acc, Acc) :- !.
to_binary(X, Acc, R) :- X > 0, Bit is X mod 2,
    Y is X // 2, to_binary(Y, [Bit|Acc], R).
to_binary(X, R) :- to_binary(X, [], R).

divisor(0, 'alot') :- !.
divisor(1, [1]) :- !.
divisor(N, D) :- N > 1,
    divs(N, 1, [], R),
    reverse(R, D).

divs(N, Curr, Acc, Acc) :- Curr > N, !.
divs(N, Curr, Acc, R) :- 0 is N mod Curr, !,
    NewAcc = [Curr|Acc], Next is Curr + 1,
    divs(N, Next, NewAcc, R).
divs(N, Curr, Acc, R) :- Next is Curr + 1,
    divs(N, Next, Acc, R).

/*
reverse(N, R) :- reverse(N, 0, R).
reverse(0, Acc, Acc) :- !.
reverse(N, Acc, R) :- N > 0, D is N mod 10, 
    N1 is N // 10, NewAcc is Acc * 10 + D,
    reverse(N1, NewAcc, R).
*/

% Operations on Lists

sum([], Acc, Acc).
sum([H|T], Acc, R) :- NewAcc is Acc + H, sum(T, NewAcc, R).
sum(L, R) :- sum(L, 0, R).

numbers([], []).
numbers([H|T], [H1|R]) :- 1 is H mod 2, !, H1 is 2 * H, numbers(T, R).
numbers([H|T], [H1|R]) :- H1 is H * H, numbers(T, R).

separate_parity(L, E, R) :- separate_parity(L, 1, E, R).
separate_parity([], _, [], []).
separate_parity([H|T], Pos, [H|E], Rest) :-
    1 is Pos mod 2, 0 is H mod 2, !, NewPos is Pos + 1,
    separate_parity(T, NewPos, E, Rest).
separate_parity([H|T], Pos, E, [H|Rest]) :- NewPos is Pos + 1,
    separate_parity(T, NewPos, E, Rest).

/*
replace_all(_, _, [], []).
replace_all(X, Y, [X|T], [Y|R]) :- !, replace_all(X, Y, T, R).
replace_all(X, Y, [H|T], [H|R]) :- replace_all(X, Y, T, R).
*/

delete_pos_even(L, X, R) :- delete_pos_even(L, X, 1, R).
delete_pos_even([], _, _, []).
delete_pos_even([X|T], X, Pos, [X|R]) :-
    1 is Pos mod 2, !, NewPos is Pos + 1, 
    delete_pos_even(T, X, NewPos, R).
delete_pos_even([X|T], X, Pos, R) :-
    NewPos is Pos + 1,
    delete_pos_even(T, X, NewPos, R), !.
delete_pos_even([H|T], X, Pos, [H|R]) :-
    NewPos is Pos + 1,
    delete_pos_even(T, X, NewPos, R).

delete_kth(L, K, R) :- delete_kth(L, K, 1, R).
delete_kth([], _, _, []).
delete_kth([H|T], K, Pos, [H|R]) :- 
    Pos < K, !, NewPos is Pos + 1,
    delete_kth(T, K, NewPos, R).
delete_kth([_|T], K, _, R) :- NewPos = 1,
    delete_kth(T, K, NewPos, R).

delete_kth_end(L, K, R) :- 
    reverse(L, Rev), 
    delete_kth(Rev, K, PR),
    reverse(PR, R).

min([H|T], M) :- min(T, M), H > M, !.
min([H|_], H).

delete_min(L, R) :- min(L, M), delete_all(L, M, R).
delete_all([X|T], X, R) :- !, delete_all(T, X, R).
delete_all([H|T], X, [H|R]) :- delete_all(T, X, R).
delete_all([], _, []).

/*
% keep the last occurence
delete_duplicates([], []).
delete_duplicates([H|T], [H|R]) :-
    \+ member(H, T), !, delete_duplicates(T, R).
delete_duplicates([_|T], R) :- delete_duplicates(T, R).
    
    % or
    
delete_duplicates([], []).
delete_duplicates([H|T], R) :-
    member(H, T), !, delete_duplicates(T, R).
delete_duplicates([H|T], [H|R]) :- delete_duplicates(T, R).
*/

% keep the first occurence
delete_duplicates(L, R) :- delete_duplicates(L, [], R).
delete_duplicates([], _, []).
delete_duplicates([H|T], Acc, [H|R]) :- 
    \+ member(H, Acc), !, NewAcc = [H|Acc],
    delete_duplicates(T, NewAcc, R).
delete_duplicates([_|T], Acc, R) :- 
    delete_duplicates(T, Acc, R).

reverse_il(L, L) :- var(L), !.
reverse_il([H|T], R) :- 
    reverse_il(T, R1), append(R1, [H|_], R), !.

reverse_k([], _, []).
reverse_k(L, K, R) :-
    length(Part1, K),
    append(Part1, Part2, L),
    reverse(Part2, Rev),
    append(Part1, Rev, R).

rle_encode([], []).
rle_encode([X], [[X, 1]]).
rle_encode([H, H|T], [[H, C]|R]) :-
    rle_encode([H|T], [[H, C1]|R]),
    C is C1 + 1, !.
rle_encode([H, H1|T], [[H,1]|R]) :- 
    rle_encode([H1|T], R).

rle_encode1(L, R) :- rle_encode(L, E),
    simplified_encoding(E, R).
simplified_encoding([], []).
simplified_encoding([[X, 1]|T], [X|R]) :- !,
    simplified_encoding(T, R).
simplified_encoding([[X, N]|T], [[X,N]|R]) :-
	simplified_encoding(T, R).

rle_decode([], []).
rle_decode([[H, 1]|T], [H|R]) :- rle_decode(T, R). 
rle_decode([[H, C]|T], [H|R]) :- C > 0, C1 is C - 1,
    rle_decode([[H, C1]|T], R), !.

    
rotate_k(L, K, R) :-
    length(L, Len),
    K1 is K mod Len,
    length(P2, K1),
    append(P1, P2, L),
    append(P2, P1, R1),
    append(R1, _, R), !.

/*
% with selection sort
sort_chars(L, [M|R]) :- min_char(L, M),
    delete_char(M, L, L1), sort_chars(L1, R).
sort_chars([], []).

min_char([H|T], M) :- min_char(T, M),
    char_code(H, HC), char_code(M, MC), HC > MC, !.
min_char([H|_], H).

delete_char(X, [X|T], T) :- !.
delete_char(X, [H|T], [H|R]) :- 
    delete_char(X, T, R).
delete_char(_, [], []).
*/

/*
% with insertion sort
sort_chars([H|T], R) :- sort_chars(T, R1),
    insert_char(H, R1, R).
sort_chars([], []).

insert_char(X, [H|T], [H|R]) :-
    char_code(X, XC), char_code(H, HC), 
    XC > HC, !, insert_char(X, T, R).
insert_char(X, T, [X|T]).
*/ 

% with quick sort
sort_chars([H|T], R) :- partition_chars(H, T, Smaller, Larger),
    sort_chars(Smaller, S), sort_chars(Larger, L),
    append(S, [H|L], R).
sort_chars([], []).

partition_chars(P, [X|T], [X|S], L) :- char_code(P, PC), 
    char_code(X, XC), PC > XC, !, 
    partition_chars(P, T, S, L).
partition_chars(P, [X|T], S, [X|L]) :- partition_chars(P, T, S, L).
partition_chars(_, [], [], []).
   
/*
% with insertion sort
sort_len([], []).
sort_len([H|T], R) :- sort_len(T, R1), insert_list(H, R1, R).

insert_list(H, [H1|T], [H1|R]) :- length(H, L),
    length(H1, L1), L > L1, !, insert_list(H, T, R).
insert_list(H, T, [H|T]).
*/

/*
% with selection sort
sort_len([], []).
sort_len(L, [M|R]) :- min_list(L, M),
    delete_list(M, L, L1), sort_len(L1, R).

min_list([H|T], M) :- min_list(T, M), length(H, L1), 
    length(M, L2), L1 > L2, !.
min_list([H|_], H).

delete_list(H, [H|T], T) :- !.
delete_list(X, [H|T], [H|R]) :- delete_list(X, T, R).
*/

% with quick sort
sort_len([], []).
sort_len([H|T], R) :- partition_len(H, T, Smaller, Larger),
    sort_len(Smaller, S), sort_len(Larger, L),
    append(S, [H|L], R).

partition_len(P, [X|T], [X|S], L) :- length(P, PL), length(X, XL),
    PL > XL, !, partition_len(P, T, S, L).
partition_len(P, [X|T], S, [X|L]) :- partition_len(P, T, S, L).
partition_len(_, [], [], []).
    
remove_dup_on_odd_pos(L, R) :- remove_dup_on_odd_pos(L, 1, R).
remove_dup_on_odd_pos([], _, []).
remove_dup_on_odd_pos([H|T], Pos, [H|R]) :-
    \+ member(H, T), 0 is Pos mod 2, !, 
    NewPos is Pos + 1, remove_dup_on_odd_pos(T, NewPos, R).
remove_dup_on_odd_pos([_|T], Pos, R) :- NewPos is Pos + 1,
    remove_dup_on_odd_pos(T, NewPos, R).

% Deep Lists

depth_list([], 1).
depth_list([H|T], D) :- atomic(H), !,
    depth_list(T, D).
depth_list([H|T], D) :-
    depth_list(H, D1), depth_list(T, D2), D3 is D1 + 1,
    max_list([D3, D2], D).
    
flatten(L, L) :- var(L), !.
flatten([H|T], [H|R]) :- atomic(H), !, flatten(T, R).
flatten([H|T], R) :- flatten(H, R1), flatten(T, R2),
    append(R1, R2, R), !.

count_lists([], 1).
count_lists([H|T], R) :- atomic(H), !, count_lists(T, R).
count_lists([H|T], R) :- count_lists(H, R1), count_lists(T, R2), R is R1 + R2.

replace_all_deep(_, _, [], []).
replace_all_deep(X, Y, [H|T], [Y|R]) :- atomic(H), H = X, !,
    replace_all_deep(X, Y, T, R).
replace_all_deep(X, Y, [H|T], [H|R]) :- atomic(H), !,
    replace_all_deep(X, Y, T, R).
replace_all_deep(X, Y, [H|T], [RH|RT]) :- 
    replace_all_deep(X, Y, H, RH),
    replace_all_deep(X, Y, T, RT).

% Trees

tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))). 
tree2(t(3, t(2, t(1, nil, nil), t(4, nil, nil)), t(5, nil, nil))). 

depth_tree(nil, 0).
depth_tree(t(_, L, R), D) :- depth_tree(L, D1), depth_tree(R, D2),
    max_list([D1, D2], D3), D is D3 + 1.
    
inorder(t(K, L, R), In) :- inorder(L, R1), inorder(R, R2), append(R1, [K|R2], In).
inorder(nil, []).

collect_k(nil, []) :- !.
collect_k(t(K, nil, nil), [K]) :- !.
collect_k(t(_, L, R), Leaves) :- collect_k(L, L1), collect_k(R, L2), append(L1, L2, Leaves).

is_bst(nil) :- !. 
is_bst(t(K, L, R)) :- all_smaller(L, K), all_greater(R, K), is_bst(L), is_bst(R).

all_smaller(nil, _).
all_smaller(t(K, L, R), X) :- K < X, all_smaller(L, X), all_smaller(R, X).

all_greater(nil, _).
all_greater(t(K, L, R), X) :- K > X, all_greater(L, X), all_greater(R, X).

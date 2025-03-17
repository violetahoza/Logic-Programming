min([H|T], M) :- min(T, M), M < H, !.
min([H|_], H).

max([H|T], M) :- max(T, M), M > H, !.
max([H|_], H).

delete_min(L, R) :- min_list(L, M), delete(L, M, R), !.
delete_max(L, R) :- max_list(L, M), delete(L, M, R), !.

del_min([], _, []).
del_min([H], H, []) :- !.
del_min([H|T], Mn, R) :- del_min(T, Mn, R1), 
    H >= Mn, !, 
    add_h(R1, Mn, H, R).
del_min([H|T], H, T).

add_h(L, Mn, H, R) :- H > Mn, !, R = [H|L].
add_h(L, _, _, L).
    
% a predicate that reverses the order of the elements of a list starting from the K-th element
reverse_k([], _, []).
reverse_k(L, 0, R) :- reverse(L, R).
reverse_k([H|T], K, [H|R]) :- K > 0, K1 is K - 1,
    reverse_k(T, K1, R).

reverse_k2(L, K, R) :- reverse_k2(L, 1, K, R).
reverse_k2([H|T], C, K, [H|R]) :- C =< K, !, C1 is C + 1,
    reverse_k2(T, C1, K, R).
reverse_k2(L, _, _, R) :- reverse(L, R).

% a predicate that encodes the elements of a list using the RLE (Runlength encoding) algorithm
% a sequence of equal and consecutive elements will be replaced with the [element, number of occurrences] pair
rle_encode([], []).
rle_encode([X], [[X, 1]]).
rle_encode([H, H|T], [[H, C]|R]) :-
    rle_encode([H|T], [[H, C1]|R]),
    C is C1 + 1, !.
rle_encode([H, H1|T], [[H,1]|R]) :- 
    rle_encode([H1|T], R).

rle_encode2([], Prev, Count, [[Prev, Count]]).
rle_encode2([H|T], Prev, Count, R) :- H = Prev, !,
    CountN is Count + 1,
    rle_encode2(T, H, CountN, R).
rle_encode2([H|T], Prev, Count, R) :- 
    rle_encode2(T, H, 1, R1),
    R = [[Prev, Count] | R1].
rle_encode2([] []).
rle_encode2([H|T], R) :- rle_encode2(T, H, 1, R).

% a predicate that rotates a list by K positions to the right
rotate_right(L, K, Result) :- length(L, Length),
	K1 is K mod Length, 
	append(LL, LF, L), 	
	length(LF, K1),
	append(LF, LL, Result), 
    !.

% a predicate that decodes the elements of a list using the RLE (Runlength encoding) algorithm
% an [element, number of occurrences] pair will be replaced with the sequence of equal and consecutive elements.
rle_decode([], []).
rle_decode([[X, 1]|T], [X|R]) :- 
    rle_decode(T, R), !.
rle_decode([[X, C]|T], [X|R]) :- 
        C > 1, C1 is C - 1, 
        rle_decode([[X, C1]|T], R), !.


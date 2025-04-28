% For complete lists
add_cl(X, [H|T], [H|R]):- add_cl(X, T, R).
add_cl(X, [], [X]).

% For difference lists
add_dl(X, LS, LE, RS, RE):- RS = LS, LE = [X|RE].
add_dl2(X,LS,[X|RE],LS,RE). % * SIMPLIFIED
append_dl(LS1, LE1, LS2,LE2, RS,RE):- RS=LS1, LE1=LS2, RE=LE2.

% For complete lists
inorder(t(K,L,R),List):-
	inorder(L,ListL),
	inorder(R,ListR),
	append(ListL,[K|ListR],List).
inorder(nil,[]).

% For difference lists
% when we reached the end of the tree we unify the beginning and end
% of the partial result list – representing an empty list as a difference list
inorder_dl(nil,L,L).
inorder_dl(t(K,L,R),LS,LE):-
	%obtain the start and end of the lists for the left and right subtrees
	inorder_dl(L,LSL,LEL),
	inorder_dl(R,LSR,LER),
	% the start of the result list is the start of the left subtree list
	LS=LSL,
	% key K is inserted between the end of left and the start of right
	LEL=[K|LSR],
	% the end of the result list is the end of the right subtree list
	LE=LER.

% *simplified 
inorder_dl2(nil,L,L).
inorder_dl2(t(K,L,R),LS,LE):-
	inorder_dl2(L,LS,[K|LT]), 
	inorder_dl2(R,LT,LE).

complete_tree(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))).
incomplete_tree(t(6, t(4,t(2,_,_),t(5,_,_)), t(9,t(7,_,_),_))).


% Write a predicate which:
% 1. Transforms a complete list into a difference list and vice versa.
% ?- convertCL2DL([1,2,3,4], LS, LE).
% LS = [1, 2, 3, 4|LE]
% ?- LS=[1,2,3,4|LE], convertDL2CL(LS,LE,R).
% R = [1, 2, 3, 4]

convertCL2DL([], LS, LS) :- !.
convertCL2DL([H|T], [H|LS], LE) :- convertCL2DL(T, LS, LE).

convertDL2CL(LS, LE, []) :- LS == LE, !.
convertDL2CL([H|T], LE, [H|R]) :- convertDL2CL(T, LE, R).


% 2. Transforms an incomplete list into a difference list and vice versa.
% ?- convertIL2DL([1,2,3,4|_], LS, LE).
% LS = [1, 2, 3, 4|LE]
% ?- LS=[1,2,3,4|LE], convertDL2IL(LS,LE,R).
% R = [1, 2, 3, 4|_]

convertIL2DL(List, LS, LS) :- var(List), !.
convertIL2DL([H|T], [H|LS], LE) :- convertIL2DL(T, LS, LE).

convertDL2IL(LS, LE, _) :- LS == LE, !.
convertDL2IL([H|T], LE, [H|R]) :- convertDL2IL(T, LE, R).

% 3. Flattens a deep list using difference lists instead of append.
% ?- flat_dl([[1], 2, [3, [4, 5]]], RS, RE).
% RS = [1, 2, 3, 4, 5|RE] ;
% false

flat_dl([], LE, LE).
flat_dl([H|T], [H|LS], LE) :-
   atomic(H), !, 
   flat_dl(T, LS, LE).
flat_dl([H|T], LS, LE) :- 
    flat_dl(H, LS, LI),
    flat_dl(T, LI, LE).

%4. Generates a list with all the possible decompositions of a list into 2 lists,
%without using the built-in predicate findall.
%?- all_decompositions([1,2,3], List).
%List=[ [[], [1,2,3]], [[1], [2,3]], [[1,2], [3]], [[1,2,3], []] ] ;
%false

%all_decompositions(L, R) :- 

% 5. Traverses a complete tree in pre-order and post-order using difference lists in an implicit manner.
% ?- complete_tree(T), preorder_dl(T, S, E).
% S = [6, 4, 2, 5, 9, 7|E]
% ?- complete_tree(T), postorder_dl(T, S, E).
% S = [2, 5, 4, 7, 9, 6|E]

preorder_dl(nil, LS, LS). 
preorder_dl(t(K,L,R), [K|LS], LE) :-
    preorder_dl(L, LS, LT),
    preorder_dl(R, LT, LE). 

postorder_dl(nil, LS, LS). 
postorder_dl(t(K,L,R), LS, LE):-
    postorder_dl(L, LS, LT), 
    postorder_dl(R, LT, [K|LE]).

% 6. Collects all even keys in a complete binary tree, using difference lists.
% ?- complete_tree(T), even_dl(T, S, E).
% S = [2, 4, 6|E]

even_dl(nil, LS, LS).
even_dl(t(K, L, R), LS, LE) :-
		0 is K mod 2, !,
		even_dl(L, LS, LT),
		even_dl(R, LT, [K|LE]).
even_dl(t(_, L, R), LS, LE) :- 
		even_dl(L, LS, LT),
		even_dl(R, LT, LE).

% 7. Collects, from a incomplete binary search tree, all keys between K1 and K2, using difference lists.
% ?- incomplete_tree(T), between_dl(T, S, E, 3, 7).
% S = [4, 5, 6|E]

between_dl(T, LS, LS, _, _) :- var(T), !.
between_dl(t(K, L, R), LS, LE, K1, K2) :-
    K > K1, K < K2, !,
    between_dl(L, LS, [K|LT], K1, K2),
    between_dl(R, LT, LE, K1, K2).
between_dl(t(_, L, R), LS, LE, K1, K2) :-
    between_dl(L, LS, LT, K1, K2),
    between_dl(R, LT, LE, K1, K2).

% 8. Collects, from a incomplete binary search tree, all keys at a given depth K using difference lists.
% ? – incomplete_tree(T), collect_depth_k(T, 2, S, E).
% S = [4, 9|E].

% collect_depth_k(t(K,L,R), S, E):- % *IMPLEMENTATION HERE*

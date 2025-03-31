count_atomic([], 0).
count_atomic([H|T], Res) :- atomic(H), !, 
    count_atomic(T, Res1),
    Res is Res1 + 1.
count_atomic([H|T], Res) :- count_atomic(H, ResH), 
    count_atomic(T, ResT), Res is ResH + ResT.

sum_atomic([], 0).
sum_atomic([H|T], Res) :- atomic(H), !, 
    sum_atomic(T, Res1),
    Res is Res1 + H.
sum_atomic([H|T], Res) :- sum_atomic(H, ResH), 
    sum_atomic(T, ResT), Res is ResH + ResT.

replace(_, _, [], []).
replace(X, Y, [X|T], [Y|Res]) :- atomic(X), !,
    replace(X, Y, T, Res).
replace(X, Y, [H|T], [H|Res]) :- atomic(H), X \= H, !,
    replace(X, Y, T, Res).
replace(X, Y, [H|T], [ResH|ResT]) :- 
    replace(X, Y, H, ResH),
    replace(X, Y, T, ResT).
    
lasts([], []).
lasts([H], [H]) :- atomic(H), !.
lasts([H|T], R) :- atomic(H), !, lasts(T, R).
lasts([H|T], R) :- lasts(H, R1), lasts(T, R2),
    append(R1, R2, R).

len_con_depth([], Cnt, [Cnt]).
len_con_depth([H|T], Cnt, R) :- atomic(H), !, 
    Cnt1 is Cnt + 1,
    len_con_depth(T, Cnt1, R).
len_con_depth([H|T], Cnt, R) :- Cnt = 0, !, 
    len_con_depth(H, 0, R1),
    len_con_depth(T, 0, R2),
    R = [R1|R2].
len_con_depth([H|T], Cnt, R) :-
    len_con_depth(H, 0, R1),
    len_con_depth(T, 0, R2),
    R = [Cnt, R1|R2].
len_con_depth(L, R) :- len_con_depth(L, 0, R).


depth_con_depth([], Cnt, [Cnt]).
depth_con_depth([H|T], Cnt, R) :- atomic(H), !, 
    depth_con_depth(T, Cnt, R).
depth_con_depth([H|T], Cnt, R) :- Cnt = 0, !,
    Cnt1 is Cnt + 1,
    depth_con_depth(H, Cnt1, R1),
    depth_con_depth(T, Cnt, R2),
    R = [R1|R2].
depth_con_depth([H|T], Cnt, R) :-
    Cnt1 is Cnt + 1,
    depth_con_depth(H, Cnt1, R1),
    depth_con_depth(T, Cnt, R2),
    R = [Cnt, R1|R2].
depth_con_depth(L, R) :- depth_con_depth(L, 0, R).

pos_dep([], _, _, []).
pos_dep([H|T], Pos, Dep, R) :- 
    atomic(H), !, 
    Pos1 is Pos + 1,
    pos_dep(T, Pos1, Dep, R1), 
    Res is Pos * Dep,
    R = [Res|R1].
pos_dep([H|T], Pos, Dep, R) :- 
    Dep1 is Dep + 1,
    Pos1 is Pos + 1,
	pos_dep(H, 1, Dep1, RH),
    pos_dep(T, Pos1, Dep, RT),
    R = [RH|RT].
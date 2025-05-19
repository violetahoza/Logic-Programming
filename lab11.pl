%edge(a,b).
%edge(a,c).
%edge(b,d).
%edge(d,e).
%edge(c,f).
%edge(e,g).
%edge(f,h).

is_edge(X,Y) :- edge(X, Y); edge(Y,X).

:- dynamic visited_node/1.
max_depth(2).

% dfs(Source, Path)
dfs(X,_) :- df_search(X). % traversal of nodes
% when traversal is finished, collection starts
dfs(_,L) :- !, collect_reverse([], L). % collecting results

% traversal predicate
df_search(X):-
    % store X as visited node
    asserta(visited_node(X)),
    % take the first edge from X to a Y
    % the rest are found through backtracking
    is_edge(X,Y),
    % check if this Y was already visited
    not(visited_node(Y)),
    % if it was not -this is why the negation is needed – 
    % then we continue the traversal by moving the current node to Y
    df_search(Y).

% collecting predicate - collecting is done in reverse order
collect_reverse(L, P):-
    % we retract each stored visited node
    retract(visited_node(X)), !, 
    % we add it to the list as the first element
    % thus, they will appear reversed
    collect_reverse([X|L], P).
    % we unify the first and second arguments, 
    % the result will be in the second argument
collect_reverse(L,L).

% dls(Source, Path) - depth-limited search
dls(X, _) :- dl_search(X, _, 0). % traversal of nodes
dls(_, L) :- !, collect_reverse2([], L). % collecting results

dl_search(X, L, Depth):-
		max_depth(MaxDepth),
		Depth =< MaxDepth,
		DepthAux is Depth + 1,
		asserta(visited_node(X)),
		edge(X, Y),
		not(visited_node(Y)),
		dl_search(Y, L, DepthAux).

collect_reverse2(L, P):-
		retract(visited_node(X)), !,
		collect_reverse2([X|L], P).
collect_reverse2(L, L).

neighbor(a, [b,c]).
neighbor(b, [a,d]).
neighbor(c, [a,e]).
neighbor(d, [b]).
neighbor(e, [c]).

bfs1(X, R) :- bfs1([X], [], P), reverse(P, R).
bfs1([], V, V).
bfs1([X|Q], V, R):-
 	\+member(X, V),
 	neighbor(X, Ns),
 	remove_visited(Ns, V, RemNs),
	append(Q, RemNs, NewQ),
 	bfs1(NewQ, [X|V], R).

remove_visited([], _, []).
remove_visited([H|T], V, [H|R]):- \+member(H, V), !, remove_visited(T, V, R).
remove_visited([_|T], V, R):- remove_visited(T, V, R).

% Edge representation
edge(a, b).
edge(a, c).
edge(b, a).
edge(b, d).
edge(c, a).
edge(c, e).
edge(d, b).
edge(e, c).

% BFS with edge representation and difference lists
bfs(Start, Path) :-
    bfs([Start], [Start], Path).

bfs([], Path, Path). % when the queue is empty, return the accumulated path
bfs([Node|Queue], Visited, Path) :- % for each node in the queue
    findall(Neighbor, % get all unvisited neighbors
           (edge(Node, Neighbor), \+member(Neighbor, Visited)), 
           Neighbors),
    append(Visited, Neighbors, NewVisited), % add all new neighbors to the end of the visited list
    append(Queue, Neighbors, NewQueue), % add all new neighbors to the end of the queue (maintaining BFS order)
    bfs(NewQueue, NewVisited, Path). % continue with the updated queue and visited list
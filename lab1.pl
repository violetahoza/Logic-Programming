woman(dorina).
woman(irina).
woman(maria).
woman(carmen).
woman(ana).
woman(sara).
woman(ema).

man(andrei).
man(george).
man(alex).
man(marius).
man(mihai).
man(sergiu).

parent(maria, ana). % maria is the parent of ana
parent(george, ana). % george is the parent of ana
parent(maria, andrei).
parent(george, andrei).
parent(marius, maria).
parent(dorina, maria).
parent(carmen, sara).
parent(alex, sara).
parent(carmen, ema).
parent(alex, ema).
parent(irina, carmen).
parent(mihai, carmen).
parent(mihai, george).
parent(irina, george).
parent(sergiu, mihai).

mother(X,Y) :- woman(X), parent(X,Y).
father(X,Y) :- man(X), parent(X,Y).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y. 
sister(X,Y) :- sibling(X,Y), woman(X). 
aunt(X,Y) :- sister(X,Z), parent(Z,Y).
brother(X,Y) :- sibling(X,Y), man(X).
uncle(X,Y) :- brother(X,Z), parent(Z,Y).
grandmother(X,Y) :- woman(X), parent(X, Z), parent(Z, Y).
grandfather(X,Y) :- man(X), parent(X, Z), parent(Z, Y).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).





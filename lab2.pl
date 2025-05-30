gcd(X, Y, Z) :- Y = 0, Z = X.
gcd(X, Y, Z) :- Y\=0, Rest is X mod Y, gcd(Y, Rest, Z).
lcm(X, Y, Z) :- gcd(X, Y, Z1), Z is (X * Y) / Z1.

triangle(A, B, C) :- A > 0, B > 0, C > 0,
    A + B > C, A + C > B, B + C > A.

solve(A, B, C, X) :- Det is B * B - 4 * A * C, 
    Det >= 0, X is (-B + sqrt(Det)) / (2 * A).
solve(A, B, C, X) :- Det is B * B - 4 * A * C, 
    Det >= 0, X is (-B - sqrt(Det)) / (2 * A).

power_bwd(_, 0, 1).
power_bwd(X, Y, Z) :- Y > 0, Y1 is Y - 1, 
    power_bwd(X, Y1, Z1), Z is X * Z1.

power_fwd(_, 0, Acc, Z) :- Z = Acc.
power_fwd(X, Y, Acc, Z) :- Y > 0, Y1 is Y - 1,
    Acc1 is Acc * X, power_fwd(X, Y1, Acc1, Z).
power_fwd(X, Y, Z) :- power_fwd(X, Y, 1, Z). 

fib_bwd(0, 0).
fib_bwd(1, 1).
fib_bwd(N, X) :- N > 1, N1 is N - 1, N2 is N - 2,
    fib_bwd(N1, X1), fib_bwd(N2, X2), 
    X is X1 + X2.

fib_bwd1(0, 0, 1).
fib_bwd1(N, F1, F2) :- N > 0, N1 is N - 1,
    fib_bwd1(N1, Z1, Z2),
    F1 = Z2, 
    F2 is Z1 + Z2.
fib_bwd1(N, F) :- fib_bwd1(N, F, _). 

fib_fwd(0, A, _, A).
fib_fwd(N, A, B, Res) :- N > 0, N1 is N - 1,
    Sum is A + B,
    fib_fwd(N1, B, Sum, Res).
fib_fwd(N, X) :- fib_fwd(N, 0, 1, X).

% calculate the sum of all numbers smaller than a givem number
for(0, 0, 0).
for(Inter, Out, In) :- In > 0, NewIn is In - 1,
    for(Inter, Intermediate, NewIn),
    Out is In + Intermediate.
    
for1(Start, 0, End) :- Start > End.
for1(Start, Sum, End) :- Start =< End,
    Start1 is Start + 1, 
    for1(Start1, S1, End), 
 	Sum is Start + S1.

for_bwd(0, 0).
for_bwd(N, Sum) :- N > 0, N1 is N - 1,
    for_bwd(N1, S1), Sum is S1 + N.
    

% calculate the sum of all values between Low and High
while_bwd(Low, High, 0) :- Low >= High.
while_bwd(Low, High, Sum) :- Low < High,
    Low1 is Low + 1,
    while_bwd(Low1, High, S1), 
    Sum is S1 + Low.

while_fwd(Low, High, Acc, Acc) :- Low >= High.
while_fwd(Low, High, Acc, Sum) :- Low < High,
    Low1 is Low + 1, Acc1 is Acc + Low,
    while_fwd(Low1, High, Acc1, Sum).
while_fwd(Low, High, Sum) :- while_fwd(Low, High, 0, Sum).
    
dowhile(Low, High, 0) :- Low >= High.
dowhile(Low, High, Sum) :- Low < High,
    High1 is High - 1,
    dowhile(Low, High1, S1),
    Sum is S1 + High.

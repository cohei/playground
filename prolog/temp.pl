square(X,Y) :- Y is X*X.

fact(0,1).
fact(X,Y) :- X>0, X1 is X-1, fact(X1,Y1), Y is X * Y1.

colour(A,B,C,D,E) :-
    map(A,B,C,D,E),
    colour(A),
    colour(B),
    colour(C),
    colour(D),
    colour(E).

colour(red).
colour(blue).
colour(green).

map(A,B,C,D,E) :-
    dif(A,B), dif(A,C), dif(A,D),
    dif(B,C), dif(B,E),
    dif(D,C), dif(D,E),
    dif(E,C).

eight_queens(Qs) :-
    length(Qs, 8),
    Qs = [(1,_),(2,_),(3,_),(4,_),(5,_),(6,_),(7,_),(8,_)],
    in_range(Qs),
    ys(Qs, Ys),
    diags1(Qs,Ds1),
    diags2(Qs,Ds2),
    fd_all_different(Ys),
    fd_all_different(Ds1),
    fd_all_different(Ds2).

in_range([]).
in_range([(X,Y)|Qs]) :-
    Range = [1,2,3,4,5,6,7,8],
    member(X, Range),
    member(Y, Range),
    in_range(Qs).

xs([], []).
xs([(X,_)|Qs], [X|Xs]) :- xs(Qs, Xs).

ys([], []).
ys([(_,Y)|Qs], [Y|Ys]) :- ys(Qs, Ys).

diags1([], []).
diags1([(X,Y)|Qs], [D|Ds]) :- D is Y - X, diags1(Qs,Ds).

diags2([], []).
diags2([(X,Y)|Qs], [D|Ds]) :- D is X + Y, diags2(Qs,Ds).

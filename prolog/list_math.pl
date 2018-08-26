count(0, []).
count(Count, [_|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

sum(0,[]).
sum(Total, [Head|Tail]) :- sum(TailTotal, Tail), Total is TailTotal + Head.

average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum / Count.

concatenate([], Xs, Xs).
concatenate([X|Xs], Ys, [X | Zs]) :- concatenate(Xs, Ys, Zs).

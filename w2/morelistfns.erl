-module(morelistfns).
-import(lists,[append/1]).
-export([double/1,evens/1,median/1,modes/1,average/1]).

double([]) -> 0;
double([X|Xs]) -> double(Xs, [X*2]).
double([], Ys) -> Ys;
double([X|Xs], Ys) -> double(Xs, lists:append(Ys,[(X*2)])).

evens([]) -> 0;
evens([X|Xs]) -> evens(lists:append(Xs,[X]), []).
evens([X|Xs],Ys) when (X rem 2 =:= 0) and (X =/= 0) -> evens(Xs, lists:append(Ys,[X]));
evens([_X|Xs],Ys) -> evens(Xs, Ys);
evens([],Ys) -> Ys.

average([]) -> 0;
average([X|Xs]) -> average(X,1,Xs).
average(Sum,Count,[]) -> Sum div Count;
average(Sum,Count,[X|Xs]) -> average(Sum+X,Count+1,Xs).

median([]) -> 0;
median([X|Xs]) -> median(lists:sort([X|Xs],length([X|Xs]))).
median(Nums,Len) when (Len rem 2 =:= 1) -> list:nth(Len div 2 + 1, Nums);
median(Nums,Len) -> average(Nums,(Len div 2),(Len div 2 + 1)).

mode([X|Xs],Y) when X =:= Y -> ,
mode([],_) -> [];

modes([]) -> 0.

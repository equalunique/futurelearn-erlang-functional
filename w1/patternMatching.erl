-module(patternMatching).
-export([maxThree/3,howManyEqual/3,test/0]).

maxThree(A,B,C) ->
	max(A, max(B, C)).

howManyEqual(x,x,x) -> 3;
howManyEqual(x,x,_) -> 2;
howManyEqual(x,_,x) -> 2;
howManyEqual(_,x,x) -> 2;
howManyEqual(_,_,_) -> 0.

test() ->
	howManyEqual(34,25,36) == 0.

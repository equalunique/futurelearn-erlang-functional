-module(fib).
-export([fib1/1,fib2/1,fib3/1,fib4/1]).

% Basic Recursion
fib1(0) -> 1;
fib1(N) when N>0 -> fib1(N-1)+N.

% Direct Recursion
fib2(0) ->
	0;
fib2(1) ->
	1;
fib2(N) when N>1 ->
	fib1(N-2) + fib1(N-1).

% Tail Recursion
fib3(0,P,_C) ->
	P;
fib3(N,P,C) ->
	fib3(N-1,C,P+C).
fib3(N) ->
	fib3(N,0,1).

% Direct definition of adjacent pairs of Fibonacci numbers
fib4P(0) ->
	{0,1};
fib4P(N) ->
	{P,C} = fib4P(N-1),
	{C,P+C}.
fib4(N) ->
	{P,_} = fib4P(N),
	P.



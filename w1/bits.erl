-module(bits).
-export([bits/1]).


bitsDirectRecursion(0,R) ->
	R;
bitsDirectRecursion(N,R) ->
	Rem = N rem 2,
	Num = N div 2,
	Val = bitsDirectRecursion(Num,Rem),
	Val + R.

bitsTailRecursion(0,R) ->
	R;
bitsTailRecursion(N,R) ->
	Rem = N rem 2,
	Num = N div 2,
	bitsTailRecursion(Num,Rem + R).

bits(N) ->
	%bitsDirectRecursion(N,0).
	bitsTailRecursion(N,0).

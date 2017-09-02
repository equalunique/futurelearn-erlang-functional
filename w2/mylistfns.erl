-module(mylistfns).
-export([product/1,product/2,maximum/1,maximum/2]).

% Combining list elements: the product of a list
%
% Using the template from the last session, define an Erlang function to give the product of a list of numbers. The product of an empty list is usually taken to be 1: why?

product([]) -> 1;
product([X|Xs]) -> product(Xs, X).
product([],P) -> P;
product([X|Xs],P) -> product(Xs, X*P).


% Combining list elements: the maximum of a list
%
% Define an Erlang function to give the maximum of a list of numbers.
%
% * You might find it helpful to use the function max/2 that gives the maximum of two values.
% * It’s not obvious what should be the value for the maximum of an empty list of numbers. You could therefore choose to define maximum on lists with at least one element only: to do this you will need to change the base case of the template.

maximum([]) -> io:format("list cannot be empty~n");
maximum([X]) -> X;
maximum([X,Y|Xs]) -> maximum(Xs, max(X,Y)).

maximum([X,Y|Xs], _M) -> maximum(Xs, max(X,Y));
maximum([X], M) -> max(X,M);
maximum([], M) -> M.



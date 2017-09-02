-module(second).
-export([rightAngleHypotenuse/2,rightAnglePerimeter/2,rightAngleArea/2]).
-import(first,[square/1,area/2]).
-import(math,[sqrt/1]).

rightAngleHypotenuse(A,B) -> math:sqrt(first:square(A) + first:square(B)).

rightAnglePerimeter(A,B) -> A + B + rightAngleHypotenuse(A,B).

rightAngleArea(A,B) -> first:area(A,B,rightAngleHypotenuse(A,B)).

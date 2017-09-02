-module(shapes).
-import(io,[format/1]).
-import(math,[sqrt/1,pi/0]).
-import(lists,[droplast/1,append/2,last/1,zip/2]).
-export([perimeter/1,area/1,enclose/1]).

% Perimeter function:
% Works on Triangles.
perimeter({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	polymeter([{X1,Y1}, {X2,Y2}, {X3,Y3}]);
% Works on Rectangles.
perimeter({rectangle, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}}) ->
	polymeter([{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}]);
% Works on Pentagons.
perimeter({pentagon, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}}) ->
	polymeter([{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}]);
% Works on *Polygons, where X is a list of X,Y tuples.
% The tuples are in "connect the dots" order.
% *Only simple convex polygons.
perimeter({polygon, X}) ->
	polymeter(X);
% Works on Circles.
perimeter({circle, {X1,Y1}, R}) ->
	2 * math:pi() * R;
% Doesn't work on anything else.
perimeter(_) ->
	io:format("not a valid shape").

% Perimeter function:
% Works on Triangles.
area({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	polyarea(0,[{X1,Y1}, {X2,Y2}, {X3,Y3}],0);
% Works on Rectangles.
area({rectangle, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}}) ->
	polyarea(0,[{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}],0);
% Works on Pentagons.
area({pentagon, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}}) ->
	polyarea(0,[{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}],0);
% Works on *Polygons, where X is a list of X,Y tuples.
% The tuples are in "connect the dots" order.
% *Only simple convex polygons.
area({polygon, X}) ->
	polyarea(0,X,0);
% Works on Circles.
area({circle, {X1,Y1}, R}) ->
	 math:pi() * pwr2(R);
% Doesn't work on anything else.
area(_) ->
	io:format("not a valid shape").

% Enclose function:
% Works on Triangles.
enclose({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	polybox([{X1,Y1}, {X2,Y2}, {X3,Y3}]);
% Works on Rectangles.
enclose({rectangle, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}}) ->
	polybox([{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}]);
% Works on Pentagons.
enclose({pentagon, {X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}}) ->
	polybox([{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, {X5,Y5}]);
% Works on *Polygons, where X is a list of X,Y tuples.
% The tuples are in "connect the dots" order.
% *Only simple convex polygons.
enclose({polygon, X}) ->
	polybox(X);
% Works on Circles.
enclose({circle, {X1,Y1}, R}) ->
	circlebox({circle, {X1,Y1}, R});
% Doesn't work on anything else.
enclose(_) ->
	io:format("not a valid shape").

% Does Erlang have a built in "power of X" operator?
pwr2(X) ->
	X*X.

% dbtp = Distance Between Two Points
dbtp({{X1,Y1},{X2,Y2}}) ->
	DX = X1 - X2,
	DY = Y1 - Y2,
	math:sqrt(pwr2(DX)+pwr2(DY)).

% Create edge representation from a list of verticies.
takeshape(Verticies1) ->
	% put last element to the front of the list & "shift other elements back one." 
	Verticies2 = lists:droplast(lists:append([lists:last(Verticies1)],Verticies1)),
	% zip arrays of verticies to get representation of edges.
	lists:zip(Verticies1,Verticies2). % Return Edges.

% Calculate area of a polygon.
polymeter(Verticies) ->
	% Make edges from Verticies.
	Edges = takeshape(Verticies),
	% list comprehension to get lengths of all edges.
	Lengths = [dbtp(Edge) || Edge <- Edges],
	% foldr to add up all of the lengths into the perimeter.
	lists:foldl(fun (Length, Sum) -> Length+Sum end, 0, Lengths).

% Heron's theorem to calculate area of triangle.
heronarea({X1,Y1},{X2,Y2},{X3,Y3}) -> 
	A = dbtp({{X1,Y1},{X2,Y2}}),
	B = dbtp({{X2,Y2},{X3,Y3}}),
	C = dbtp({{X3,Y3},{X1,Y1}}),
	% This is one way to get 1/2 the perimiter:
	S = (A+B+C)/2,
	% This alternative way is less efficent, but is a 1-liner.
	%S = polymeter([{X1,Y1},{X2,Y2},{X3,Y3}])/2.
	math:sqrt(S*(S-A)*(S-B)*(S-C)).

% Tail-recursive function for calculating area of a polygon.
polyarea(0,[A,B,C|_]=Verticies,0) ->
	polyarea(lists:nth(1,Verticies),lists:nthtail(1,Verticies),0);
polyarea(A,[B,C|_]=Verticies,Sum) ->
	% 0,1,2 , 0,2,3 , 0,3,4 , ...
	polyarea(A,lists:nthtail(1,Verticies),(Sum+heronarea(A,B,C)));
polyarea(A,_,Sum) ->
	Sum.



circlebox({circle, {X,Y}, R}) ->

	% Return rectangle.
	{rectangle, {X-R,Y-R}, {X+R,Y-R}, {X+R,Y+R}, {X-R,Y+R}}.

polybox(Verticies) ->

	% Implementation using list comprehension.
	% Perhaps this is less efficient because the min & max funcions are needed.
	MinY = lists:min([Y || {X,Y} <- Verticies]),
	MinX = lists:min([X || {X,Y} <- Verticies]),
	MaxY = lists:max([Y || {X,Y} <- Verticies]),
	MaxX = lists:max([X || {X,Y} <- Verticies]),
	
	% Implementation using foldl.
	% Perhaps this is more efficient because only one list function is needed.
	% Caveat is that it only works if both MaxX and MaxY are greater than 0.
	%MaxX = lists:foldl(fun({X,Y},Z) -> (if Z < X -> X; X < Z -> Z end) end, 0, Verticies),
	%MinX = lists:foldl(fun({X,Y},Z) -> (if Z > X -> X; X > Z -> Z end) end, MaxX, Verticies),
	%MaxY = lists:foldl(fun({X,Y},Z) -> (if Z < Y -> Y; Y < Z -> Z end) end, 0, Verticies),
	%MinY = lists:foldl(fun({X,Y},Z) -> (if Z > Y -> Y; Y > Z -> Z end) end, MaxY, Verticies),
	
	% Return rectangle.
	{rectangle, {MinX,MinY}, {MaxX,MinY}, {MaxX,MaxY}, {MinX,MaxY}}.

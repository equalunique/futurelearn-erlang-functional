-module( morelistfns ).
-import( lists, [append/1] ).
-export( [double/1, evens/1, median/1, modes/1, average/1] ).

double( [] ) ->
	0;
double( [FirstUndoubled|RemainingUndoubled] ) ->
	double( RemainingUndoubled,  [FirstUndoubled*2] ).
double( [], Doubled ) ->
	Doubled;
double( [FirstToDouble|RemainingToDouble], Doubled ) ->
	double( RemainingToDouble,  lists:append( Doubled, [( FirstToDouble*2 )] ) ).

evens( [] ) ->
	0;
evens( [FirstUnfiltered|RemainingUnfiltered] ) ->
	evens( lists:append( RemainingUnfiltered, [FirstUnfiltered] ),  [] ).
evens( [FirstUnfiltered|RemainingUnfiltered], Filtered ) when ( FirstUnfiltered rem 2 =:= 0 ) and ( FirstUnfiltered =/= 0 ) ->
	evens( RemainingUnfiltered, lists:append( Filtered, [FirstUnfiltered] ) );
evens( [_FirstUnfiltered|RemainingUnfiltered], Filtered ) ->
	evens( RemainingUnfiltered, Filtered );
evens( [], Filtered ) ->
	Filtered.

average( [] ) ->
	0;
average( [First|Remaining] ) ->
	average( First, 1, Remaining ).
average( Sum, Count, [] ) ->
	Sum div Count;
average( Sum, Count, [First|Remaining] ) -> 
	average( Sum+First, Count+1, Remaining ).

median( [] ) -> 
	0;
median( [First|Remaining] ) -> 
	median( lists:sort( [First|Remaining] ), length( [First|Remaining] ) ).
median( [Num|Nums], Len ) when ( Len rem 2 =:= 1 ) -> 
	lists:nth( (Len div 2 + 1), [Num|Nums] );
median( [Num|Nums], Len ) -> 
	average( [ lists:nth( (Len div 2), [Num|Nums] ), lists:nth( (Len div 2 + 1), [Num|Nums] ) ] ).

% personally, I feel like the modes function question in this lession is not well defined. It's easy to understand that if the list contains one number, then we are only supposed to return a list with that number. It's also easy to understand that if the list contains many numbers, then we are to return a list of numbers which appear most frequently. But what is not clear is what are we supposed to do when the list contains all unique numers? Just return the list of unique numbers? After all, those would be the numbers which occur most frequently... But they all only occur once, so that doesn't seem like quite a correct answer either, becuause this is looking for numbers which occur frequently. If they are supposed to be returned, then it seems like what this function really should do is just return a list where any duplicated numbers are de-duped, so that only unique numbers remain. This is my best guess to what should be done here, so that's how my modes/2 will work:

modes( [] ) -> % output empty list if input list is empty
	[];
modes( [X|Xs] ) when ( length(Xs) =:= 0 ) -> % output list with one element if input is list with one element
	lists:append([],[X]);
modes( [X|Xs] ) -> % When the list has more than one element, run a foldr on a list to accumulate (Acc) elements (Elem) which aren't already contained in Acc. Should give a list with only unique elements.
	lists:foldr(fun(Elem, Acc) ->
		case lists:member(Elem, Acc) of
			true -> Acc; % false -> Acc;
			false -> [Elem|Acc]
		end
	end, [], [X|Xs]).

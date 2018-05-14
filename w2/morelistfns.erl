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

mode( [], _, _ ) -> [];
mode( [First|Remaining], Num, Count ) when ( First =:= Num ) ->  
	mode( Remaining, Num, Count+1 ).

modes( [] ) -> 
	0.
% map mode/2 across list in modes/1, transforming into list of hashes (Num, Count), then return Num w/ highest Coun

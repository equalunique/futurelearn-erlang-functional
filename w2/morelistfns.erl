-module( morelistfns ).
-import( lists,[append/1] ).
-export( [double/1,evens/1,median/1,modes/1,average/1] ).

double( [] ) ->
	0;
double( [FirstUndoubled|RemainingUndoubled] ) ->
	double( RemainingUndoubled, [FirstUndoubled*2] ).
double( [], Doubled ) ->
	Doubled;
double( [FirstToDouble|RemainingToDouble], Doubled ) ->
	double( RemainingToDouble, lists:append( Doubled,[( FirstToDouble*2 )] ) ).

evens( [] ) ->
	0;
evens( [FirstUnfiltered|RemainingUnfiltered] ) ->
	evens( lists:append( RemainingUnfiltered,[FirstUnfiltered] ), [] ).
evens( [FirstUnfiltered|RemainingUnfiltered],Filtered ) when ( FirstUnfiltered rem 2 =:= 0 ) and ( FirstUnfiltered =/= 0 ) ->
	evens( RemainingUnfiltered, lists:append( Filtered,[FirstUnfiltered] ) );
evens( [_FirstUnfiltered|RemainingUnfiltered],Filtered ) ->
	evens( RemainingUnfiltered, Filtered );
evens( [],Filtered ) ->
	Filtered.

average( [] ) ->
	0;
average( [X|Xs] ) ->
	average( X,1,Xs ).
average( Sum,Count,[] ) ->
	Sum div Count;
average( Sum,Count,[X|Xs] ) -> 
	average( Sum+X,Count+1,Xs ).

median( [] ) -> 
	0;
median( [X|Xs] ) -> 
	median( lists:sort( [X|Xs],length( [X|Xs] ) ) ).
median( Nums,Len ) when ( Len rem 2 =:= 1 ) -> 
	list:nth( Len div 2 + 1, Nums );
median( Nums,Len ) -> 
	average( Nums,( Len div 2 ),( Len div 2 + 1 ) ).

%mode( [X|Xs],Y ) when X =:= Y -> ,
%mode( [],_ ) -> [];
%
modes( [] ) -> 
	0.

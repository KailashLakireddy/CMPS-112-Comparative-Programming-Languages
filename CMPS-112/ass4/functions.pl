






%% Prolog not
 
not( X ) :- X, !, fail.
not( _ ).









%% Haversine formula provided by the teacher 

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance, FlightT ) :-
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    A is sin( Dlat / 2 ) ** 2
    + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
    Distance is (Dist * 3961),
    FlightT is Distance / 500.


	
	
	
	
	
	
%% Helper function to convert egrees to radian
convert_radian( degmin( Deg, Min ), Result ) :-
    Result is (Deg + Min / 60) * pi / 180.	
	
	
	
	
	
	
	
	
%% Calculate the distance to arrival airport in hours

distance_flight_time( L, M, FlightT ) :-
    airport(L, _, LX, LY),
    airport(M, _, MX, MY),
	convert_radian( LX ,Lat1),
	convert_radian( LY ,Lon1),
	convert_radian( MX ,Lat2),
	convert_radian( MY ,Lon2),
    haversine_radians( Lat1, Lon1, Lat2, Lon2, _, FlightT ).


	
	
	
	
	
	
	
%% Store in Hour (how many hours nedded for the flight) 
%% Sore in Min (how many hours nedded for the flight)
	
convert_flight_time(L, M, Hour, Min) :-
    distance_flight_time( L, M, FlightT ),
    Temp = FlightT * 60,
    TotalMin is round(Temp),
    Min is mod(TotalMin, 60),
    Hour is div(TotalMin,60).
	



	



	
%% Helper functions used to print the time 	
format_time(Min) :-
    Min < 10,
    write(0),
    write(Min).

format_time(Min) :-
    Min >= 10,
    write(Min).



	
	
	
% Helper functions used to check two flight times 

check_time(time(H, M1), time(H, M2)) :- !, M1 < M2.
check_time(time(H1, _), time(H2, _)) :- H1 < H2.








% Helper function used to check if there is a 
% direct flight to destination

direct_flight(Start, End) :- flight(Start, End, _), !.









% Helper function used to get the  name of the city 
% having the abreciation of the airport.

get_name(A, Name) :-
    airport(A, Name, _, _),
    write(Name).

	
	
	
	
	
	
	
% Calculate the arrival time 	
	
arrival_time(L, M, time(DepH,DepM), time(ArrH,ArrM)) :-
    convert_flight_time(L, M, Hour, Min),
    ArrH is (DepH + Hour + div(Min + DepM, 60)),
    ArrM is mod(DepM + Min, 60).
	


	
	
% Calculate the	departure time
	
calculate_departure(time(ArrH,ArrM), time(NxtDepH,NxtDepM)):-
     NxtDepH is ArrH + div(ArrM + 30, 60), 
     NxtDepM is mod(ArrM + 30, 60).

	 
	 
	 
	

	
% Helper functions to create the path  

get_path( Node, End, List, Times ) :-
    flight(Node, _, Departure),
    get_path( Node, End, Departure, [Node], List, Times ).

get_path( Node, Node, _, [Node], []).

get_path( Node, End, Departure, _, 
    [Node|List], [Departure|Times] ) :-
    flight( Node, End, Departure ),
    get_path(End, End, _, List, Times).

get_path( Node, End, Departure, Tried, 
    [Node|List], [Departure|Times]) :-
    (direct_flight(Node, End)
     ->
    flight(Node, End, Departure),
    get_path(Node, End, Departure, _, List, Times)
    ;
    flight( Node, Next, Departure ),
    flight( Next, Next2, Next_Departure),
    not( member( Next, Tried )),
    not( member( Next2, Tried)),
    arrival_time(Node, Next, Departure, Arrival),
    calculate_departure(Arrival, Exp_Departure),
    (check_time(Exp_Departure, Next_Departure)
    -> 
    get_path( Next, End, Next_Departure, 
        [Next|Tried], List, Times );
    fail)
    ).




  


%  Helper function to prints arrival and departure times .

write_path( [_], [] ) :-
    nl.
write_path( [From|[To|List]], [time(H1, M1)|Times] ) :-
    flight(From,To,time(H1,M1)),
    arrival_time(From, To, time(H1, M1), time(ArrH, ArrM) ),
    write('Depart: '),
    write(From), 
    write('    '),
    get_name(From, _),
    write('    '),
    write(H1),
    write(':'),
    format_time(M1),nl,
    
    write('Arrive: '),
    write(To),
    write('    '),
    get_name(To, _),
    write('    '),
    write(ArrH),
    write(':'),
    format_time(ArrM),nl,

    write_path( [To|List], Times ).


 
 
% Main
% Check if departure and arrival are the same  

fly( Depart, Depart ) :-
    write( 'Error: Departure and arrival are the same! ' ),
    nl,
    !, fail.
	
	
	
% Find the path if exists	
	
fly(Node, End) :-
    get_path(Node, End, List, Times),
	!,nl,
	write_path(List, Times),
    true.
	
	
	
% Prints an error if flight path cannot be determined
fly( Depart, Arrive ) :-
    write( 'Error: No path from: ' ), write(Depart),
    write( ' to '), write(Arrive), write( '!' ),
    !, fail.



% Prints an error if airport cannot be found in database
fly( _, _) :-
    write( 'Error: nonexistent airports.' ), nl,
!, fail.


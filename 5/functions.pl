/*
* Function: Distance between airports.
* Tested using SWI prolog.
*/

degmin_to_deg( degmin( Degrees, Minutes ), Degreesonly ) :-
   Degreesonly is Degrees + Minutes / 60.

pythagoras( X1, Y1, X2, Y2, Hypotenuse ) :-
   DeltaX is X1 - X2,
   DeltaY is Y1 - Y2,
   Hypotenuse is sqrt( DeltaX * DeltaX + DeltaY * DeltaY ).

distance( Airport1, Airport2, DistanceMiles ) :-
   airport( Airport1, _, Latitude1, Longitude1 ),
   airport( Airport2, _, Latitude2, Longitude2 ),
   degmin_to_deg( Latitude1, Latdegrees1 ),
   degmin_to_deg( Latitude2, Latdegrees2 ),
   degmin_to_deg( Longitude1, Longdegrees1 ),
   degmin_to_deg( Longitude2, Longdegrees2 ),
   pythagoras( Latdegrees1, Longdegrees1, Latdegrees2, Longdegrees2,
               DistanceDegrees ),
   DistanceMiles is 69 * DistanceDegrees.

flight_time(Airport1, Airport2, FlightTime) :-
   distance(Airport1, Airport2, DistanceMiles),
   FlightTime is DistanceMiles / 500.

arrival_time(Airport1, Airport2, ArrivalTime) :-
   flight(Airport1, Airport2, DepartureTime),
   flight_time(Airport1, Airport2, FlightTime),
   ArrivalTime is DepartureTime + FlightTime.

hoursmins_to_hours( time( Hours, Mins ), Hoursonly ) :-
   Hoursonly is Hours + Mins / 60.

print_2digits( Digits ) :-
   Digits < 10, print( 0 ), print( Digits ).

print_2digits( Digits ) :-
   Digits >= 10, print( Digits ).

print_time( Hoursonly ) :-
   Minsonly is floor( Hoursonly * 60 ),
   Hours is Minsonly // 60,
   Mins is Minsonly mod 60,
   print_2digits( Hours ),
   print( ':' ),
   print_2digits( Mins ).

connected(X,Y) :- flight(X, Y, t).

flight_path( A, B, Path) :- 
   travel(A,B,[A],Q), 
   reverse(Q,Path).

travel(A,B,P,[B|P]) :- 
   connected(A,B).

travel(A,B,Visited,Path) :-
   connected(A,C),           
   C \== B,
   \+member(C,Visited),
   travel(C,B,[C|Visited],Path).  
   

/*
* The following is a dummy predicate which doesn't work
* and must be replaced.
*/

skate(Depart, Arrive) :-
   write('---'), nl,
   airport( Depart, Depart_name, Depart_lat, Depart_long ),
   airport( Arrive, Arrive_name, Arrive_lat, Arrive_long),
   write('depart  '), write( Depart ), write('  '), write( Depart_name ), nl,
   write('arrive  '), write( Arrive ), write('  '), write( Arrive_name ), nl.

fly( Depart, Arrive ) :-
   write( 'Depart=' ), write( Depart ), nl,
   write( 'Arrive=' ), write( Arrive ), nl,
   flight( Depart, Arrive, Time ),
   write( 'Time=' ), write( Time ), nl,
   airport( Depart, Depart_name, Depart_lat, Depart_long ),
   airport( Arrive, Arrive_name, Arrive_lat, Arrive_long ),
   write( 'Depart=' ), write( Depart ), nl,
   write( 'Depart_name=' ), write( Depart_name ), nl,
   write( 'Depart_lat=' ), write( Depart_lat ), nl,
   write( 'Depart_long=' ), write( Depart_long ), nl,
   write( 'Arrive=' ), write( Arrive ), nl,
   write( 'Arrive_name=' ), write( Arrive_name ), nl,
   write( 'Arrive_lat=' ), write( Arrive_lat ), nl,
   write( 'Arrive_long=' ), write( Arrive_long ), nl,
   nl.


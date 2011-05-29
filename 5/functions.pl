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

/*
* The following is a dummy predicate which doesn't work
* and must be replaced.
*/

fly( Depart, Arrive ) :-
   nl, nl,
   write('======================================================'), nl,
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


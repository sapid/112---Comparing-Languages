/*
* Function: Distance between airports.
* Tested using SWI prolog.
*/

not(X) :- X, !, fail.
not(_).

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
   flight(Airport1, Airport2, time(DH,DM)),
   flight_time(Airport1, Airport2, FlightTimeMins),
   hoursmins_to_hours(time(DH,DM), DepartureTime),
   mins_to_hours(FlightTimeMins, FlightTime), % Convert to hoursonly
   nl, write('Flight time is '), write(FlightTime), nl,
   ArrivalTime is DepartureTime + FlightTime.

mins_to_hours(Mins, Hours):-
   Hours is Mins / 60.

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

% Find a path.

%ispath(A1, A1). % Base case. 
%ispath(A1, A2) :- % Airport1, Airport2 
%   flight(A1, AM, _),
%   ispath(AM, A2). % Recurse.
%
%ispath(A1, A2) :- ispath2( A1, A2, [] ).

ispath2( A1, A1, _). % Base case.
ispath2( A1, A2, Path) :- % Make sure we don't repeat.
   flight( A1, AM, _),
   not(member(AM, Path)),
   ispath2(AM, A2, [A1|Path]).
writeallpaths( Node, Node ) :-
   write( Node ), write( ' is ' ), write( Node ), nl.
writeallpaths( Node, Next ) :-
   listpath( Node, Next, [Node], List ),
   write( Node ), write( ' to ' ), write( Next ), write( ' is ' ),
   writepath( List ),
   fail.

writepath( [] ) :-
   nl.
writepath( [Head|Tail] ) :-
   write( ' ' ), write( Head ), writepath( Tail ).

listpath( Node, End, Outlist ) :-
   listpath( Node, End, [Node], Outlist ).

listpath( Node, Node, _, [Node] ).
listpath( Node, End, Tried, [Node|List] ) :-
   flight( Node, Next, _ ),
   not( member( Next, Tried )),
   listpath( Next, End, [Next|Tried], List ).



% 

%connected(X,Y) :- flight(X, Y, _).
%
%flight_path( A, B, Path) :- 
%   travel(A,B,[A],Q), 
%   reverse(Q,Path).
%
%travel(A,B,P,[B|P]) :- 
%   connected(A,B).
%
%travel(A,B,Visited,Path) :-
%   connected(A,C),           
%   C \== B,
%   \+member(C,Visited),
%   travel(C,B,[C|Visited],Path).  
   
fly( Depart, Arrive ) :-
   flight( Depart, Arrive, DTimeHM ), % Departure time in (hours, mins)
   airport( Depart, Depart_name, _, _ ),
   airport( Arrive, Arrive_name, _, _),
   hoursmins_to_hours(DTimeHM, DepartTime), % Convert to hoursonly
   arrival_time(Depart,Arrive,ArrivalTime), % Arrival time in hoursonly
   write('depart  '), write( Depart ), 
      write('  '), write( Depart_name ), 
      write('  '), print_time( DepartTime),
   nl,
   write('arrive  '), write( Arrive ), 
      write('  '), write( Arrive_name ), 
      write('  '), print_time( ArrivalTime),
   nl.


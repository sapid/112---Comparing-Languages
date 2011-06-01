% Authors
%     Will Crawford <wacrawfo@ucsc.edu>
%     Ben Ross      <bpross@ucsc.edu>
%     Significant contributions from Wesley Mackey

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
   ArrivalTime is DepartureTime + FlightTime.

mins_to_hours(Mins, Hours):-
   Hours is Mins / 60.

hours_to_mins(Mins, Hours) :-
   Mins is Hours * 60.

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

writepath( [] ) :-
   true.

writepath( [Depart|List] ) :-
   writepath( Depart, List ).

writepath(_, []) :-
   true.

writepath( Depart, [Arrive|List]) :-
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
   nl,
   writepath( Arrive, List ).

sanetime(T1, T2) :-
   hoursmins_to_hours(T1, H1),
   hoursmins_to_hours(T2, H2),
   hours_to_mins(M1, H1),
   hours_to_mins(M2, H2),
   M1 + 29 < M2.

listwrapper(Node, End, Collection) :-
   % Collect lists, ensuring they aren't already members of our 
   % Find smallest of those lists.
   listpath(Node, End, List).

listpath( Node, End, Outlist ) :-
   not(Node = End), % Pre-condition: Not trying to fly to the departure airport.
   listpath( Node, End, [Node], Outlist ).

listpath( Node, Node, _, [Node] ).
listpath( Node, End, Tried, [Node|List] ) :-
   arrival_time( Node, Next, Time1 ),
   flight( Next, _, Time2 ),
   sanetime(Time1, Time2),
   not( member( Next, Tried )),
   listpath( Next, End, [Next|Tried], List ).

fly( Depart, Arrive ) :-
   listpath(Depart, Arrive, List),
   % If we didn't find a path, fail with a message.
   writepath(List).

fly( Depart, Depart ) :-
   write('Error: Departure and arrival airports are the same.'),
   !, fail.


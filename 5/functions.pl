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

arrival_time(flight(Airport1, Airport2, time(DH,DM)), ArrivalTime) :-
   flight_time(Airport1, Airport2, FlightTime),
   hoursmins_to_hours(time(DH,DM), DepartureTime),
   ArrivalTime is DepartureTime + FlightTime. % Unit is hoursonly

mins_to_hours(Mins, Hours):-
   Hours is Mins / 60.

hours_to_mins(Mins, Hours) :-
   Mins is Hours * 60.

hoursmins_to_hours( time( Hours, Mins) , Hoursonly ) :-
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

writepath( [] ).

writepath( [flight(Depart,Arrive,DTimeHM)|List]) :-
   airport( Depart, Depart_name, _, _ ),
   airport( Arrive, Arrive_name, _, _),
   hoursmins_to_hours(DTimeHM, DepartTime), % Convert to hoursonly
   arrival_time(flight(Depart,Arrive,DTimeHM), ArrivalTime), % Arrival time in hoursonly
   write('depart  '), write( Depart ), 
      write('  '), write( Depart_name ), 
      write('  '), print_time( DepartTime),
   nl,
   write('arrive  '), write( Arrive ), 
      write('  '), write( Arrive_name ), 
      write('  '), print_time( ArrivalTime),
   nl,
   writepath( List ). 

sanetime(H1, T2) :-
   hoursmins_to_hours(T2, H2),
   hours_to_mins(M1, H1),
   hours_to_mins(M2, H2),
   M1 + 29 < M2.

sanearrival(flight(Dep,Arriv,DepTime)) :-
   arrival_time(flight(Dep,Arriv,DepTime), ArrivTime),
   ArrivTime < 24.

listpath( Node, End, [flight(Node, Next, NDep)|Outlist] ) :-
   % Pre-condition: Not trying to fly to the departure airport.
   not(Node = End), 
   flight(Node, Next, NDep),
   listpath( Next, End, [flight(Node, Next, NDep)], Outlist ).

listpath( Node, Node, _, [] ).
listpath( Node, End,
   [flight(PDep,PArr,PDepTime)|Tried], 
   [flight(Node, Next, NDep)|List] ) :-
   flight(Node, Next, NDep), % Find a potential flight.
   %write('  Testing: '), write(Node), write(' '), write(Next),nl,
   arrival_time(flight(PDep,PArr,PDepTime), PArriv), % Get PrevArrivalTime.
   sanetime(PArriv, NDep), % Is this transfer possible?
   sanearrival(flight(Node,Next,NDep)),
   Tried2 = append([flight(PDep,PArr,PDepTime)], Tried),
   not( member( Next, Tried2 )), % Is this flight in our path already?
   not(Next = PArr),
   % We found a potential flight. Add it to the list.
   %write('Flight found: '), write(Node), write(' '), write(Next), nl,
   %write('  '), write(Tried), nl,
   listpath( Next, End, 
   [flight(Node, Next, NDep)|Tried2], 
      List ). 

traveltime([flight(Dep, Arr, DTimeHM)|List], Length) :-
   length(List, 0),
   hoursmins_to_hours(DTimeHM,DTimeH), 
   arrival_time(flight(Dep, Arr, DTimeHM), ArrivalTime),
   Length is ArrivalTime - DTimeH.

traveltime([flight(Dep, Arr, DTimeHM)|List], Length) :-
   length(List, L),
   L > 0,
   traveltime(flight(Dep, Arr, DTimeHM), List, Length).
   

traveltime(flight(_, _, DTimeHM), [Head|List], Length) :-
   length(List, 0),
   hoursmins_to_hours(DTimeHM, DTimeH),
   arrival_time(Head, ArrivalTime),
   Length is ArrivalTime - DTimeH.

traveltime(flight(Dep, Arr, DTimeHM), [_|List], Length) :-
   length(List, L),
   L > 0,
   traveltime(flight(Dep, Arr, DTimeHM), List, Length).
   

shortest(Depart, Arrive, List) :-
   listpath(Depart, Arrive, List),
   noshorter(Depart, Arrive, List).

noshorter(Depart, Arrive, List) :-
   listpath(Depart, Arrive, List2),
   traveltime(List, Length1),
   traveltime(List2, Length2),
   Length1 > Length2,
   !, fail.

noshorter(_, _, _).


fly( Depart, Arrive ) :-
   shortest(Depart, Arrive, List),
   %listpath(Depart, Arrive, List),
   % If we didn't find a path, fail with a message.
   nl,
   writepath(List),!.

fly( Depart, Depart ) :-
   write('Error: Departure and arrival airports are the same.'),
   !, fail.

fly( Depart, _ ) :-
   \+ airport(Depart, _, _, _),
   write('Departure airport was invalid.'),
   !, fail.

fly( _, Arrive ) :-
   \+ airport(Arrive, _, _, _),
   write('Arrival airport was invalid.'),
   !, fail.

fly( Depart, Arrive ) :- 
   \+shortest(Depart, Arrive, _),
   write('Error: Did not find a valid itinerary.'),
   !, fail.
   


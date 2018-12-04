%Wumpus World

%Example 4x4
%4|   | b |   |   |
%3| b | P |bGg|   |
%2|   | bs| W | s |
%1|   |   | s |   |
%   1   2   3   4
%P = Pit, W = Wumpus, G = Gold, b = breeze, s = stench, g = glitter

%Every time we move into a square, assert(Parameters):
%    Parameters =
%    safe([X, Y])
%    hasBreeze([X, Y]) or noBreeze([X, Y]), depending on case.
%    hasStench([X, Y]) or noStench([X, Y]), depending on case.
%
%    Then you have access to inquiring the following...

% Pass in [X, Y] coords of a square -returns TRUE if it's a GUARANTEED
% safe square. Return of false can mean unsafe or unknown.
safe([TRUE,TRUE]).
safeToTravel([X, Y]) :-
    safe([X,Y])
    ;A is X-1, noPit([A, Y]), noWumpus([A, Y])
    ;A is X+1, noPit([A, Y]), noWumpus([A, Y])
    ;A is Y-1, noPit([A, Y]), noWumpus([X, A])
    ;A is Y+1, noPit([A, Y]), noWumpus([X, A]).

% Pass in [X, Y] coords of a square -returns TRUE if it's a GUARANTEED
% noWumpus square. Return of false can mean Wumpus or unknown.

noBreeze([TRUE,TRUE]).
noStench([TRUE,TRUE]).
hasBreeze([TRUE,TRUE]).
hasStench([TRUE,TRUE]).

noWumpus([X,Y]):-
    A is X-1, noStench([A,Y])
    ;A is X+1, noStench([A,Y])
    ;A is Y-1, noStench([X,A])
    ;A is Y+1, noStench([X,A])
    ;noStench([X,Y])
    ;safe([X,Y]).

% Pass in [X, Y] coords of a square -returns TRUE if it's a GUARANTEED
% Wumpus square. Return of false can mean noWumpus or unknown.
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    A is X-1, C is Y-1,
    hasStench([A,Y]), hasStench([X,C]), noWumpus([A,C]).
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    B is X+1, C is Y-1,
    hasStench([B,Y]), hasStench([X,C]), noWumpus([B,C]).
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    B is X+1, D is Y+1,
    hasStench([B,Y]), hasStench([X,D]), noWumpus([B,D]).
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    A is X-1, D is Y+1,
    hasStench([A,Y]), hasStench([X,D]), noWumpus([A,D]).
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    A is X-1, B is X+1,
    hasStench([A,Y]), hasStench([B,Y]).
hasWumpus([X,Y]):-
    not(noWumpus([X,Y])),
    C is Y-1, D is Y+1,
    hasStench([X,C]), hasStench([X,D]).

% Pass in [X, Y] coords of a square -returns TRUE if it's a GUARANTEED
% noPit square. Return of false can mean Pit or unknown.
noPit([X,Y]):-
    A is X-1, noBreeze([A,Y]);
    A is X+1, noBreeze([A,Y]);
    A is Y-1, noBreeze([X,A]);
    A is Y+1, noBreeze([X,A]);
    noBreeze([X,Y]);
    safe([X,Y]).

% Pass in [X, Y] coords of a square -returns TRUE if it's a GUARANTEED
% hasPit square. Return of false can mean noPit or unknown.
hasPit([X, Y]) :-
    (X =:= 1),
    B is X+1,
    C is Y-1,
    D is Y+1,
    hasBreeze([X, C]),
    hasBreeze([X, D]),
    hasBreeze([B, Y]),
    safe([B, C]),
    safe([B, D])

    ;(Y =:= 1),
    A is X-1,
    B is X+1,
    D is Y+1,
    hasBreeze([A, Y]),
    hasBreeze([X, D]),
    hasBreeze([B, Y]),
    safe([A, D]),
    safe([B, D]).

% Once we know the size of the cave (N), we have the ability to
% calculate a few additional hasPit situations. Pass in [X, Y] coords of
% a square and caveSize N -returns TRUE if it's a GUARANTEED hasPit
% square. Return of false can mean noPit or unknown.
hasPitN([X, Y], N) :-
    (X =:= 1, Y =:= N),
    B is X+1,
    C is Y-1,
    hasBreeze([X, C]),
    hasBreeze([B, Y]),
    safe([B, C])

    ;(X =:= N, Y =:= 1),
    A is X-1,
    D is Y+1,
    hasBreeze([A, Y]),
    hasBreeze([X, D]),
    safe([A, D])

    ;(X =:= N, Y =:= N),
    A is X-1,
    C is Y-1,
    hasBreeze([X, C]),
    hasBreeze([A, Y]),
    safe([A, C])

    ;(X =:= 1),
    B is X+1,
    C is Y-1,
    D is Y+1,
    hasBreeze([X, C]),
    hasBreeze([X, D]),
    hasBreeze([B, Y]),
    safe([B, C]),
    safe([B, D])

    ;(Y =:= 1),
    A is X-1,
    B is X+1,
    D is Y+1,
    hasBreeze([A, Y]),
    hasBreeze([X, D]),
    hasBreeze([B, Y]),
    safe([A, D]),
    safe([B, D])

    ;(X =:= N),
    A is X-1,
    C is Y-1,
    D is Y+1,
    hasBreeze([X, C]),
    hasBreeze([A, Y]),
    hasBreeze([X, D]),
    safe([A, C]),
    safe([A, D])

    ;(Y =:= N),
    A is X-1,
    B is X+1,
    C is Y-1,
    hasBreeze([X, C]),
    hasBreeze([A, Y]),
    hasBreeze([B, Y]),
    safe([B, C]),
    safe([B, C]).








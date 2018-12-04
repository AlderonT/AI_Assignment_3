% assert noWumpus
not(P) :-
    call(P),
    !,
    fail.
not(_).

%room (pos(x,y),noBreeze/breeze, noStench/stench, noGlitter/glitter, noWumpus/wumpus, noPit/pit, noGold/gold,seen)
room(pos(0,0),noBreeze,noStench,noGlitter,noWumpus,noPit,noGold,seen).
%room(pos(1,0),noBreeze,noStench,noGlitter,noWumpus,noPit,noGold,seen).
%room(pos(2,0),noBreeze,stench,noGlitter,noWumpus,noPit,noGold,seen).
wumpus(P):-
    room(P,_,_,_,hasWumpus,_,_,_).
wumpus(P):-
    hasWumpus(P).
foundWumpus:-
    wumpus(P),
    !,
    asserta(room(P,_,_,_,hasWumpus,_,_,_)).
wall(pos(true,true)).
safe(pos(0,0)).
safe(P):-
    room(P,_,_,_,_,_,_,seen).
safe(P):-
    foundWumpus,
    not(wumpus(P)).

noWumpus(pos(X,Y)) :-
    room(pos(X1,Y),_,stench,_,_,_,_,_),
    X2 is X1-1,
    X = X2,
    not(room(pos(X,Y),_,_,_,_,_,_,seen)),
    !,
    fail.
noWumpus(pos(X,Y)) :-
    room(pos(X1,Y),_,stench,_,_,_,_,_),
    X2 is X1+1,
    X = X2,
    not(room(pos(X,Y),_,_,_,_,_,_,seen)),
    !,
    fail.
noWumpus(pos(X,Y)) :-
    room(pos(X,Y1),_,stench,_,_,_,_,_),
    Y2 is Y1-1,
    Y = Y2,
    not(room(pos(X,Y),_,_,_,_,_,_,seen)),
    !,
    fail.
noWumpus(pos(X,Y)) :-
    room(pos(X,Y1),_,stench,_,_,_,_,_),
    Y2 is Y1+1,
    Y = Y2,
    not(room(pos(X,Y),_,_,_,_,_,_,seen)),
    !,
    fail.
noWumpus(P):-   
    wall(P),
    !,
    fail.

noWumpus(P) :-
    room(P,_,_,_,noWumpus,_,_,_).

noWumpus(_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    A is X-1, C is Y-1,
    room(pos(A,Y),_,stench,_,_,_,_,_), room(pos(X,C),_,stench,_,_,_,_,_), room(pos(A,C),_,_,_,noWumpus,_,_,_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    B is X+1, C is Y-1,
    room(pos(B,Y),_,stench,_,_,_,_,_), room(pos(X,C),_,stench,_,_,_,_,_), room(pos(B,C),_,_,_,noWumpus,_,_,_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    B is X+1, D is Y+1,
    room(pos(B,Y),_,stench,_,_,_,_,_), room(pos(X,D),_,stench,_,_,_,_,_), room(pos(B,D),_,_,_,noWumpus,_,_,_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    A is X-1, D is Y+1,
    room(pos(A,Y),_,stench,_,_,_,_,_), room(pos(X,D),_,stench,_,_,_,_,_), room(pos(A,D),_,_,_,noWumpus,_,_,_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    A is X-1, B is X+1,
    room(pos(A,Y),_,stench,_,_,_,_,_), room(pos(B,Y),_,stench,_,_,_,_,_).

hasWumpus(pos(X,Y)):-
    not(noWumpus(pos(X,Y))),
    C is Y-1, D is Y+1,
    room(pos(X,C),_,stench,_,_,_,_,_), room(pos(X,D),_,stench,_,_,_,_,_).

    

possibleMoves(pos(X,Y),pos(X1,Y)) :-
    X1 is X+1,
    not(wall(pos(X1,Y))).
possibleMoves(pos(X,Y),pos(X1,Y)) :-
    X1 is X-1,
    not(wall(pos(X1,Y))).
possibleMoves(pos(X,Y),pos(X,Y1)) :-
    Y1 is Y+1,
    not(wall(pos(X,Y1))).
possibleMoves(pos(X,Y),pos(X,Y1)) :-
    Y1 is Y-1,
    not(wall(pos(X,Y1))).



safeMovesFrom(C,P) :-
    possibleMoves(C,P),
    %noPit(X,Y),
    safe(P).

%safeMoveFrom(pos(X,Y),Target) :-

%assert(room(pos(2,0),noBreeze,stench,noGlitter,noWumpus,noPit,noGold,player)).

%room(pos(3,0),noBreeze,noStench,noGlitter,noWumpus,noPit,noGold,player)

%Example 4x4
%3|   | b |   |   |
%2| b | P |bGg|   |
%1|   | bs| W | s |
%0|   |   | s |   |
%   0   1   2   3
%P = Pit, W = Wumpus, G = Gold, b = breeze, s = stench, g = glitter

%assert(room(pos(1,1),breeze,stench,noGlitter,noWumpus,noPit,noGold,seen)).
%room(P,_,stench,_,_,_,_,_).
:- module(minimaxdraw, [caseTest/3
	,evaluate_and_choose/6,minimax/5]
).

:- use_module(eval).
:- use_module(jeu).

:- dynamic caseTest/3.


evaluate_and_choose([Move|Moves], InitPlayer, Depth, MaxMin, Record, Best) :-
	move(Move, MaxMin, InitPlayer),
	minimax(Depth, InitPlayer, MaxMin, Move, Value),
	update(Move, Value, Record, Record1),
	evaluate_and_choose(Moves, InitPlayer, Depth, MaxMin, Record1, Best),
	undo_move(Move, Color).

evaluate_and_choose([], InitPlayer, Depth, MaxMin, Record, Record).

minimax(0, InitPlayer, MaxMin, Move, Value) :-
	value(InitPlayer, V),
	Value:=V*MaxMin.

minimax(Depth, InitPlayer, MaxMin, Move, Value) :-
	Depth > 0,
	findPossibleMoves(1, 8, Moves),
	%NextDepth:=Depth-1,
	%MinMax := -MaxMin,
	evaluate_and_choose(Moves, InitPlayer, Depth-1, -MaxMin, (nil, -1000), (Move, Value)).

update(Move, Value, (Move1, Value1), (Move1, Value1)) :-
	Value =< Value1.

update(Move, Value, (Move1, Value1), (Move, Value)) :-
	Value > Value1.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==jaune, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)).

move(Move, MinMax, InitPlayer) :- 
	InitPlayer=jaune, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)).

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)).

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)).

undo_move(Move, Color) :-
	calculPositionJeton(Move, 1, X),
	retract(caseTest(Move, X - 1, Color)).

value(InitPlayer, V) :-
	evalPosition(InitPlayer,Score1,1),
	V is Score1 * 1.

%%%%%%%%%%%% minimaxdraw.pl %%%%%%%%%%%%

%%% Code implémentant les algorithmes minimax et élagage alpha beta 

:- module(minimaxdraw, [
	caseTest/3,
	evaluate_and_choose/6,
	minimax/5,
	evaluate_and_choose_ab/8,
 	alpha_beta/7
]).

:- use_module(eval).
:- use_module(jeu).
:- use_module(util).

:- dynamic caseTest/3.

ponderate(MaxMin, 0, Value, Value1) :- 
	MaxMin < 0,
	Value1 is Value * MaxMin,!.

ponderate(MaxMin, 0, Value, Value) :- 
	MaxMin > 0,!.

ponderate(MaxMin, Depth, Value, Value1) :- 
	Depth > 0,
	Value1 is -Value.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     Alpha beta     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate_and_choose_ab([Move|Moves], InitPlayer, Depth, Alpha, Beta, Record, BestMove, MaxMin) :-
	move(Move, MaxMin, InitPlayer),
	alpha_beta(Depth, InitPlayer, Alpha, Beta, MoveX, Value, MaxMin),
	undo_move(Move, Color),
	ponderate(MaxMin, Depth, Value, Value1),
	cutoff(Move, Value1, Depth, Alpha, Beta , Moves ,InitPlayer, Record, BestMove, MaxMin).

evaluate_and_choose_ab([], InitPlayer, Depth, Alpha, Beta, Move, (Move, Alpha), MaxMin).


alpha_beta(0, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	value(InitPlayer, Value),
	!.

alpha_beta(Depth, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	size(Moves,L),
	L > 0,
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	NewDepth is Depth-1,
	NewMaxMin is -MaxMin,
	evaluate_and_choose_ab(Moves, InitPlayer, NewDepth, Alpha1, Beta1, nil, (Move, Value), NewMaxMin).

alpha_beta(Depth, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	size(Moves,0),
	alpha_beta(0, InitPlayer, Alpha, Beta, Move, Value, MaxMin).

size([],0).

size([H|T],L) :-
	size(T,L1),
	L is 1 + L1.


cutoff(Move,Value,Depth,Alpha,Beta,Moves, InitPlayer, Move1,(Move1,Value), MaxMin):-
	Value >= Beta.

cutoff(Move,Value,Depth,Alpha, Beta , Moves, InitPlayer, Move1,BestMove, MaxMin) :- 
	Alpha < Value,
	Value < Beta,
	evaluate_and_choose_ab(Moves, InitPlayer, Depth, Value, Beta, Move, BestMove, MaxMin).

cutoff(Move,Value,Depth,Alpha, Beta , Moves, InitPlayer, Move1,BestMove, MaxMin):- 
	Value =< Alpha, 
	evaluate_and_choose_ab(Moves, InitPlayer, Depth, Alpha, Beta, Move1, BestMove, MaxMin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Min Max classique   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


evaluate_and_choose([Move|Moves], InitPlayer, Depth, MaxMin, Record, Best) :-
	move(Move, MaxMin, InitPlayer),
	minimax(Depth, InitPlayer, MaxMin, MoveX, Value),
	update(Move, Value, Record, Record1, MaxMin),
	undo_move(Move, Color),
	evaluate_and_choose(Moves, InitPlayer, Depth, MaxMin, Record1, Best).

evaluate_and_choose([], InitPlayer, Depth, MaxMin, Record, Record).

minimax(0, InitPlayer, MaxMin, Move, Value) :-
	value(InitPlayer, V),
	Value is V.

minimax(Depth, InitPlayer, MaxMin, Move, Value) :-
	Depth > 0,
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	NewDepth is Depth-1,
	NewMaxMin is -MaxMin,
	evaluate_and_choose(Moves, InitPlayer, NewDepth, NewMaxMin, (nil, 1000*MaxMin), (Move, Value)).

update(Move, Value, (Move1, Value1), (Move1, Value1), MinMax) :-
	MinMax > 0,
	Value =< Value1,!.

update(Move, Value, (Move1, Value1), (Move, Value), MinMax) :-
	MinMax > 0,
	Value > Value1,!.

update(Move, Value, (Move1, Value1), (Move1, Value1), MinMax) :-
	MinMax < 0,
	Value > Value1,!.

update(Move, Value, (Move1, Value1), (Move, Value), MinMax) :-
	MinMax < 0,
	Value =< Value1.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==jaune, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==jaune, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)).

calculPositionJetonTest(X,YCheck,YCheck) :- 
	caseVideTest(X,YCheck), !.
calculPositionJetonTest(X,YCheck,Y) :- 
	incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

undo_move(Move, Color) :-
	calculPositionJetonTest(Move, 1, X),
	LinePos is X-1,
	retract(caseTest(Move, LinePos, Color)).

value(InitPlayer, V) :-
	evalJeu(InitPlayer,Score),
	V is Score.
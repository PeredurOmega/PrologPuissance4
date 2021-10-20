% - Emmanuel GARREAU
% - Mathis GOICHON
% - Yanis MANSOUR
% - Bérenger MAYOUD--DUPIN
% - Paul SOUTEYRAT
% - Timothé VERSTRAETE

%%%%%%%%%%%% evalNew.pl %%%%%%%%%%%%
% Fonctions d'évaluation pour le Puissance 4
% Heuristique sur le positionnement des pions dans le plateau
% Heuristique sur l'alignement des pions en vérifiant que l'on peut compléter celui-ci

:- module(evalNew, [evalAlignements3New/3, evalBlocage3New/3]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(minimaxdraw).

:- use_module(library(random)).


caseVideTestNew(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%% Nouvelles Evaluations sur l'alignement %%%


evalAlignements3New(Courant,Score,PoidsAlignement) :-
	PoidsAlignement>0,
	findall(S, evalCasesAlignements(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalAlignements3New(_,0,_).

evalCasesAlignements(Courant,ScoreCase) :-
	caseTest(X,Y,Courant),
	calculPoidsAlignements(X,Y,Courant,ScoreCase).

evalBlocage3New(Courant,Score,PoidsBlocage) :-
	PoidsBlocage>0,
	findall(S, evalCasesBlocage(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalBlocage3New(_,0,_).

evalCasesBlocage(Courant,ScoreCase) :-
	ennemi(Courant,AutreJoueur),
	caseTest(X,Y,AutreJoueur),
	calculPoidsAlignements(X,Y,AutreJoueur,Score),
	ScoreCase is Score * -1.

calculPoidsAlignements(X,Y,J,Score) :-
	gagneTestDirectLigne(X,Y,J,RgL,RdL,HgL,HdL,PgL,PdL),
	gagneTestDirectColonne(X,Y,J,RgC,RdC,HgC,HdC,PgC,PdC),
	gagneTestDirectDiag1(X,Y,J,RgD1,RdD1,HgD1,HdD1,PgD1,PdD1),
	gagneTestDirectDiag2(X,Y,J,RgD2,RdLD2,HgD2,HdD2,PgD2,PdD2),
	scoreWin(RgL,RdL,HgL,HdL,PgL,PdL,ScoreLignes),
	scoreWin(RgC,RdC,HgC,HdC,PgC,PdC,ScoreColonnes),
	scoreWin(RgD1,RdD1,HgD1,HdD1,PgD1,PdD1,ScoreDiag1),
	scoreWin(RgD2,RdLD2,HgD2,HdD2,PgD2,PdD2,ScoreDiag2),
	Score is ScoreLignes + ScoreColonnes + ScoreDiag1 + ScoreDiag2.

scoreWin(Rg,Rd,Hg,Hd,Pg,Pd,Score) :-
	findall(S, scoreWin3(Rg,Rd,Hg,Hd,Pg,Pd,S),ScoreList),
    sum(ScoreList,S3),
    scoreWin4(Rg,Rd,Hg,Hd,Pg,Pd,S4),
    (S4 > S3 -> Score = S4; Score = S3).

%o,o,o,o%
scoreWin4(Rg,Rd,_,_,_,_,Score):-
	1+Rg+Rd >= 4,
	Score = 1000,
    !.
scoreWin4(_,_,_,_,_,_,Score):-
	Score = 0.
%_,o,o,o%
scoreWin3(Rg,Rd,Hg,_,_,_,Score):-
	(Hg==1,Rd==2; Hg==1,Rg==1,Rd==1; Hg==1,Rg==2),
	Score = 100.
%o,_,o,o%
scoreWin3(Rg,Rd,_,_,Pg,_,Score):-
	(Pg==1,Rg==1; Pg==1,Rd==1),
	Score = 100.
%o,o,_,o%
scoreWin3(Rg,Rd,_,_,_,Pd,Score):-
	(Pd==1,Rd==1; Pd==1,Rg==1),
	Score = 100.
%o,o,o,_%
scoreWin3(Rg,Rd,_,Hd,_,_,Score):-
	(Hd==1,Rg==2; Hd==1,Rg==1,Rd==1; Hd==1,Rg==2),
	Score = 100.
scoreWin3(_,_,_,_,_,_,Score):-
	Score = 0.


%chaque fonction gagneTest retourne%
%Rg/Rd nb de pions à gauche/droite ou bas/haut ou diag%
%Hg/Hd case libre à gauche/doite du pion le plus à gauche/droite : 0 = faux, 1 = vrai%
%Pg/Pd pion placé dans la case après le trou après le pion le plus à gauche/droite : 0 = faux, 1 = vrai%

%%% En ligne %%%
gagneTestDirectLigne(X,Y,J,Rg,Rd,Hg,Hd,Pg,Pd) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg,Hg,Pg),
	incr(X,X2),
	droiteVerif(X2,Y,J,Rd,Hd,Pd),
	!.

gaucheVerif(X,Y,J,Rg,H,P):-
	gauche(X,Y,J,0,Rg,H,P).
gauche(X,Y,J,R,R,H,P):-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X>0, not(caseTest(X,Y,EJ)),
	H = 1,
	decr(X,X1),
	(caseTest(X1,Y,J) -> P=1;P=0).
gauche(X,Y,J,R,R,H,P):-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(X=<0;caseTest(X,Y,EJ)),
	H = 0, P = 0.
gauche(X,Y,J,R,Rg,_,_) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg,0,0).

droiteVerif(X,Y,J,Rg,H,P):-
	droite(X,Y,J,0,Rg,H,P).
droite(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X<8, not(caseTest(X,Y,EJ)),
	H = 1,
	incr(X,X1),
	(caseTest(X1,Y,J) -> P=1;P=0).
droite(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(X>=8; caseTest(X,Y,EJ)),
	H = 0, P = 0.
droite(X,Y,J,R,Rg,_,_) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg,0,0).

%%% En Colonne %%%
gagneTestDirectColonne(X,Y,J,Rb,Rh,Hb,Hh,Pb,Ph) :-
	decr(Y,Y1),
	basVerif(X,Y1,J,Rb,Hb,Pb),
	incr(Y,Y2),
	hautVerif(X,Y2,J,Rh,Hh,Ph),
	!.

basVerif(X,Y,J,Rb,H,P):-
	bas(X,Y,J,0,Rb,H,P).
bas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	Y>0, not(caseTest(X,Y,EJ)),
	H = 1,
	decr(Y,Y1),
	(caseTest(X,Y1,J) -> P=1;P=0).
bas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(Y=<0; caseTest(X,Y,EJ)),
	H = 0, P = 0.
bas(X,Y,J,R,Rb,_,_) :-
	decr(Y,Y1),
	incr(R,R1),
	bas(X,Y1,J,R1,Rb,0,0).

hautVerif(X,Y,J,Rh,H,P):-
	haut(X,Y,J,0,Rh,H,P).
haut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	Y<7, not(caseTest(X,Y,EJ)),
	H = 1,
	incr(Y,Y1),
	(caseTest(X,Y1,J) -> P=1;P=0).
haut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(Y>=7; caseTest(X,Y,EJ)),
	H = 0, P = 0.
haut(X,Y,J,R,Rh,_,_) :-
	incr(Y,Y1),
	incr(R,R1),
	haut(X,Y1,J,R1,Rh,0,0).


%%% En diagonale \ %%%
gagneTestDirectDiag1(X,Y,J,Rg,Rd,Hg,Hd,Pg,Pd) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg,Hg,Pg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd,Hd,Pd),
	!.

gaucheHautVerif(X,Y,J,Rg,H,P):-
	gaucheHaut(X,Y,J,0,Rg,H,P).
gaucheHaut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X>0, Y<7, not(caseTest(X,Y,EJ)),
	H = 1,
	incr(Y,Y1),
	decr(X,X1),
	(caseTest(X1,Y1,J) -> P=1;P=0).
gaucheHaut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)),
	ennemi(J,EJ),
	(X=<0;Y>=7;caseTest(X,Y,EJ)),
	H = 0, P = 0.
gaucheHaut(X,Y,J,R,Rg,_,_) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg,0,0).

droiteBasVerif(X,Y,J,Rg,H,P):-
	droiteBas(X,Y,J,0,Rg,H,P).
droiteBas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X<8, Y>0, not(caseTest(X,Y,EJ)),
	H = 1,
	decr(Y,Y1),
	incr(X,X1),
	(caseTest(X1,Y1,J) -> P=1;P=0).
droiteBas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(X>=8;Y=<0;caseTest(X,Y,EJ)),
	H = 0, P = 0.
droiteBas(X,Y,J,R,Rg,_,_) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg,0,0).

%%% En diagonale / %%%
gagneTestDirectDiag2(X,Y,J,Rg,Rd,Hg,Hd,Pg,Pd) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg,Hg,Pg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd,Hd,Pd),
	!.

gaucheBasVerif(X,Y,J,Rg,H,P) :-
	gaucheBas(X,Y,J,0,Rg,H,P).
gaucheBas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X>0, Y>0, not(caseTest(X,Y,EJ)),
	H = 1,
	decr(Y,Y1),
	decr(X,X1),
	(caseTest(X1,Y1,J) -> P=1;P=0).
gaucheBas(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(X=<0;Y=<0;caseTest(X,Y,EJ)),
	H = 0, P = 0.
gaucheBas(X,Y,J,R,Rg,_,_) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg,0,0).

droiteHautVerif(X,Y,J,Rg,H,P) :-
	droiteHaut(X,Y,J,0,Rg,H,P).
droiteHaut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	X<8, Y<7, not(caseTest(X,Y,EJ)),
	H = 1,
	incr(Y,Y1),
	incr(X,X1),
	(caseTest(X1,Y1,J) -> P=1;P=0).
droiteHaut(X,Y,J,R,R,H,P) :-
	not(caseTest(X,Y,J)), 
	ennemi(J,EJ),
	(X>=8;Y>=7;caseTest(X,Y,EJ)),
	H = 0, P = 0.
droiteHaut(X,Y,J,R,Rg,_,_) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg,0,0).
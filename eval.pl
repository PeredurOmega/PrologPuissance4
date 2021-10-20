% - Emmanuel GARREAU
% - Mathis GOICHON
% - Yanis MANSOUR
% - Bérenger MAYOUD--DUPIN
% - Paul SOUTEYRAT
% - Timothé VERSTRAETE

%%%%%%%%%%%% eval.pl %%%%%%%%%%%%
% Fonctions d'évaluation pour le Puissance 4
% Heuristique sur le positionnement des pions dans le plateau
% Heuristique sur l'alignement des pions, sans vérifier que le gain est encore possible
% 				exemple : J R R R J comptabilisé alors qu'il n'apporte rien

:- module(eval, [caseVideTest/2, evalJeu/2]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(minimaxdraw).
:- use_module(evalNew).

:- use_module(library(random)).


caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%%%%%%%%% Attribution de poids en fonction de la position %%%%%%%%%%%
caseTableauAlignements(1,1,3).
caseTableauAlignements(2,1,4).
caseTableauAlignements(3,1,5).
caseTableauAlignements(4,1,7).
caseTableauAlignements(5,1,5).
caseTableauAlignements(6,1,4).
caseTableauAlignements(7,1,3).

caseTableauAlignements(1,2,4).
caseTableauAlignements(2,2,6).
caseTableauAlignements(3,2,8).
caseTableauAlignements(4,2,10).
caseTableauAlignements(5,2,8).
caseTableauAlignements(6,2,6).
caseTableauAlignements(7,2,4).

caseTableauAlignements(1,3,5).
caseTableauAlignements(2,3,8).
caseTableauAlignements(3,3,11).
caseTableauAlignements(4,3,13).
caseTableauAlignements(5,3,11).
caseTableauAlignements(6,3,8).
caseTableauAlignements(7,3,5).

caseTableauAlignements(1,4,5).
caseTableauAlignements(2,4,8).
caseTableauAlignements(3,4,11).
caseTableauAlignements(4,4,13).
caseTableauAlignements(5,4,11).
caseTableauAlignements(6,4,8).
caseTableauAlignements(7,4,5).

caseTableauAlignements(1,5,4).
caseTableauAlignements(2,4,6).
caseTableauAlignements(3,5,8).
caseTableauAlignements(4,5,10).
caseTableauAlignements(5,5,8).
caseTableauAlignements(6,5,6).
caseTableauAlignements(7,5,4).

caseTableauAlignements(1,6,3).
caseTableauAlignements(2,6,4).
caseTableauAlignements(3,6,5).
caseTableauAlignements(4,6,7).
caseTableauAlignements(5,6,5).
caseTableauAlignements(6,6,4).
caseTableauAlignements(7,6,3).


% Prédicat qui permet d'évaluer l'état à l'instant t du plateau
% Courant est la couleur du joueur courant
% evalJeu(+Courant, -ScoreFinal)
evalJeu(Courant,ScoreFinal) :-
	poidsPosition(PoidsPosition),
	poidsAlignement(PoidsAlignement),
	poidsBlocage(PoidsBlocage),
	poidsAlignementNew(PoidsAlignementNew),
	poidsBlocageNew(PoidsBlocageNew),
	evalPosition(Courant,ScorePosition,PoidsPosition),
	evalNbAlignements(Courant,ScoreAlignement,PoidsAlignement),
	evalBlocage(Courant,ScoreBlocage,PoidsBlocage),
	evalAlignements3New(Courant,ScoreAlignementNew,PoidsAlignementNew),
	evalBlocage3New(Courant,ScoreBlocageNew,PoidsBlocageNew),
	Score is ScorePosition * PoidsPosition 
		+ ScoreAlignement * PoidsAlignement
		+ ScoreBlocage * PoidsBlocage
		+ ScoreAlignementNew * PoidsAlignementNew
		+ ScoreBlocageNew * PoidsBlocageNew,
	random(1,10,Percentage),
	random(-1,1,Advantage),
	perturbation(Score,Percentage,Advantage,ScoreFinal).

% Prédicat qui ajoute une perturbation au score final d'un coup
% pertubation(+Score, +Percentage, +Advantage, -ScoreFinal)
perturbation(Score,Percentage,Advantage,ScoreFinal) :-
	Advantage > 0,
	ScoreFinal is Score *(1 + Percentage/100). 

perturbation(Score,Percentage,Advantage,ScoreFinal) :-
	Advantage =< 0,
	ScoreFinal is Score *(1 - Percentage/100).

% Evalue le placement de l'ensemble des pions du plateau 
% evalPosition(+Courant,-Score,+PoidsPosition) 
evalPosition(Courant,Score,PoidsPosition) :-
	PoidsPosition>0,
	findall(S, evalCasesPosition(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalPosition(_,0,_).

% Evalue la position d'un pion dans le plateau
% evalCasesPosition(+Courant,-ScoreCase) 
evalCasesPosition(Courant,ScoreCase) :-
	caseTest(X,Y,Couleur),
	evalCaseNew(X,Y,Courant,Couleur,ScoreCase).

% Attribue un poids à un jeton en fonction de sa couleur
% et de son placement dans le plateau
% evalCaseNew(+X,+Y,+Courant,+Couleur,-ScoreCase)
evalCaseNew(X,Y,Courant,Couleur,ScoreCase) :-
	Couleur == Courant,
	caseTableauAlignements(X,Y,Poids),
	ScoreCase is Poids.

evalCaseNew(X,Y,Courant,Couleur,ScoreCase) :-
	Couleur \== Courant,
	caseTableauAlignements(X,Y,Poids),
	ScoreCase is Poids * -1.

% Evalue l'ensemble des alignements des pions de couleur Courant
% evalNbAlignements(+Courant,-Score,+PoidsAlignement)
evalNbAlignements(Courant,Score,PoidsAlignement) :-
	PoidsAlignement>0,
	findall(S, evalCasesAlignements(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalNbAlignements(_,0,_).

% evalCasesAlignements(+Courant,-ScoreCase)
evalCasesAlignements(Courant,ScoreCase) :-
	caseTest(X,Y,Courant),
	calculPoidsAlignements(X,Y,Courant,ScoreCase).

% Evalue l'ensemble des alignements des pions de couleur opposé à Courant
% evalNbAlignements(+Courant,-Score,+PoidsAlignement)
evalBlocage(Courant,Score,PoidsBlocage) :-
	PoidsBlocage>0,
	findall(S, evalCasesBlocage(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalBlocage(_,0,_).

% evalCasesBlocage(+Courant,-ScoreCase)
evalCasesBlocage(Courant,ScoreCase) :-
	ennemi(Courant,AutreJoueur),
	caseTest(X,Y,AutreJoueur),
	calculPoidsAlignements(X,Y,AutreJoueur,Score),
	ScoreCase is Score * -1.

% Evalue l'alignement d'un pion dans le plateau
% calculPoidsAlignements(+X,+Y,+J,-Score)
calculPoidsAlignements(X,Y,J,Score) :-
	gagneTestDirectLigne(X,Y,J,RfLignes),
	gagneTestDirectDiag1(X,Y,J,RfDiag1),
	gagneTestDirectDiag2(X,Y,J,RfDiag2),
	gagneTestDirectColonne(X,Y,J,RfColonnes),
	scoreAlignement(RfLignes,ScoreLignes),
	scoreAlignement(RfDiag1,ScoreDiag1),
	scoreAlignement(RfDiag2,ScoreDiag2),
	scoreAlignement(RfColonnes,ScoreColonnes),
	Score is ScoreLignes + ScoreColonnes + ScoreDiag1 + ScoreDiag2.

% Calcule un score en fonction du nombre de pions alignés
% scoreAlignement(+NbAlignes,-Score)
scoreAlignement(NbAlignes,Score) :-
	NbAlignes == 1,
	Score is 0.
scoreAlignement(NbAlignes,Score) :-
	NbAlignes == 2,
	Score is 10.
scoreAlignement(NbAlignes,Score) :-
	NbAlignes == 3,
	Score is 100.
scoreAlignement(NbAlignes,Score) :-
	NbAlignes == 4,
	Score is 1000.


%%%%% Prédicats suivants calcule les alignements d'un pion donné

%%% En ligne %%%
gagneTestDirectLigne(X,Y,J, Rf) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerif(X2,Y,J,Rd),
	!,
	Rf is Rg+Rd+1.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En Colonne %%%
gagneTestDirectColonne(X,Y,J,Rf) :-
	decr(Y,Y1),
	basVerif(X,Y1,J,Rb),
	incr(Y,Y2),
	hautVerif(X,Y2,J,Rh),
	!,
	Rf is Rh+Rb+1.

basVerif(X,Y,J,Rb):-
	bas(X,Y,J,0,Rb).
bas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
bas(X,Y,J,R,Rb) :-
	decr(Y,Y1),
	incr(R,R1),
	bas(X,Y1,J,R1,Rb).

hautVerif(X,Y,J,Rh):-
	haut(X,Y,J,0,Rh).
haut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
haut(_,Y,J,R,Rh) :-
	incr(Y,Y1),
	incr(R,R1),
	haut(x,Y1,J,R1,Rh).


%%% En diagonale paire %%%
gagneTestDirectDiag1(X,Y,J,Rf) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd+1.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).


%%% En diagonale impaire %%%
gagneTestDirectDiag2(X,Y,J,Rf) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd+1.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)).
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). 
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).

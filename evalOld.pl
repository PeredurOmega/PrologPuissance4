% - Emmanuel GARREAU
% - Mathis GOICHON
% - Yanis MANSOUR
% - Bérenger MAYOUD--DUPIN
% - Paul SOUTEYRAT
% - Timothé VERSTRAETE

%%%%%%%%%%%% evalOld.pl %%%%%%%%%%%%

%%% Ancien code basé sur la source : https://github.com/SIGSWAG/PrologPuissance4 %%%
%%% Code non utilisé dans notre solution sauf pour jouer contre afin de tester nos IA %%%

% Différentes fonctions d'évaluation pour le Puissance 4, toutes basées sur des heuristiques différentes.

:- module(evalOld, [evalJeuOld/5, caseVideTestOld/2]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(miniMax).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeuOld/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Évalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y). Le score est pondéré par les différentes pondérations données en entrée (par assert) à evalJeuOld. Le score est ensuite perturbé par une valeur aléatoire, permettant de casser le caractère déterministe de l'IA.
% Score s'unifie avec le score évalué pour la position courante.
evalJeuOld(JoueurCourant,AutreJoueur,X,Y,Score) :-
	assert(caseTestOld(X,Y,JoueurCourant)),
	assert(ennemiTest(AutreJoueur)),
	poidsPuissance3(PoidsPuissance3), poidsPosition(PoidsPosition), poidsDensite(PoidsDensite), poidsAdjacence(PoidsAdjacence),
	evalPositionOld(JoueurCourant,Score1,PoidsPosition),
	evalPuissances3Old(JoueurCourant,AutreJoueur,Score2,PoidsPuissance3),
	densiteOld(JoueurCourant,Score3,PoidsDensite),
	evalAdjacenceOld(X,Y,_,Score4, PoidsAdjacence),
	retract(caseTestOld(X,Y,JoueurCourant)),
	retract(ennemiTest(AutreJoueur)),
	random_between(-2,2,Perturbation),
	Score is Score1 * PoidsPosition
			+ Score2 * PoidsPuissance3
			+ Score3
			+ Score4
			+ Perturbation.

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% evalPositionOld/3(+Courant,-Score,+PoidsPosition)
% Évalue en privilégiant les positions centrales en fonction de la pondération.
% Score s'unifie à une valeur entre -400 et 400.
evalPositionOld(Courant,Score,PoidsPosition) :-
	PoidsPosition>0,
	assert(nbCasesPleines(0)),
	findall(S, evalCasesOld(Courant,S), Scores),
	sum(Scores, ScoreTot),
	nbCasesPleines(NbCasesPleinesFinal),
	retract(nbCasesPleines(NbCasesPleinesFinal)),
	Score is ScoreTot / (NbCasesPleinesFinal+1).
evalPositionOld(_,0,_).

evalCasesOld(Courant,ScoreCase) :-
	caseTestOld(X,Y,_),
	nbCasesPleines(NbCasesPleines),
	retract(nbCasesPleines(NbCasesPleines)),
	incr(NbCasesPleines,NbCasesPleinesF),
	assert(nbCasesPleines(NbCasesPleinesF)),
	evalCaseOld(X,Y,Courant,ScoreCase).

% renvoie un score entre -400 et 400
evalCaseOld(X,Y,Courant,ScoreCase) :-
	nbColonnes(NBCOLONNES),
	nbLignes(NBLIGNES),
	ponderationJ(X, Y, Courant, PonderationJoueur),
	CentreX is NBCOLONNES // 2 + 1,
	CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX,
	Dy is Y - CentreY,
	abs(Dx,AbsX),
	abs(Dy,AbsY),
	ScoreCase is ( 200/(AbsX+1) + 200/(AbsY+1) )*PonderationJoueur.

ponderationJ(X,Y, Courant,1) :-
	caseTestOld(X,Y,Courant), !.
ponderationJ(X,Y,_,-1) :-
	ennemiTest(J),
	caseTestOld(X,Y,J), !.
ponderationJ(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3Old/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s'unifie au score de la position.
evalPuissances3Old(JoueurCourant,AutreJoueur,ScoreFinal,PoidsPuissance3) :-
	PoidsPuissance3>0,
	findall(S,evalCasesOldVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesOldVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.
evalPuissances3Old(_,_,0,_).

evalCasesOldVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseTestOld(X,Y,Joueur),
	incr(X,X1),
	decr(X,X2),
	incr(Y,Y1),
	decr(Y,Y2),
	caseVideTestOld(X1,Y1),
	caseVideTestOld(X2,Y1),
	caseTestOld(X2,Y2,_),
	caseTestOld(X1,Y2,_),
	(gagneTestDirectOld(X1,Y1,Joueur) -> ScoreCase1=100 ; ScoreCase1=0),
	(gagneTestDirectOld(X2,Y1,Joueur) -> ScoreCase2=100 ; ScoreCase2=0),
	ScoreCase is ScoreCase1+ScoreCase2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR ADJACENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalAdjacenceOld/5(+X,+Y,+Joueur,-Note,+PoidsAdjacence)
% Donne une note d'autant plus forte qu'un pion est entouré de pions amis.
% Note s'unifie au score de la position.

evalAdjacenceOld(X,Y,Joueur,Note,PoidsAdjacence) :-
	PoidsAdjacence>0,
	aggregate_all(count,caseAdjacente(X,Y,Joueur,_,_),N),
	pow(N,2,Note).
evalAdjacenceOld(_,_,_,0,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR DENSITE DE PION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% densiteOld/3(+Joueur,-Note,+PoidsDensite)
% Donne une note d'autant plus élevée que les pions sont groupés.
% Note s'unifie au score de la position.
densiteOld(J,Note,PoidsDensite) :- PoidsDensite>0, Z is 1, calculNbPointsOld(J,Z,Note).
densiteOld(_,0,_).
calculNbPointsOld(_,Z,Note) :- Z>6, Note is 0.
calculNbPointsOld(J,Z,Note) :- nbPointsZoneOld(J,Z,N), incr(Z,ZP), calculNbPointsOld(J,ZP,NP), Note is N+NP.
nbPointsZoneOld(J,Z,NbPoints) :- nbPionsZoneOld(J,Z,N), pow(N,2,NbPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nbPionsZoneOld/3(+Joueur,+Zone,-NbPions)
% Donne le nombre de pions contenu dans une zone.
% NbPions s'unifie au nombre de pions contenu dans une zone.
nbPionsZoneOld(J,Z,NbPions) :-
	aggregate_all(count,caseTestZoneOld(Z,J,_,_),NbPions).

caseTestZoneOld(Zone,Joueur,X,Y) :- caseTestOld(X,Y,Joueur), zone(Zone,X,Y).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.



%%%%% gagneTestDirectOld %%%%%


gagneTestDirectOld(X,Y,J) :-
	gagneTestDirectLigneOld(X,Y,J).
gagneTestDirectOld(X,Y,J) :-
	gagneTestDirectDiag1Old(X,Y,J).
gagneTestDirectOld(X,Y,J) :-
	gagneTestDirectDiag2Old(X,Y,J).


%%% En ligne %%%

gagneTestDirectLigneOld(X,Y,J) :-
	decr(X,X1),
	gaucheVerifOld(X1,Y,J,Rg),
	incr(X,_),
	droiteVerifOld(X,_,J,Rd),
	!,
	Rf is Rg+Rd, Rf>2.

gaucheVerifOld(X,Y,J,Rg):-
	gaucheOld(X,Y,J,0,Rg).
gaucheOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
gaucheOld(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gaucheOld(X1,Y,J,R1,Rg).

droiteVerifOld(X,Y,J,Rg):-
	droiteOld(X,Y,J,0,Rg).
droiteOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
droiteOld(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droiteOld(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDirectDiag1Old(X,Y,J) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerifOld(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerifOld(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerifOld(X,Y,J,Rg):-
	gaucheHautOld(X,Y,J,0,Rg).
gaucheHautOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
gaucheHautOld(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHautOld(X1,Y1,J,R1,Rg).

droiteBasVerifOld(X,Y,J,Rg):-
	droiteBasOld(X,Y,J,0,Rg).
droiteBasOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
droiteBasOld(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBasOld(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDirectDiag2Old(X,Y,J) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerifOld(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerifOld(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerifOld(X,Y,J,Rg) :-
	gaucheBasOld(X,Y,J,0,Rg).
gaucheBasOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
gaucheBasOld(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBasOld(X1,Y1,J,R1,Rg).

droiteHautVerifOld(X,Y,J,Rg) :-
	droiteHautOld(X,Y,J,0,Rg).
droiteHautOld(X,Y,J,R,R) :-
	not(caseTestOld(X,Y,J)). %Jusqu'à la case non J
droiteHautOld(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHautOld(X1,Y1,J,R1,Rg).

%%%%%%% caseVideTestOld %%%%%
% caseVideTestOld(+X,+Y)
% vrai si la case X,Y est vide
caseVideTestOld(X,Y) :- nonvar(X),nonvar(Y),not(caseTestOld(X,Y,_)).

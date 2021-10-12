%%%%%%%%%%%% eval.pl %%%%%%%%%%%%
% Différentes fonctions d'évaluation pour le Puissance 4, toutes basées sur des heuristiques différentes.

:- module(eval, [evalTest1/2, evalPosition/3, caseVideTest/2, evalJeu/4]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(minimaxdraw).

:- use_module(library(random)).


%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeu/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Évalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y). Le score est pondéré par les différentes pondérations données en entrée (par assert) à evalJeu. Le score est ensuite perturbé par une valeur aléatoire, permettant de casser le caractère déterministe de l'IA.
% Score s'unifie avec le score évalué pour la position courante.
% evalJeu(JoueurCourant,AutreJoueur,X,Y,Score) :-
% 	assert(caseTest(X,Y,JoueurCourant)),
% 	assert(ennemiTest(AutreJoueur)),
% 	poidsPuissance3(PoidsPuissance3), poidsPosition(PoidsPosition), poidsDensite(PoidsDensite), poidsAdjacence(PoidsAdjacence),
% 	evalPosition(JoueurCourant,Score1,PoidsPosition),
% 	evalPuissances3(JoueurCourant,AutreJoueur,Score2,PoidsPuissance3),
% 	densite(JoueurCourant,Score3,PoidsDensite),
% 	evalAdjacence(X,Y,_,Score4, PoidsAdjacence),
% 	retract(caseTest(X,Y,JoueurCourant)),
% 	retract(ennemiTest(AutreJoueur)),
% 	random_between(-2,2,Perturbation),
% 	Score is Score1 * PoidsPosition
% 			+ Score2 * PoidsPuissance3
% 			+ Score3
% 			+ Score4
% 			+ Perturbation.

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% evalPosition/3(+Courant,-Score,+PoidsPosition)
% Évalue en privilégiant les positions centrales en fonction de la pondération.
% Score s'unifie à une valeur entre -400 et 400.

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


evalJeu(Courant,Score,X,Y) :-
	poidsPosition(PoidsPosition),
	poidsAlignement(PoidsAlignement),
	poidsBlocage(PoidsBlocage),
	evalPosition(Courant,ScorePosition,PoidsPosition),
	evalNbAlignements(Courant,ScoreAlignement,PoidsAlignement,X,Y),
	evalBlocage(Courant,ScoreBlocage,PoidsBlocage,X,Y),
	Score is ScorePosition * PoidsPosition 
		+ ScoreAlignement * PoidsAlignement
		+ ScoreBlocage * PoidsBlocage.


evalBlocage(Courant,Score,PoidsBlocage,X,Y) :-
	PoidsBlocage>0,
	%findall(S, evalCasesBlocage(Courant,S), Scores),
	evalCasesBlocage(Courant,Score,X,Y).
	%sum(Scores, ScoreTot),
	%Score is ScoreTot.
%evalBlocage(_,0,_,_,_).

evalCasesBlocage(Courant,ScoreCase,X,Y) :-
	ennemi(Courant,AutreJoueur),
	%caseTest(X,Y,Courant),
	calculPoidsAlignements(X,Y,AutreJoueur,ScoreCase).

evalNbAlignements(Courant,Score,PoidsAlignement,X,Y) :-
	PoidsAlignement>0,
	findall(S, evalCasesAlignements(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalNbAlignements(_,0,_,_,_).

evalCasesAlignements(Courant,ScoreCase) :-
	caseTest(X,Y,Courant),
	calculPoidsAlignements(X,Y,Courant,ScoreCase).
	

evalPosition(Courant,Score,PoidsPosition) :-
	PoidsPosition>0,
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, ScoreTot),
	Score is ScoreTot.
evalPosition(_,0,_).

evalCases(Courant,ScoreCase) :-
	caseTest(X,Y,Couleur),
	evalCaseNew(X,Y,Courant,Couleur,ScoreCase).

evalCaseOld(X,Y,Courant,ScoreCase) :-
	nbColonnes(NBCOLONNES),
	nbLignes(NBLIGNES),
	CentreX is NBCOLONNES // 2 + 1,
	CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX,
	Dy is Y - CentreY,
	abs(Dx,AbsX),
	abs(Dy,AbsY),
	ScoreCase is ( 200/(AbsX+1) + 200/(AbsY+1) ).

%-------------------------------------------------

evalCaseNew(X,Y,Courant,Couleur,ScoreCase) :-
	Couleur == Courant,
	caseTableauAlignements(X,Y,Poids),
	ScoreCase is Poids.

evalCaseNew(X,Y,Courant,Couleur,ScoreCase) :-
	Couleur \== Courant,
	caseTableauAlignements(X,Y,Poids),
	ScoreCase is Poids * -1.

%-------------------------------------------------


ponderationJ(X,Y, Courant,1) :-
	caseTest(X,Y,Courant), !.
ponderationJ(X,Y,_,-1) :-
	ennemiTest(J),
	caseTest(X,Y,J), !.
ponderationJ(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s'unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal,PoidsPuissance3) :-
	PoidsPuissance3>0,
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.
evalPuissances3(_,_,0,_).

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseTest(X,Y,Joueur),
	incr(X,X1),
	decr(X,X2),
	incr(Y,Y1),
	decr(Y,Y2),
	caseVideTest(X1,Y1),
	caseVideTest(X2,Y1),
	caseTest(X2,Y2,_),
	caseTest(X1,Y2,_),
	(gagneTestDirect(X1,Y1,Joueur) -> ScoreCase1=100 ; ScoreCase1=0),
	(gagneTestDirect(X2,Y1,Joueur) -> ScoreCase2=100 ; ScoreCase2=0),
	ScoreCase is ScoreCase1+ScoreCase2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR ADJACENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalAdjacence/5(+X,+Y,+Joueur,-Note,+PoidsAdjacence)
% Donne une note d'autant plus forte qu'un pion est entouré de pions amis.
% Note s'unifie au score de la position.

evalAdjacence(X,Y,Joueur,Note,PoidsAdjacence) :-
	PoidsAdjacence>0,
	aggregate_all(count,caseAdjacente(X,Y,Joueur,_,_),N),
	pow(N,2,Note).
evalAdjacence(_,_,_,0,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR DENSITE DE PION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% densite/3(+Joueur,-Note,+PoidsDensite)
% Donne une note d'autant plus élevée que les pions sont groupés.
% Note s'unifie au score de la position.
densite(J,Note,PoidsDensite) :- PoidsDensite>0, Z is 1, calculNbPoints(J,Z,Note).
densite(_,0,_).
calculNbPoints(_,Z,Note) :- Z>6, Note is 0.
calculNbPoints(J,Z,Note) :- nbPointsZone(J,Z,N), incr(Z,ZP), calculNbPoints(J,ZP,NP), Note is N+NP.
nbPointsZone(J,Z,NbPoints) :- nbPionsZone(J,Z,N), pow(N,2,NbPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nbPionsZone/3(+Joueur,+Zone,-NbPions)
% Donne le nombre de pions contenu dans une zone.
% NbPions s'unifie au nombre de pions contenu dans une zone.
nbPionsZone(J,Z,NbPions) :-
	aggregate_all(count,caseTestZone(Z,J,_,_),NbPions).

caseTestZone(Zone,Joueur,X,Y) :- caseTest(X,Y,Joueur), zone(Zone,X,Y).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.

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
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

% en colonne %

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
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
bas(X,Y,J,R,Rb) :-
	decr(Y,Y1),
	incr(R,R1),
	bas(X,Y1,J,R1,Rb).

hautVerif(X,Y,J,Rh):-
	haut(X,Y,J,0,Rh).
haut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
haut(X,Y,J,R,Rh) :-
	incr(Y,Y1),
	incr(R,R1),
	haut(x,Y1,J,R1,Rh).


%%% En diagonale \ %%%



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
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

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
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).

%%%%%%% caseVideTest %%%%%
% caseVideTest(+X,+Y)
% vrai si la case X,Y est vide
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%% Utilisé pour les tests unitaires

evalTest1(1,-3).
evalTest1(2,-4).
evalTest1(3,5).
evalTest1(4,10).
evalTest1(5,9).
evalTest1(6,-5).
evalTest1(7,8).

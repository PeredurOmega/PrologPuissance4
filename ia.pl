% - Emmanuel GARREAU
% - Mathis GOICHON
% - Yanis MANSOUR
% - Bérenger MAYOUD--DUPIN
% - Paul SOUTEYRAT
% - Timothé VERSTRAETE

%%%%%%%%%%%% ia.pl %%%%%%%%%%%%

%%% Code permettant d'appeler les différentes IA %%% 

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/6
			  ,iaMinimaxOld/7
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAlignement/1
			  ,poidsBlocage/1
			  ,poidsAdjacence/1
			  ,initDepth/1
			  ,ennemiTest/1
			  ,iaAlphabeta/8
			  ,poidsAlignementNew/1
			  ,poidsBlocageNew/1]
).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(minimaxdraw).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.
:- dynamic poidsAlignement/1.
:- dynamic poidsBlocage/1.
:- dynamic poidsAlignementNew/1.
:- dynamic poidsBlocageNew/1.
:- dynamic ennemiTest/1.
:- dynamic initDepth/1.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% AI Aléatoire a choisi une colonne pleine, donc on la fait recommencer.
iaAleatoire(Coup) :-
	iaAleatoire(Coup).

get_best((Move,_), Move).

initCaseTest :- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsAlignement,PoidsBlocage) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsAlignement(PoidsAlignement)),
	assert(poidsBlocage(PoidsBlocage)),
	initCaseTest,
	ennemi(JoueurCourant,AutreJoueur),
	assert(ennemiTest(AutreJoueur)),
	MaxMin is -1,
	minimax(Profondeur,JoueurCourant,MaxMin,Coup,_),
	retract(ennemiTest(AutreJoueur)),
	retractall(caseTest(_,_,_)).

iaAlphabeta(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsAlignement,PoidsBlocage,PoidsAlignementNew,PoidsBlocageNew) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsAlignement(PoidsAlignement)),
	assert(poidsBlocage(PoidsBlocage)),
	assert(poidsAlignementNew(PoidsAlignementNew)),
	assert(poidsBlocageNew(PoidsBlocageNew)),
	assert(initDepth(Profondeur)),
	initCaseTest,
	ennemi(JoueurCourant,AutreJoueur),
	assert(ennemiTest(AutreJoueur)),
	Alpha is -99999,
	Beta is 99999,
	MaxMin is -1,
	alpha_beta(Profondeur,JoueurCourant, Alpha, Beta, Coup, _, MaxMin),
	retract(ennemiTest(AutreJoueur)),
	retract(initDepth(Profondeur)),
	retractall(caseTest(_,_,_)).

iaMinimaxOld(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
		assert(poidsPosition(PoidsPosition)),
		assert(poidsPuissance3(PoidsPuissance3)),
		assert(poidsDensite(PoidsDensite)),
		assert(poidsAdjacence(PoidsAdjacence)),
		parcoursArbre(JoueurCourant,Profondeur,Coup,_).
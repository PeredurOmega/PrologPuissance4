﻿%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
% Deux "moteurs" d'IA :
% - "Aléatoire" jouant aléatoirement ;
% - "Minimax", implémentation de minimax assez paramétrable.

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/9
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAlignement/1
			  ,poidsBlocage/1
			  ,poidsAdjacence/1,
				ennemiTest/1,
				iaAlphabeta/9]
).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(minimaxdraw).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.
:- dynamic poidsAlignement/1.
:- dynamic poidsBlocage/1.
:- dynamic ennemiTest/1.

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

get_best((Move, Value), Move).

initCaseTest :- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsAlignement,PoidsBlocage,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsPuissance3(PoidsPuissance3)),
	assert(poidsDensite(PoidsDensite)),
	assert(poidsAdjacence(PoidsAdjacence)),
	assert(poidsAlignement(PoidsAlignement)),
	assert(poidsBlocage(PoidsBlocage)),
	initCaseTest,
	ennemi(JoueurCourant,AutreJoueur),
	assert(ennemiTest(AutreJoueur)),
	MaxMin is -1,
	minimax(Profondeur,JoueurCourant,MaxMin,Coup,Value),
	retract(ennemiTest(AutreJoueur)),
	retractall(caseTest(_,_,_)).
	%parcoursArbre(JoueurCourant,Profondeur,Coup,_).

iaAlphabeta(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsAlignement,PoidsBlocage,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsPuissance3(PoidsPuissance3)),
	assert(poidsDensite(PoidsDensite)),
	assert(poidsAdjacence(PoidsAdjacence)),
	assert(poidsAlignement(PoidsAlignement)),
	assert(poidsBlocage(PoidsBlocage)),
	initCaseTest,
	ennemi(JoueurCourant,AutreJoueur),
	assert(ennemiTest(AutreJoueur)),
	Alpha is -99999,
	Beta is 99999,
	MaxMin is -1,
	alpha_beta(Profondeur,JoueurCourant, Alpha, Beta, Coup, Value, MaxMin),
	retract(ennemiTest(AutreJoueur)),
	retractall(caseTest(_,_,_)).
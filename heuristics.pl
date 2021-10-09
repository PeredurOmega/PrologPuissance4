


% plusLongueSequence(Couleur,N) :- case(X,Y,Couleur), case(X1,Y,Couleur), X1 is X+1, N is N+1.

aligneesColonne(X,Y,[Y]) :- case(X,Y,_).
aligneesColonne(X,Y,[H|T]) :- case(X,Y,Couleur), case(X,H,Couleur), H is Y+1, aligneesColonne(X,H,T).

alignedColumns(X, [H|T]):- case(X,H,_), alignedColumns(X,T).


aligneesColonne2(X,Y,[],0,_).
aligneesColonne2(X,Y,[H|T],N,Couleur) :- case(X,Y,Couleur), case(X,H,Couleur), aligneesColonne2(X,Y,T,N1,Couleur), N is N1-1, Y is H - N + 1.


aligneesColonne3(X,Y,[],0).
aligneesColonne3(X,Y,[H|T],N) :- aligneesColonne3(X,Y,T,N1), N is N1+1, case(X,Y,Couleur), case(X,H,Couleur), Y is H-N+1.
aligneesColonne3(X,Y,[Y|T],N) :- case(X,Y,Couleur), aligneesColonne3(X,Y,T,N1), N is N1+1.

% casesAlignees(Couleur) :- 
%     % Alignées sur une colonne
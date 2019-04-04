:- lib(listut).       % a placer en commentaire si on utilise Swi-Prolog
                      % (le predicat delete/3 est predefini)
                      
                      % Indispensable dans le cas de ECLiPSe Prolog
                      % (le predicat delete/3 fait partie de la librairie listut)
                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
/*
initial_state([ [a, b, c],
                [g, h, d],
                [vide,f,e] ]). % h=2, f*=2


initial_state([ [b, h, c],     % EXEMPLE
                [a, f, d],     % DU COURS
                [g,vide,e] ]). % h=5 = f* = 5actions


initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h=10 f*=10
			
initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h=16, f*=20
			
initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h=24, f*=30 

*/
initial_state([[a, b,  c],
             [h,vide,d],
             [g, f,  e]]).

   %******************
   % ETAT FINAL DU JEU
   %******************

final_state([[a, b,  c],
             [h,vide,d],
             [g, f,  e]]).



%********************
% ETAT INITIAL DU JEU - Cas 4x4
%********************   
/* Cas 1 */ % L'état initial est l'état final.  
initial4_state1([ [a, b, c, d],
                [e, f, g, h],
                [i, j, k, l],
                [m, n, o, vide]]).

    
/* Cas 2 */ % Aucun élément n'est à sa place.
initial4_state2([ [n, g, a, i],
                [h, vide, b, j],
                [l, c, o, e], 
                [d, f, k, m]  ]). 

%******************
% ETAT FINAL DU JEU - Cas 4x4
%******************
/*final_state([ [a, b, c, d],
              [e, f, g, h],
              [i, j, k, l],
              [m, n, o, vide]]).
*/


   %********************
   % AFFICHAGE D'UN ETAT
   %********************

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal            
   %***********************
   
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.

   %**********************************
   % HEURISTIQUES (PARTIE A COMPLETER)
   %**********************************
   
heuristique(U,H) :-
   heuristique1(U, H).  % choisir l'heuristique 
 %  heuristique2(U, H).  % utilisee ( 1 ou 2)  
   
   %****************
   %HEURISTIQUE no 1
   %****************
   
   % Calcul du nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F
heuristique1(F, 0) :- 
    final_state(F),!.

heuristique1(U, H) :- 
    final_state(F), 
    matrice_difference(U,F,H).

% matrice_difference(+M1,+M2,?Resu)
matrice_difference([],[],0).
matrice_difference([L1|R1],[L2|R2],Resu) :- 
    ligne_difference(L1,L2,Resu_L), 
    matrice_difference(R1,R2,Resu_M), Resu is Resu_L+Resu_M.

% ligne_difference(+L1,+L2,?Resu)
ligne_difference([],[],0).
ligne_difference([E1|R1],[E2|R2],Resu) :- 
    element_difference(E1,E2,Resu_E),
    ligne_difference(R1,R2,Resu_L), Resu is Resu_E+Resu_L.

% element_difference(+E1,+E2,?Resu)
element_difference(vide,_E2,0):- !.
element_difference(E,E,0).
element_difference(E1,E2,1) :- 
    E1 \= E2, 
    E1 \= vide.


   
   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme sur l'ensemble des pieces des distances de Manhattan
   % entre la position courante de la piece et sa positon dans l'etat final

heuristique2(F, 0) :- 
    final_state(F), !.

heuristique2(U, H) :- 
    final_state(F), 
    findall(Dist, (dist_manhattan(E,U,F,Dist), E \= vide), List), 
    sumlist(List, H).

dist_manhattan(E,U,F,Resu) :- 
    position(E,U,X1,Y1), 
    position(E,F,X2,Y2), 
    Resu is (abs(X1-X2)+abs(Y1-Y2)).

position(E,M,X,Y) :- 
    nth1(Y,M,L), nth1(X,L,E).


 

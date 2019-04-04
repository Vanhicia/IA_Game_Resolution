%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de façon synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% initialisations Pf, Pu et Q 
	initial_state(S0), 
	heuristique(S0,H0), 
	G0 is 0, 
	F0 is G0+H0, 
	insert([ [F0,H0,G0], S0 ], nil, Pf), 
	insert([S0, [F0,H0,G0], nil, nil], nil, Pu),
	empty(Q),
	aetoile(Pf, Pu, Q).


%*******************************************************************************

% aucun état ne peut être développé => pas de solution
aetoile(nil, nil, _) :- 
	false,
	writeln("Pas de solution, l'état final n'est pas atteignable !").

% on atteint la situation terminale => une solution a été trouvée
aetoile(Pf,Pu,Q) :- 
	suppress_min([_, U], Pf, _), 
	final_state(U),
	suppress([U, L, Pere, Action], Pu, _),
	writeln("Affichage des étapes de la solution :"),
	affiche_solution(Q, [U,L,Pere,Action]),!.

% cas général
aetoile(Pf, Pu, Q) :- 
	suppress_min([[F,H,G], U], Pf, NPf), 
	suppress([U, [F,H,G], Pere, Action], Pu, NPu),
	expand(G, U, NPu, NPf, Q, NPf2, NPu2),
	insert([U, [F,H,G], Pere, Action], Q, NQ),
	aetoile(NPf2, NPu2, NQ),!.

% on cherche tous les noeuds contenant un état successeur S de la situation U
% et on traite chaque noeud grâce à loop_successor
expand(Gu, U, Pu, Pf, Q, NPf, NPu) :- 
	findall([S, [Fs,Hs,Gs], U, A], 
		    (rule(A,Cost, U, S), 
		    heuristique(S, Hs), 
		    Gs is Gu+Cost, 
		    Fs is Hs + Gs),
		    List_next),
	loop_successors(List_next, Pf, Pu, Q, NPf, NPu).


loop_successors([],Pf,Pu,_,Pf,Pu).
loop_successors([S|L], Pf, Pu, Q, NPf, NPu) :- 
	treat_one_successor(S, Pf, Pu, Q, NPf1, NPu1), 
	loop_successors(L, NPf1, NPu1, Q, NPf, NPu).


treat_one_successor([S,_,_,_], Pf, Pu, Q, Pf, Pu) :- 
	belongs([S,_,_,_],Q).

treat_one_successor([S, [Fs,Hs,Gs], U, A], Pf, Pu, _Q, NPf2, NPu2) :- 
	belongs([S,[_,_,_],_,_],Pu), 
	suppress([[F1,H1,G1], S], Pf, NPf), 
	suppress([S, [F1,H1,G1],_,_], Pu, NPu), 
	[Fs,Hs,Gs] @< [F1,H1,G1], 
	insert([[Fs,Hs,Gs], S], NPf, NPf2), 
	insert([S, [Fs,Hs,Gs], U, A], NPu, NPu2).
 
treat_one_successor([S,_,_,_], Pf, Pu, _Q, Pf, Pu) :- 
	belongs([S, [_,_,_],_,_],Pu).
%	[Fs,Hs,Gs] @> [F1,H1,G1], 

treat_one_successor([S, [Fs,Hs,Gs], U, A], Pf, Pu, _Q, NPf, NPu) :-  
	insert([S, [Fs,Hs,Gs],U, A], Pu, NPu), 
	insert([[Fs,Hs,Gs], S], Pf, NPf).

affiche_solution(_Q, [U,L,Pere,Action]) :- 
    initial_state(U), 
    print_step([U,L,Pere,Action]).
affiche_solution(Q, [U,L,Pere,Action]) :-
	belongs([Pere,L2,Pere2,Action2],Q),
	affiche_solution(Q, [Pere,L2,Pere2,Action2]),
	print_step([U,L,Pere,Action]).


print_step([U,[F,H,G],_Pere,A]) :-
	write("F = "),
	write(F),
	write(", H = "),
	write(H),
	write(", G = "),
	write(G),
	write(", Action = "),
	writeln(A),
	write_state(U),
	writeln("-------------------------------------------------").

	
   

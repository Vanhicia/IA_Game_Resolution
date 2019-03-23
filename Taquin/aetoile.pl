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

aetoile(nil, nil, _) :- false. %print("PAS de SOLUTION L ETAT FINAL N EST PAS ATTEIGNABLE!")
aetoile(Pf,_,_) :- suppress_min([[_,_,_], U], Pf, _), final_state(U), writeln(U). % état bien placé au départ
aetoile(Pf, Pu, Q) :- suppress_min([[F,H,G], U], Pf, NPf), 
					  suppress([U, [F,H,G], _, _], Pu, NPu),	
					  expand(G, U, NPu, NPf, Q, NPf2, NPu2),
					  insert([U, [F,H,G], _, _], Q, NQ),
					  aetoile(NPf2, NPu2, NQ).

expand(Gu, U, Pu, Pf, Q, NPf, NPu) :- 
		findall([S, [Fs,Hs,Gs], U, A],  
			(rule(A,Cost, U, S), 
			heuristique(S, Hs), 
			Gs is Gu+Cost, 
			Fs is Hs + Gs),
			List_next),
		put_flat(Pf),
		put_flat(Pu),
		loop_successors1(List_next, Pf, Pu, Q, NPf, NPu),
		put_flat(NPf),
		put_flat(NPu).


loop_successors1([],Pf,Pu,_,Pf,Pu).
loop_successors1([S|L], Pf, Pu, Q, NPf, NPu) :- loop_successors(S, Pf, Pu, Q, NPf1, NPu1), loop_successors1(L, NPf1, NPu1, Q, NPf, NPu).


loop_successors([S,_,_,_], Pf, Pu, Q, Pf, Pu) :- belongs([S,_,_,_],Q).

loop_successors([S, [Fs,Hs,Gs], U, A], Pf, Pu, _Q, NPf2, NPu2) :- belongs([S,[_,_,_],_,_],Pu), 
	suppress([[F1,H1,G1], S], Pf, NPf), 
	suppress([S, [F1,H1,G1],_,_], Pu, NPu), 
	[Fs,Hs,Gs] @< [F1,H1,G1], 
	insert([[Fs,Hs,Gs], S], NPf, NPf2), 
	insert([S, [Fs,Hs,Gs], U, A], NPu, NPu2).
 
loop_successors([S,_,_,_], Pf, Pu, _Q, Pf, Pu) :- belongs([S, [_,_,_],_,_],Pu).
%	[Fs,Hs,Gs] @> [F1,H1,G1], 

loop_successors([S, [Fs,Hs,Gs], U, A], Pf, Pu, _Q, NPf, NPu) :-  
	insert([S, [Fs,Hs,Gs],U, A], Pu, NPu), 
	insert([[Fs,Hs,Gs], S], Pf, NPf).

affiche_solution([First|Q], EF) :-
	writeln("----------------------------------"),
	writeln("------------Initial state---------"),
	writeln("----------------------------------"), 
	write(First),
	writeln("----------------------------------"),
	writeln("----------------Step--------------"),
	writeln("----------------------------------"),
	mem_states(Q, EF),
	writestep(First, Q, EF),


writestep(_,[]). 
writestep(S0,[[U,L,S0, A]|Q]) :- write(U), write("------->"), write(A), writestep(S0, Q).

%mem_states(Q,[U,L,S0,A], Mem) :- append( ,Mem).


	
   

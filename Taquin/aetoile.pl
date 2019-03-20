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

aetoile([], [], _) :- false. %print("PAS de SOLUTION L ETAT FINAL N EST PAS ATTEIGNABLE!")
aetoile(Pf,_,_) :- suppress_min([ [_,_,_], S0 ], Pf, _), final_state(S0). % état bien placé au départ
aetoile(Pf, Pu, Q) :- suppress_min([ [F0,H0,G0], S0], Pf, NPf), 
					  suppress([S0, [F0,H0,G0], _, _], Pu, NPu),	
					  expand(G0, S0, Pu, Pf, Q, NPf, NPu),
					  insert([S0, [F0,H0,G0], _,_], Q,NQ),
					  aetoile(NPf, NPu, NQ).

expand(G0, S0, Pu, Pf, Q, NPf, NPu) :- 
		findall([S, [Fs,Hs,Gs],S0,A],  
			(rule(A,Cost, S0, S), 
			heuristique(S, Hs), 
			Gs is G0+Cost, 
			Fs is Hs + Gs),
			List_next),
		loop_successors1(List_next, Pf, Pu, Q, NPf, NPu).


loop_successors1([],_,_,_,_,_) :- false.
loop_successors1([S|L], Pf, Pu, Q, NPf, NPu) :- loop_successors(S, Pf, Pu, Q, NPf, NPu), loop_successors1(L, Pf, Pu, Q, NPf, NPu).


loop_successors(S, _Pf, _Pu, Q, _NPf2, _NPu2) :- belongs(S,Q).

loop_successors([S, [Fs,Hs,Gs],S0,A], Pf, Pu, _Q, NPf2, NPu2) :- belongs([S, [Fs,Hs,Gs],S0,A],Pu), 
	suppress([[F1,H1,G1], S], Pf, NPf), 
	Fs < F1, 
	suppress([S, [F1,H1,G1],_,_], Pu, NPu), 
	insert([[Fs,Hs,Gs], S], NPf, NPf2), 
	insert([S, [Fs,Hs,Gs],S0,A], NPu, NPu2).
 
loop_successors([S, [Fs,Hs,Gs],S0,A], Pf, Pu, _Q, NPf, NPu) :-  
	insert([S, [Fs,Hs,Gs],S0,A], Pu, NPu), 
	insert([[Fs,Hs,Gs], S], Pf, NPf).





	
   

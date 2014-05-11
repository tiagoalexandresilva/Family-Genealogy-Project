%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Exercicio 1

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic progenitor/2.
:- dynamic avo/2.
:- dynamic bisavo/2.
:- dynamic bisneto/2.
:- dynamic tio/2.
:- dynamic irmao/2. 
:- dynamic primo/2.
:- dynamic sobrinho/2. 
:- dynamic descendente/2. 
:- dynamic ascendente/2.
:- dynamic listadescendentes/2.
:- dynamic listascendentes/2.
:- dynamic naturalidade/2.
:- dynamic relacao/3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento da Familia Duck
% Extensao do predicado filho: Filho,Progenitor -> {V,F}

filho(huey,della).
filho(dewey,della).
filho(louie,della).
filho(della,hortense).
filho(della,quackmore).
filho(donald,hortense).
filho(donald,quackmore).
filho(matilda,downy).
filho(matilda,fergus).
filho(scrooge,downy).
filho(scrooge,fergus).
filho(hortense,downy).
filho(hortense,fergus).
filho(quackmore,humperdink).
filho(quackmore,elvira).
filho(daphne,humperdink).
filho(daphne,elvira).
filho(eider,humperdink).
filho(eider,elvira).
filho(gladstone,goostave).
filho(gladstone,daphne).
filho(fethry,lulubelle).
filho(fethry,eider).
filho(abner,eider).
filho(abner,lulubelle).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado progenitor: Progenitor,Filho -> {V,F}
progenitor(P,F):-filho(F,P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}
avo(A,N):- filho(N,X),progenitor(A,X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisavo,Bisneto -> {V,F}
bisavo(B,BN):- filho(X,B),avo(X,BN).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisneto,Bisavo -> {V,F}
bisneto(BN,B):- bisavo(B,BN).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado neto: Neto,Avo -> {V,F}
neto(N,A):-avo(A,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: Irmao,Irmao -> {V,F}
irmao(I1,I2):- filho(I1,X),filho(I2,X), I1 \= I2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tio: Tio,Primo -> {V,F}
tio(T,S):- progenitor(P,S),irmao(P,T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sobrinho: Sobrinho,Tio -> {V,F}
sobrinho(S,T):- tio(T,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado primo: Primo,Primo -> {V,F}
primo(P1,P2):- progenitor(X,P1),tio(X,P2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado relacao: Individuo,Individuo,Relacao -> {V,F}
relacao(A,B,S):-filho(A,B),'filho'=S.
relacao(A,B,S):-progenitor(A,B),'progenitor'=S.
relacao(A,B,S):-neto(A,B),'neto'=S.
relacao(A,B,S):-avo(A,B),'avo'=S.
relacao(A,B,S):-primo(A,B),'primo'=S.
relacao(A,B,S):-tio(A,B),'tio'=S.
relacao(A,B,S):-irmao(A,B),'irmao'=S.
relacao(A,B,S):-sobrinho(A,B),'sobrinho'=S.
relacao(A,B,S):-bisavo(A,B),'bisavo'=S.
relacao(A,B,S):-bisneto(A,B),'bisneto'=S.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: X,Y,Grau -> {V,F}
descendente(D,A,1) :- filho(D,A).
descendente(D,A,1) :- pai(A,D).
descendente(D,A,G) :- filho(D,P), descendente(P,A,R), G is R+1.
descendente(D,A,G) :- pai(A,P), descendente(D,P,R), G is R+1.
		
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ascendente: X,Y,Grau -> {V,F}
ascendente(X,Y,G):- descendente(Y,X,G).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listadescendentes: Elemento,Grau,Lista -> {V,F}

listadescendentes(_,0,[]).				
listadescendentes(X,G,R) :-
		setof(Y,descendente(Y,X,G),R1), listadescendentes(X,G2,R2), concatenar(R2,R1,R),
			G is G2+1.

			
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				
% Extensao do predicado listascendentes: Elemento,Grau,Lista -> {V,F}
listascendentes(_,0,[]).	 
listascendentes(X,G,R) :-
		setof(Y,ascendente(Y,X,G),R1), listascendentes(X,G2,R2), concatenar(R2,R1,R),
			G is G2+1.
				
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Resultado -> {V,F}
comprimento([ ],0).
comprimento([H|T],R) :-
	comprimento(T,X),
		R is X+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado solucoes: I, T, S -> {V,F}		
solucoes( I,T,S ) :-
  findall( I,T,S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido de filho

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ), N == 1
                  ).

% Invariante Referencial: nao admitir mais do que 2 progenitores para um mesmo individuo

+filho( F,P ) :: (solucoes( (Ps),(filho( F,Ps )),S ),
                  comprimento( S,N ), N =< 2
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido de naturalidade
% Extensão do predicado naturalidade: Pessoa, Local -> {V,F}
+naturalidade( P,L ) :: (solucoes( (P,L),(naturalidade( P,L )),S ),
                  comprimento( S,N ), N == 1
                  ).				  

% Invariante Referencial: nao admitir mais do que 1 local para um mesmo individuo

+naturalidade( P,L ) :: (solucoes( (Ls),(naturalidade( P,Ls )),S ),
                  comprimento( S,N ), N == 1
                  ).
				  
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado evolucao: Termo -> {V,F}
evolucao( T ) :-
    solucoes( I,+T::I,L ),
    insercao( T ),
    teste( L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado insercao: Termo -> {V,F}
insercao( T ) :-
    assert( T ).
insercao( T ) :-
    retract( T ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado teste: Lista -> {V,F}
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).
	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado regressao: Termo -> {V,F}
regressao(T) :-
	remocao(T).
	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -				  
% Extensão do predicado remocao: Termo -> {V,F}
remocao(T) :-
		retract(T).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}

concatenar( [],L2,L2 ).
concatenar( [X|R],L2,[X|L] ) :-
    concatenar( R,L2,L ).


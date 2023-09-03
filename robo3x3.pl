% 0 - Limpo
% 1 - Sujeira
% 3 - Obstáculo
% Associação do vértice com status do mapa
:- dynamic st/2.
st(a,0).
st(b,1).
st(c,3).
st(d,0).
st(e,1).
st(f,1).
st(g,3).
st(h,1).
st(i,0).

% Conexões do grafo
s(a,b).
s(a,d).
s(b,c).
s(b,e).
s(c,f).
s(d,e).
s(d,g). 
s(e,f).
s(e,h). 
s(f,i). 
s(g,h).
s(h,i).

% Grafo não direcionado
% se "a" conecta a "b", "b" conecta a "a"
sSim(X,Y):-
    s(X,Y).
sSim(X,Y):-
    s(Y,X).

% H dos grafos
sH(a,4).
sH(b,3).
sH(c,2).
sH(d,3).
sH(e,2).
sH(f,1).
sH(g,2).
sH(h,1).
sH(i,0).

% Custo G dos grafos
% sG(G(V1,V2),V1,V2): custo de mudar do estado V1 para o estado V2
sGB(1,a,b).
sGB(1,a,d).
sGB(1,b,c).
sGB(1,b,e).
sGB(1,c,f).
sGB(1,d,e).
sGB(1,d,g).
sGB(1,e,f).
sGB(1,e,h).
sGB(1,f,i). 
sGB(1,g,h).
sGB(1,h,i).

sH(H,V1,V2):-
    sSim(V1,V2),
    sH(V2,H).

sG(G,V1,V2):-
    sGB(G,V1,V2).
sG(G,V1,V2):-
	sGB(G,V2,V1).

%sF(G(n),H(n),F(n),VerticeOrigem,VerticeDestino) - usa a função F
sF(G,H,F,V1,V2):-
    sG(G,V1,V2),
    sH(V2,H),
    F is G + H.

% Objetivo
objetivo(i).
% checa se a sala toda está limpa
stLimpo(X):- 
    st(b,X),
	st(e,X),
	st(f,X),
	st(h,X).

% limpa cada area da sala
removeSujeira(SujPos):-
    retract(st(SujPos,1)), % remove o fato
    assert(st(SujPos,0)). % adiciona o fato, limpo

% movimento do robo na sala
stRobo([Pos|_]):-
    objetivo(Pos),
    stLimpo(0).
stRobo([Pos|Caminho]):-
    st(Pos,0), % area sem obstaculo
    stRobo(Caminho).
stRobo([Pos|Caminho]):-
    st(Pos,1), % area suja
    removeSujeira(Pos),
    stRobo(Caminho).

% predicados principais 
limpezaHeuristica(hillClimb,Inicio):-
    hillClimb([[_,Inicio]],Solucao,_),
    stRobo(Solucao).
limpezaHeuristica(bestFirst,Inicio):-
    bestFirst([[_,Inicio]],Solucao,_),
    stRobo(Solucao).
limpezaHeuristica(branchAndBound,Inicio):-
    branchAndBound([[0,Inicio]],Solucao,_),
    stRobo(Solucao).
limpezaHeuristica(aEstrela,Inicio):-
    aEstrela([[0,0,0,Inicio]],Solucao,_),
    stRobo(Solucao).

% algoritmos dados pela professora
/*relações acessórias para processamento de listas*/
membro(X,[X|_]):-!.
membro(X,[_|C]):-
    membro(X,C).

concatena([],L,L).
concatena([X|L1],L,[X|L2]):-
    concatena(L1,L,L2).


/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ordenaF(Caminhos,CaminhosOrd)

Ordena os Caminhos a partir do primeiro valor de heurística 
dos caminhos em Caminhos e retorna em CaminhosOrd.
+ <arg-1> Caminhos - lista de caminhos a ser ordenada
- <arg-2> Caminhos ordenados
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
ordenaF(Caminhos,CaminhosOrd):-
	quicksortF(Caminhos,CaminhosOrd).

particionarF(_,[],[],[]).
particionarF(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorF(X,Y),!,
	particionarF(X,Cauda,Menor,Maior).
particionarF(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarF(X,Cauda,Menor,Maior).

quicksortF([],[]).
quicksortF([X|Cauda],ListaOrd):-
	particionarF(X,Cauda,Menor,Maior),
	quicksortF(Menor,MenorOrd),
	quicksortF(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).

%maiorF retorna verdadeiro se o valor de heurística F1 da lista do caminho
%é maior que o valor F2 da segunda lista
maiorF([F1|_],[F2|_]):-F1 > F2.

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeF(Caminho,NovosCaminhos).

Gera a partir de [F,G,H,NohAtual|Caminho] todos os Caminhos possiveis 
a partir de Caminho
utilizando função de custo G, função de avaliação H, e calculando 
F(NohNovo) = G(NohNovo) + H(NohNovo)
+ <arg-1> Caminho - [F,G,H,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
estendeF([_,GC,_,No|Caminho],NovosCaminhos):-
	findall([FNovo,GNovo,HNovo,NovoNo,No|Caminho],
	      (
          	  sF(GN,HN,_,No,NovoNo),
              not(member(NovoNo,[No|Caminho])),
              GNovo is GC + GN, 
          	  HNovo is HN, 
              FNovo is GNovo + HNovo
          ),
	      NovosCaminhos).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeG(Caminho,NovosCaminhos).

Gera a partir de [G,NoAtual|Caminho] todos os Caminhos possiveis 
a partir de Caminho
utilizando somente a função de custo G. 
O G dos caminhos resultantes deve ser o somatório
do caminho atual G com o custo para os nós visitados
+ <arg-1> Caminho - [G,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho 
(lista de todos os caminhos resultantes a partir de NoAtual)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
estendeG([G,NoAtual|Caminho],NovosCaminhos):-
    findall([GNovo,NovoNo,NoAtual|Caminho],
	       (
           	   sG(GH,NoAtual,NovoNo),
               not(membro(NovoNo,[NoAtual|Caminho])),
               GNovo is G + GH
           ),
           NovosCaminhos).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeH(Caminho,NovosCaminhos).

Gera a partir de [H,NoAtual|Caminho] todos os 
Caminhos possiveis a partir de Caminho
utilizando somente a função de avaliação H
+ <arg-1> Caminho - [H,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho
(lista de todos os caminhos resultantes a partir de NoAtual)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
estendeH([_,No|Caminho],NovosCaminhos) :-
	findall([HNovo,NovoNo,No|Caminho],
	(
		sH(HN,No,NovoNo),
		not(member(NovoNo,[No|Caminho])),
		HNovo is HN),
		NovosCaminhos
	).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
bestFirst(PossiveisCaminhos,Solucao)

Dada uma lista de PossiveisCaminhos (inicialmente com somente um
possivel caminho percorrido, contendo somente o nó inicial), 
na qual o primeiro caminho
[NoAtual|Caminho] é o que deve ser analisado, 
este programa implementa o algoritmo
de busca Best Fisrt para encontrar o melhor caminho. 
A estratégia consiste em gerar
uma lista de possíveis caminhos NovosCaminhos, 
concatenar com os Caminhos já existentes,
ordenar a lista completa segundo a função de avaliação H e seguir na busca.

+ <arg-1> Lista contendo os possiveis caminhos
Inicia com vazio - <arg-2> Solucao para o problema

Pre-condicoes:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
bestFirst([[_,No|Caminho]|_],Solucao, '-'):-
	objetivo(No),
	reverse([No|Caminho],Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo):-
	estendeH(Caminho, NovosCaminhos),
    concatena(NovosCaminhos, Caminhos, CaminhosTotal),
	ordenaF(CaminhosTotal, CaminhosOrd),
	bestFirst(CaminhosOrd, Solucao, Custo).


/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
branchAndBound(PossiveisCaminhos,Solucao)

Dada uma lista de PossiveisCaminhos (inicialmente com somente um
possivel caminho percorrido, contendo somente o nó inicial), 
na qual o primeiro caminho
[NoAtual|Caminho] é o que deve ser analisado, 
este programa implementa o algoritmo 
de busca Best First para encontrar o melhor caminho. 
A estratégia consiste em gerar 
uma lista de possíveis caminhos NovosCaminhos, 
concatenar com os Caminhos já existentes,
ordenar a lista completa segundo a função de avaliação G e seguir na busca.

+ <arg-1> Lista contendo os possiveis caminhos
Inicia com vazio 
- <arg-2> Solucao para o problema
- <arg-3> Custo da solucao encontrada

Pre-condicoes:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
branchAndBound([[G,_,_,No|Caminho]|_],Solucao,G):-	 
	objetivo(No),                                    
    reverse([No|Caminho],Solucao).

branchAndBound([Caminho|Caminhos], Solucao, G) :-
	estendeG(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
	ordenaF(CaminhosTotal,CaminhosTotOrd),
	branchAndBound(CaminhosTotOrd, Solucao, G). 	

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
hillClimb(PossiveisCaminhos,Solucao)

Dada uma lista de PossiveisCaminhos (inicialmente com somente um
possivel caminho percorrido, contendo somente o nó inicial), 
na qual o primeiro caminho
[NoAtual|Caminho] é o que deve ser analisado, 
este programa implementa o algoritmo 
de busca Hill Climbing para encontrar o
melhor caminho. A estratégia consiste em gerar 
uma lista de possíveis caminhos NovosCaminhos,
ordenar os caminhos de NovosCaminhos segundo 
a função de avaliação H, concatenar com os Caminhos já existentes
e seguir na busca.

+ <arg-1> Lista contendo os possiveis caminhos
Inicia com vazio - <arg-2> Solucao para o problema

Pre-condicoes:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
hillClimb([[_,No|Caminho]|_],Solucao,'-') :-
	objetivo(No),
	reverse([No|Caminho],Solucao).
hillClimb([Caminho|Caminhos], Solucao, Custo) :-
	estendeH(Caminho, NovosCaminhos),
	ordenaF(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
aEstrela(PossiveisCaminhos,Solucao)

Dada uma lista de PossiveisCaminhos (inicialmente com somente um
possivel caminho percorrido, contendo somente o nó inicial), 
na qual o primeiro caminho
[NoAtual|Caminho] é o que deve ser analisado, 
este programa implementa o algoritmo 
de busca A* para encontrar o melhor caminho. 
A estratégia consiste em gerar 
uma lista de possíveis caminhos NovosCaminhos, 
concatenar com os Caminhos já existentes,
ordenar a lista completa segundo 
a função de avaliação F = G+H e seguir na busca.

+ <arg-1> Lista contendo os possiveis caminhos - Inicia com vazio
- <arg-2> Solucao para o problema
- <arg-3> Custo da solucao encontrada
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

aEstrela([[G,_,_,No|Caminho]|_],Solucao,G):-	 
	objetivo(No),                                    
    reverse([No|Caminho],Solucao).

aEstrela([Caminho|Caminhos], Solucao, G) :-
	estendeF(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
	ordenaF(CaminhosTotal,CaminhosTotOrd),
	aEstrela(CaminhosTotOrd, Solucao, G). 	

% 0 - Limpo
% 1 - Sujeira
% 3 - Obstáculo
% Associação do vértice com status do mapa
:- dynamic st/2.
st(a,0).
st(b,0).
st(c,0).
st(d,1).
st(e,1).
st(f,0).
st(g,3).
st(h,0).
st(i,0).
st(j,0).
st(k,1).
st(l,0).
st(m,3).
st(n,0).
st(o,0).
st(p,1).

% Conexões do grafo
s(a,b).
s(a,e).
s(b,c).
s(b,f).
s(c,d).
s(c,g).
s(d,h).
s(e,f).
s(e,i).
s(f,g).
s(f,j).
s(g,h).
s(g,k).
s(h,l).
s(i,j).
s(i,m).
s(j,k).
s(j,n).
s(k,l).
s(k,o).
s(l,p).
s(m,n).
s(n,o).
s(o,p).

% Grafo não direcionado
% se "a" conecta a "b", "b" conecta a "a"
sSim(X,Y):-
    s(X,Y).
sSim(X,Y):-
    s(Y,X).

% H dos grafos
sH(a,6).
sH(b,5).
sH(c,4).
sH(d,3).
sH(e,5).
sH(f,4).
sH(g,3).
sH(h,2).
sH(i,4).
sH(j,3).
sH(k,2).
sH(l,1).
sH(m,3).
sH(n,2).
sH(o,1).
sH(p,0).

% Custo G dos grafos
% sG(G(V1,V2),V1,V2): custo de mudar do estado V1 para o estado V2
sGB(1,a,b).
sGB(1,a,e).
sGB(1,b,c).
sGB(1,b,f).
sGB(1,c,d).
sGB(1,c,g).
sGB(1,d,h).
sGB(1,e,f).
sGB(1,e,i).
sGB(1,f,g).
sGB(1,f,j).
sGB(1,g,h).
sGB(1,g,k).
sGB(1,h,l).
sGB(1,i,j).
sGB(1,i,m).
sGB(1,j,k).
sGB(1,j,n).
sGB(1,k,l).
sGB(1,k,o).
sGB(1,l,p).
sGB(1,m,n).
sGB(1,n,o).
sGB(1,o,p).

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
objetivo(p).
% checa se a sala toda está limpa
stLimpo(X):- 
    st(d,X),
	st(e,X),
	st(k,X),
	st(p,X).

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

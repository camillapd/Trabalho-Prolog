% 0 - Limpo
% 1 - Sujeira
% 3 - Obstáculo
% Associação do vértice com status do mapa
:- dynamic st/2.
st(a,0).
st(b,0).
st(c,1).
st(d,1).
st(e,1).
st(f,3).
st(g,3).
st(h,0).
st(i,1).
st(j,0).
st(k,0).
st(l,0).
st(m,1).
st(n,3).
st(o,3).
st(p,0).
st(q,1).
st(r,0).
st(s,0).
st(t,0).
st(u,3).
st(v,0).
st(x,1).
st(y,1).
st(z,0).

% Conexões do grafo
s(a,b).
s(a,f).
s(b,c).
s(b,g).
s(c,d).
s(c,h).
s(d,e).
s(d,i).
s(e,j).
s(f,g).
s(f,k).
s(g,h).
s(g,l).
s(h,i).
s(h,m).
s(i,j).
s(i,n).
s(j,o).
s(k,l).
s(k,p).
s(l,m).
s(l,q).
s(m,n).
s(m,r).
s(n,o).
s(n,s).
s(o,t).
s(p,q).
s(p,u).
s(q,r).
s(q,v).
s(r,s).
s(r,x).
s(s,t).
s(s,y).
s(t,z).
s(u,v).
s(v,x).
s(x,y).
s(y,z).

% Grafo não direcionado
% se "a" conecta a "b", "b" conecta a "a"
sSim(X,Y):-
    s(X,Y).
sSim(X,Y):-
    s(Y,X).

% H dos grafos
sH(a,8).
sH(b,7).
sH(c,6).
sH(d,5).
sH(e,4).
sH(f,7).
sH(g,6).
sH(h,5).
sH(i,4).
sH(j,3).
sH(k,6).
sH(l,5).
sH(m,4).
sH(n,3).
sH(o,2).
sH(p,5).
sH(q,4).
sH(r,3).
sH(s,2).
sH(t,1).
sH(u,4).
sH(v,3).
sH(x,2).
sH(y,1).
sH(z,0).

% Custo G dos grafos
% sG(G(V1,V2),V1,V2): custo de mudar do estado V1 para o estado V2
sGB(1,a,b).
sGB(1,a,f).
sGB(1,b,c).
sGB(1,b,g).
sGB(1,c,d).
sGB(1,c,h).
sGB(1,d,e).
sGB(1,d,i).
sGB(1,e,j).
sGB(1,f,g).
sGB(1,f,k).
sGB(1,g,h).
sGB(1,g,l).
sGB(1,h,i).
sGB(1,h,m).
sGB(1,i,j).
sGB(1,i,n).
sGB(1,j,o).
sGB(1,k,l).
sGB(1,k,p).
sGB(1,l,m).
sGB(1,l,q).
sGB(1,m,n).
sGB(1,m,r).
sGB(1,n,o).
sGB(1,n,s).
sGB(1,o,t).
sGB(1,p,q).
sGB(1,p,u).
sGB(1,q,r).
sGB(1,q,v).
sGB(1,r,s).
sGB(1,r,x).
sGB(1,s,t).
sGB(1,s,y).
sGB(1,t,z).
sGB(1,u,v).
sGB(1,v,x).
sGB(1,x,y).
sGB(1,y,z).

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
objetivo(z).

stLimpo(X):-
	st(c,X),
	st(d,X),
	st(e,X),
	st(i,X),
	st(m,X),
	st(q,X),
	st(x,X),
	st(y,X).

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
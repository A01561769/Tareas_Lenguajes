% A01561769 - Antonio Torres Carvajal
% A00819647 - Carlos Eduardo Govea González

/* 1.- Predicado que encuentra el n-ésimo elemento de una lista. */
enesimo(1, [X | _], X).
enesimo(N, [_ | Y], M):-
    Z is N - 1,
    enesimo(Z, Y, R), !,
    M is R.

/* 2.-  Predicado que obtiene  una  lista  incremental  de números
        enteros entre dos valores pasados como argumentos. */
rango(X, X, [X]).
rango(X, Y, R):-
    Z is X + 1,
    rango(Z, Y, Rest),
    R = [X | Rest].

/* 3.-  Predicado que obtiene una lista de pares de elementos
        construida como el producto cartesiano de dos conjuntos
        representados como listas. */
cartesiano(L1, L2, R):- cartesiano_aux(L1, L2, R, L1).

cartesiano_aux([], [_|Resto], R, Memoria):- !,
    cartesiano_aux(Memoria, Resto, R, Memoria).
cartesiano_aux([L1_E|L1_Resto], [L2_E|L2_Resto], [[L1_E|L2_E]|R_Resto], Memoria):- !,
    cartesiano_aux(L1_Resto, [L2_E|L2_Resto], R_Resto, Memoria).
cartesiano_aux(_, [], [], _).

/* 4.-  Predicado que cuenta las veces que aparece un elemento
        particular dentro de una lista imbricada. */
cuenta_profundo(E, [A | B], R):-
    cuenta_profundo(E, A, C1),
    cuenta_profundo(E, B, C2),
    R = C1 + C2.
cuenta_profundo(_, [], 0).
cuenta_profundo(E, [E | Resto], R):-
    cuenta_profundo(E, Resto, S), !,
    R is 1 + S.
cuenta_profundo(E, [_ | Resto], R):-
    cuenta_profundo(E, Resto, S), !,
    R is 0 + S.

/* 5.-  Predicado que obtiene una lista con los elementos que
        no aparecen repetidos dentro de una lista imbricada. */
lista_unicos(Lista, R):- !,
    crea_lista(Lista, Lista, Res),
    R = Res.
lista_unicos([], []).

crea_lista([A | B], C, R):-
    crea_lista(A, C, R1),
    crea_lista(B, C, R2),
    R = [R1 | R2].
crea_lista([A | B], C, R):-
    cuenta_profundo(A, C, Reps),
    Reps > 1,
    crea_lista(B, C, Res),
    R = Res.
crea_lista([A | B], C, R):-
    cuenta_profundo(A, C, Reps),
    Reps =:= 1,
    crea_lista(B, C, Res),
    R = [A | Res].
crea_lista([], _, []).

/* 7.-  Predicado que, a partir de una lista de números, crea
        un árbol binario de búsqueda descrito con la función:
        arbol(Raíz, SubárbolIzquierdo, SubárbolDerecho). */
siembra([E|Resto], Resultado):-
    crea_nodo(nil, E, Arbol),
    inserta_nodo(Resto, Arbol, Resultado).
siembra([], nil).

% Predicado que crea la estructura del árbol a partir de nodos
crea_nodo(arbol(Raiz, SubI1, SubD), E, arbol(Raiz, SubI2, SubD)):-          % Subárbol Izquierdo
    Raiz > E, !,
    crea_nodo(SubI1, E, SubI2).
crea_nodo(arbol(Raiz, SubI, SubD1), E, arbol(Raiz, SubI, SubD2)):-         % Subárbol Derecho
    E > Raiz, !,
    crea_nodo(SubD1, E, SubD2).
crea_nodo(arbol(Raiz, SubI, SubD), Raiz, arbol(Raiz, SubI, SubD)):-!.       % Caso base para evitar duplicados
crea_nodo(nil, Raiz, arbol(Raiz, nil, nil)).                                % Crear raíz

% Predicado que inserta un nuevo nodo en el árbol binario
inserta_nodo([E|Resto], Arbol1, Resultado):- !,
  crea_nodo(Arbol1, E, Arbol2),
  inserta_nodo(Resto, Arbol2, Resultado).
inserta_nodo([], Arbol, Arbol).

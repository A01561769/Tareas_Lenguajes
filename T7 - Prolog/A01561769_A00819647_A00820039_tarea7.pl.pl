% A01561769 - Antonio Torres Carvajal

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
cartesiano([], _, []).
cartesiano([E, R], L2, R):-
    generaSubL(E, L2, Ra),
    cartesiano(R, L2, Rb), !,
    R = [Ra | Rb].

generaSubL(_, [], []).
generaSubL(A, [B | Resto], R):-
    generaSubL(A, Resto, Sig), !,
    R = [[A, B] | Sig].

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
lista_unicos([], []).
lista_unicos(Lista, R):-
    crea_lista(Lista, Lista, Res),
    R = Res.

crea_lista([], _, []).
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
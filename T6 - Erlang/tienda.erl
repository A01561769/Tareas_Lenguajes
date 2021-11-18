% A01561769 - Antonio Torres Carvajal
% A00819647 - Carlos Eduardo Govea Gonzales
% A00820039 - Mariano Hurtado de Mendoza Carranza
-module(tienda).
-export([servidor/3, abre_tienda/0]).

% Proceso servidor que atiende todas las solicitudes de socios y productos
% Lst_Socios: Lista que almacena todos los nombres de los socios
% Lst_Prods: Lista que almacena todos los productos disponibles para socios
% Lst_Pedidos: Lista que almacena todos los pedidos realizados por los socios
servidor(Lst_Socios, Lst_Prods, Lst_Pedidos) ->
    receive
        % -------------- SOCIOS --------------
        % Subscribir socio en Lst_Socios
        {De, {suscribe, Socio}} ->
            case busca(Socio, Lst_Socios) of
                registrado ->
                    De ! {tienda, ya_Registrado},
                    servidor(Lst_Socios, Lst_Prods, Lst_Pedidos);
                noRegistrado ->
                    De ! {tienda, nuevo_registro},
                    servidor(suscribe(Socio, Lst_Socios), Lst_Prods, Lst_Pedidos)
            end;
        % Eliminar socio de Lst_Socios
        {De, {elimina, Socio}} ->
            case busca(Socio, Lst_Socios) of
                registrado ->
                    De ! {tienda, eliminado},
                    servidor(elimina(Socio, Lst_Socios), Lst_Prods, Lst_Pedidos);
                noRegistrado ->
                    De ! {tienda, no_Registrado},
                    servidor(Lst_Socios, Lst_Prods, Lst_Pedidos)
            end;
        % Generar una lista de tuplas {Producto, CantidadPedida} para el usuario
        % añadir el pedido a Lst_Pedidos y actualizar Lst_Prods
        % para reducir las existencias no menor a 0.
        % {De, {pedido, Socio, ListaDeProds}} ->

        % Regresar el contenido de Lst_Prods a un socio
        {De, lista_exts} ->
            De ! {tienda, Lst_Prods},
            servidor(Lst_Socios, Lst_Prods, Lst_Pedidos)
        
        % -------------- PRODUCTOS --------------
    end.

% Función que inicializa el servidor
abre_tienda() ->
    register(tienda, spawn( tienda, servidor, [[],[],[]] ) ).

% Función de búsqueda de socios en Lst_Socios
busca(Socio, [Socio | _]) ->
    registrado;
busca(Socio, [_ | T]) ->
    busca(Socio, T);
busca(_, []) ->
    noRegistrado.

% Función que registra a un nuevo socio en Lst_Socios
suscribe(Socio, [H | T]) ->
    [H | suscribe(Socio, T)];
suscribe(Socio, []) ->
    [Socio].

% Función que elimina a un socio de Lst_Socios
elimina(Socio, [Socio | T]) ->
    [T];
elimina(Socio, [H | T]) ->
    [H | elimina(Socio, T)].

% Esto es un comentario, es posible código para el despliege de Lst_Socios
-ifdef(comment).
lista_socios([Socio | T]) ->
    io:format("~p~n", [Socio]),
    lista_socios(T);
lista_socios([]) ->
    ok.
-endif.
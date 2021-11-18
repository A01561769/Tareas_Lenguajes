% A01561769 - Antonio Torres Carvajal
% A00819647 - Carlos Eduardo Govea Gonzales
% A00820039 - Mariano Hurtado de Mendoza Carranza
-module(socio).
-export([suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0]).

% Hay que modificar 'AntonyPC' para que funcione en otra PC
matriz() -> 'tienda@AntonyPC'.

% Función que crea la llamada al servidor para registrar a un socio.
% Socio: Nombre a registrar.
suscribir_socio(Socio) -> 
    llama_tienda({suscribe, Socio}).

% Función que crea la llamada al servidor para eliminar a un socio.
% Socio: Nombre a eliminar.
elimina_socio(Socio) -> 
    llama_tienda({elimina, Socio}).

% Función que crea la llamada al servidor para crear un pedido.
% Socio: Nombre sel socio.
% ListaDeProds: Lista de productos del pedido.
crea_pedido(Socio, ListaDeProds) -> 
    llama_tienda({pedido, Socio, ListaDeProds}).

% Función que crea la llamada al servidor para mostrar la lista de 
% productos existentes.
lista_existencias() -> 
    llama_tienda(lista_exts).

% Socio.
llama_tienda(Mensaje) ->
    Matriz = matriz(),
    monitor_node(Matriz, true),
    {tienda, Matriz} ! {self(), Mensaje},
    receive
        {tienda, Respuesta} ->
            monitor_node(Matriz, false),
            Respuesta;
        {nodedown, Matriz} ->
            no
    end.
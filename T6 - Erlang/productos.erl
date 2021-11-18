% A01561769 - Antonio Torres Carvajal
% A00819647 - Carlos Eduardo Govea Gonzales
% A00820039 - Mariano Hurtado de Mendoza Carranza
-module(productos).
-export([registra_producto/2, elimina_producto/1, modifica_producto/2]).

% Hay que modificar 'AntonyPC' para que funcione en otra PC
matriz() -> 'productos@AntonyPC'.

% Función que crea la llamada al servidor para registrar un producto.
% Producto: Producto a registrar.
% Cantidad: Cantidad disponible del producto.
registra_producto(Producto,Cantidad) ->
    llama_tienda({registra_Prod, Producto, Cantidad}).

% Función que crea la llamada al servidor para eliminar un producto.
% Producto: Producto a eliminar.
elimina_producto(Producto) ->
    llama_tienda({elimina_Prod, Producto}).

% Función que crea la llamada al servidor para modificar el status
% de un producto.
% Producto: Producto a modificar.
% Cantidad: Cantidad del producto a modificar.
modifica_producto(Producto,Cantidad) ->
    llama_tienda([modifica_Prod, Producto, Cantidad]).

% Productos
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
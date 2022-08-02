-module(rabbit_utils).
%%---------------------------------------------------------------------%%
-export([start/1]).

-export([connect/1]).
-export([create_channel/0]).
-export([declare_exchange/1]).
-export([declare_queue/1]).
-export([bind_queue/1]).
%%---------------------------------------------------------------------%%
-include_lib("amqp_client/include/amqp_client.hrl").
%%---------------------------------------------------------------------%%
start(Params) ->
    connect(Params),
    create_channel(),
    declare_exchange(Params),
    declare_queue(Params),
    bind_queue(Params).

connect(Params) ->
    Network = #amqp_params_network{ host = proplists:get_value(host, Params, "localhost"),
                                    username = proplists:get_value(username, Params, <<"guest">>),
                                    password = proplists:get_value(password, Params, <<"guest">>),
                                    port = proplists:get_value(port, Params, 5672) },
    {ok, Connection} = amqp_connection:start(Network),
    register(connection, Connection),
    {ok, Connection}.

create_channel() ->
    {ok, Channel} = amqp_connection:open_channel(whereis(connection)),
    register(channel, Channel),
    {ok, Channel}.

declare_exchange(Params) ->
    Exchange = proplists:get_value(exchange, Params, <<"DefaultExchange">>),
    Declare = #'exchange.declare'{ exchange=Exchange },
    #'exchange.declare_ok'{} = amqp_channel:call(whereis(channel), Declare).

declare_queue(Params) ->
    Queue = proplists:get_value(queue, Params, <<"DefaultQueue">>),
    Declare = #'queue.declare'{ queue=Queue },
    #'queue.declare_ok'{} = amqp_channel:call(whereis(channel), Declare).

bind_queue(Params) ->
    Queue = proplists:get_value(queue, Params, <<"DefaultQueue">>),
    Exchange = proplists:get_value(exchange, Params, <<"DefaultExchange">>),
    RoutingKey = proplists:get_value(routing_key, Params, <<"DefaultExchange">>),
    Binding = #'queue.bind'{ queue=Queue,
                             exchange=Exchange,
                             routing_key=RoutingKey },
    #'queue.bind_ok'{} = amqp_channel:call(whereis(channel), Binding).
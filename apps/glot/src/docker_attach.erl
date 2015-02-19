-module(docker_attach).
-behaviour(gen_fsm).

-include_lib("hackney/include/hackney_lib.hrl").

-export([
    start_link/0,
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    code_change/4,
    terminate/3,

    ready/3,
    recv_http/2,
    recv_http_header/2,
    attached/3,
    recv_response/2,

    attach/2,
    send/2
]).

-record(state, {
    socket,
    payload,
    callback_pid,
    buffer=[]
}).


start_link() ->
    lager:info("docker_attach start_link"),
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, ready, #state{}}.

ready({attach, ContainerId}, From, State) ->
    lager:info("attach!"),
    Url = hackney_url:parse_url(config:docker_api_url()),
    Host = Url#hackney_url.host,
    Port = Url#hackney_url.port,
    lager:info("Connecting to ~s:~p", [Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, line}, {keepalive, true}]),
    gen_tcp:send(Socket, <<"POST /v1.7/containers/", ContainerId/binary, "/attach?stdin=1&stdout=1&stderr=1&stream=1 HTTP/1.1\r\nContent-Type: application/vnd.docker.raw-stream\r\n\r\n">>),
    NewState = State#state{socket=Socket, callback_pid=From},
    {next_state, recv_http, NewState}.

recv_http(<<"HTTP/1.1 200 OK\r\n">>, State) ->
    lager:info("got 200 ok"),
    {next_state, recv_http_header, State}.

recv_http_header(<<"\r\n">>, State=#state{callback_pid=From}) ->
    lager:info("got end of headers"),
    gen_fsm:reply(From, ok),
    {next_state, attached, State};
recv_http_header(Header, State) ->
    lager:info("Received http header: ~p", [Header]),
    {next_state, recv_http_header, State}.

attached({payload, Payload}, From, State=#state{socket=Socket}) ->
    lager:info("Sending payload"),
    gen_tcp:send(Socket, Payload),
    {next_state, recv_response, State#state{callback_pid=From}}.

recv_response(Data, State=#state{buffer=Buffer}) ->
    lager:info("Received data"),
    NewState = State#state{buffer=[Data|Buffer]},
    {next_state, recv_response, NewState}.

handle_event(_Event, StateName, State) ->
    lager:info("Got unhandled event"),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

% Handle incoming data from socket
handle_info({tcp, _Socket, Data}, StateName, State) ->
    gen_fsm:send_event(self(), Data),
    {next_state, StateName, State};

handle_info({tcp_closed, _}, _StateName, State=#state{callback_pid=From, buffer=Buffer}) ->
    lager:info("Socket closed"),
    Data = parse_data(Buffer),
    gen_fsm:reply(From, Data),
    lager:info("Stopping fsm"),
    {stop, normal, State};

handle_info(stop, _StateName, State) ->
    io:format("stopping..~n"),
    {stop, normal, State};

handle_info(_Info, StateName, State) ->
    lager:info("Got unhandled msg"),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    Reason.

parse_data(Data) ->
    parse_data(list_to_binary(lists:reverse(Data)), []).

parse_data(<<>>, Bodies) ->
    format_response(lists:reverse(Bodies));
parse_data(Data, Bodies) ->
    <<Type:8, 0, 0, 0, Size:32, Rest/binary>> = Data,
    <<Body:Size/binary, Next/binary>> = Rest,
    parse_data(Next, [{Type, Body}|Bodies]).

format_response(Data) ->
    format_response(
        proplists:get_all_values(1, Data),
        proplists:get_all_values(2, Data)
    ).

format_response(Stdout, []) ->
    {ok, list_to_binary(Stdout)};
format_response(_, Stderr) ->
    {error, list_to_binary(Stderr)}.

attach(FsmPid, ContainerId) ->
    gen_fsm:sync_send_event(FsmPid, {attach, ContainerId}).

send(FsmPid, Payload) ->
    % TODO: Add proper timeout (same above)
    % infinity? docker_gc will send shutdown signal
    gen_fsm:sync_send_event(FsmPid, {payload, Payload}).

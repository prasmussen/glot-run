-module(docker_attach).
-behaviour(gen_fsm).

-include_lib("hackney/include/hackney_lib.hrl").

% Wait 1 hour before giving up when using sync calls
-define(SYNC_TIMEOUT, 3600000).

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
    detach/2,
    send/2
]).

-record(state, {
    socket,
    callback_pid,
    buffer=[],
    buffer_size=0
}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    log:event(<<"Transition to ready state">>),
    {ok, ready, #state{}}.

ready({attach, ContainerId}, From, State) ->
    ApiUrl = config:docker_api_url(),
    #hackney_url{host=Host, port=Port} = hackney_url:parse_url(ApiUrl),
    log:event([<<"Connect to ">>, list_to_binary(Host), <<":">>, integer_to_binary(Port)]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [
        binary, {active, true}, {packet, line}, {keepalive, true}
    ]),
    gen_tcp:send(Socket, raw_container_attach_request(ContainerId)),
    NewState = State#state{socket=Socket, callback_pid=From},
    log:event(<<"Transition to recv_http state">>),
    {next_state, recv_http, NewState}.

recv_http(<<"HTTP/1.1 101 UPGRADED\r\n">>, State) ->
    log:event(<<"Transition to recv_http_header state">>),
    {next_state, recv_http_header, State}.

recv_http_header(<<"\r\n">>, State=#state{callback_pid=From}) ->
    gen_fsm:reply(From, ok),
    log:event(<<"Transition to attached state">>),
    {next_state, attached, State};
recv_http_header(_Header, State) ->
    {next_state, recv_http_header, State}.

attached({payload, Payload}, From, State=#state{socket=Socket}) ->
    log:event(<<"Send payload">>),
    gen_tcp:send(Socket, Payload),
    log:event(<<"Transition to recv_response state">>),
    {next_state, recv_response, State#state{callback_pid=From}}.

recv_response(Data, State=#state{buffer=Buffer}) ->
    log:event(<<"Received data">>),
    NewState = State#state{buffer=[Data|Buffer]},
    {next_state, recv_response, NewState}.

handle_event({detach, timeout}, _, State=#state{callback_pid=From, socket=Socket}) ->
    log:event(<<"Reply with timeout error">>),
    gen_fsm:reply(From, {error, timeout}),
    gen_tcp:close(Socket),
    log:event(<<"Stop normal">>),
    {stop, normal, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

% Handle incoming data from socket
handle_info({tcp, Socket, Data}, StateName, State=#state{callback_pid=From, buffer_size=Size}) ->
    NewSize = Size + byte_size(Data),
    case NewSize > config:max_output_size() of
        false ->
            gen_fsm:send_event(self(), Data),
            {next_state, StateName, State#state{buffer_size=NewSize}};
        true ->
            log:event(<<"Reply with max size error">>),
            gen_fsm:reply(From, {error, max_output_size}),
            gen_tcp:close(Socket),
            log:event(<<"Stop normal">>),
            {stop, normal, State}
    end;
handle_info({tcp_closed, _}, _StateName, State=#state{callback_pid=From, buffer=Buffer}) ->
    log:event(<<"Socket closed">>),
    Data = parse_data(Buffer),
    log:event(<<"Reply with data">>),
    gen_fsm:reply(From, Data),
    log:event(<<"Stop normal">>),
    {stop, normal, State};
handle_info(stop, _StateName, State) ->
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    Reason.

raw_container_attach_request(ContainerId) ->
    Url = docker_url:container_attach(<<>>, ContainerId, [
        {stdin, true}, {stdout, true}, {stream, true}
    ]),
    [
        <<"POST ", Url/binary, " HTTP/1.1\r\n">>,
        <<"Content-Type: application/vnd.docker.raw-stream\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Upgrade: tcp\r\n">>,
        <<"Host: 127.0.0.1\r\n">>,
        <<"\r\n">>
    ].

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
    gen_fsm:sync_send_event(FsmPid, {attach, ContainerId}, ?SYNC_TIMEOUT).

detach(FsmPid, Reason) ->
    gen_fsm:send_all_state_event(FsmPid, {detach, Reason}).

send(FsmPid, Payload) ->
    gen_fsm:sync_send_event(FsmPid, {payload, Payload}, ?SYNC_TIMEOUT).

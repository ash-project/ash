%%%-------------------------------------------------------------------
%% @private ETS table for handlers.
%%
%% Each handler is stored in the table. A key is an event name the
%% handler is attached to. All writes to a table go through a single
%% Agent process to make sure that we don't get duplicate handler IDs.
%%
%% Reads (`list_handlers_...') are executed by the calling process.
%% @end
%%%-------------------------------------------------------------------
-module(telemetry_handler_table).

-behaviour(gen_server).

-export([start_link/0,
         insert/4,
         delete/1,
         list_for_event/1,
         list_by_prefix/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("telemetry.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec insert(HandlerId, EventNames, Function, Config) -> ok | {error, already_exists} when
      HandlerId :: telemetry:handler_id(),
      EventNames :: [telemetry:event_name()],
      Function :: telemetry:handler_function(),
      Config :: telemetry:handler_config().
insert(HandlerId, EventNames, Function, Config) ->
    gen_server:call(?MODULE, {insert, HandlerId, EventNames, Function, Config}).

-spec delete(telemetry:handler_id()) -> ok | {error, not_found}.
delete(HandlerId) ->
    gen_server:call(?MODULE, {delete, HandlerId}).

-spec list_for_event(telemetry:event_name()) -> [#handler{}].
list_for_event(EventName) ->
    try
        ets:lookup(?MODULE, EventName)
    catch
        error:badarg ->
            ?LOG_WARNING("Failed to lookup telemetry handlers. "
                         "Ensure the telemetry application has been started. ", []),
            []
    end.

-spec list_by_prefix(telemetry:event_prefix()) -> [#handler{}].
list_by_prefix(EventPrefix) ->
    Pattern = match_pattern_for_prefix(EventPrefix),
    ets:match_object(?MODULE, Pattern).

init([]) ->
    _ = create_table(),
    {ok, []}.

handle_call({insert, HandlerId, EventNames, Function, Config}, _From, State) ->
    case ets:match(?MODULE, #handler{id=HandlerId,
                                     _='_'}) of
        [] ->
            Objects = [#handler{id=HandlerId,
                                event_name=EventName,
                                function=Function,
                                config=Config} || EventName <- EventNames],
            ets:insert(?MODULE, Objects),
            {reply, ok, State};
        _ ->
            {reply, {error, already_exists}, State}
    end;
handle_call({delete, HandlerId}, _From, State) ->
    case ets:select_delete(?MODULE, [{#handler{id=HandlerId,
                                              _='_'}, [], [true]}]) of
        0 ->
            {reply, {error, not_found}, State};
        _ ->
            {reply, ok, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%

create_table() ->
    ets:new(?MODULE, [duplicate_bag, protected, named_table,
                      {keypos, 3}, {read_concurrency, true}]).

match_pattern_for_prefix(EventPrefix) ->
    #handler{event_name=match_for_prefix(EventPrefix),
             _='_'}.

-dialyzer({nowarn_function, match_for_prefix/1}).
match_for_prefix([]) ->
    '_';
match_for_prefix([Segment | Rest]) ->
    [Segment | match_for_prefix(Rest)].

-module(session_counter_api).
-author('mkorszun@gmail.com').

-export([add/2, active_sessions/3]).

-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% API
%% ###############################################################

%% @doc Updates session counter for given application
-spec add(binary(), pid()) -> ok.
add(Key, SessionProcess) ->
    ProcessName = ?B2A(Key),
    supervisor:start_child(session_counter_sup, [ProcessName, Key]),
    gen_server:cast(ProcessName, {add, SessionProcess}).

%% @doc Returns list of active sessions in given time period
-spec active_sessions(binary(), integer() | undefined, integer() | undefined) ->
    {ok, integer()} | {ok, list()} | {error, term()}.
active_sessions(Key, Start, End) when is_integer(Start), is_integer(End) ->
    case application_obj:exists(Key) of
        true ->
            session_counter_stats:read(Key, Start, End);
        false ->
            {error, application_not_found}
    end;

%% @doc Returns list of current active sessions
active_sessions(Key, _, _) ->
    case application_obj:exists(Key) of
        true ->
            try gen_server:call(?B2A(Key), get_counter) of
                Res -> {ok, Res}
            catch
                exit:{noproc, _} -> {ok, 0};
                _:Reason -> {error, Reason}
            end;
        false ->
            {error, application_not_found}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
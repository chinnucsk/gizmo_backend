-module(session_api_active_sessions).
-author('mkorszun@gmail.com').

-export([init/3, content_types_provided/2]).
-export([resource_info/0]).
-export([active_sessions_to_json/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, active_sessions_to_json}], Req, State}.

resource_info() ->
    {"/application/:application/active_sessions", [], ?MODULE, []}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

active_sessions_to_json(ReqData, State) ->
    {Key, _} = cowboy_req:binding(application, ReqData),
    {Start, _} = cowboy_req:qs_val(<<"start">>, ReqData, undefined),
    {End, _} = cowboy_req:qs_val(<<"end">>, ReqData, undefined),
    case session_counter_api:active_sessions(Key, ?B2I(Start), ?B2I(End)) of
        {ok, Res} ->
            {mochijson2:encode({struct, [{active_sessions, Res}]}), ReqData, State};
        {error, application_not_found} ->
            ?ERR("Read active sessions for ~s: key not found", [Key]),
            session_api_response:error(application_not_found, 404, ReqData, State);
        {error, Reason} ->
            ?ERR("Read active sessions for ~s: ~p", [Key, Reason]),
            session_api_response:error(internal_error, 500, ReqData, State)
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
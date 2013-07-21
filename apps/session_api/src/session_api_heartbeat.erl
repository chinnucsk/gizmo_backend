-module(session_api_heartbeat).
-author('mkorszun@gmail.com').

-export([init/3, allowed_methods/2]).
-export([resource_info/0]).
-export([handle/2, terminate/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init(_Transport, Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_info() ->
    Path = "/application/:application/device/:device/heartbeat",
    Constraints = [{device, function, fun(V) ->
        {true, ?B2A(<<<<"device">>/binary, V/binary>>)}
    end}],
    {Path, Constraints, ?MODULE, []}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

handle(ReqData, State) ->
    {Key, _} = cowboy_req:binding(application, ReqData),
    {Device, _} = cowboy_req:binding(device, ReqData),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, ReqData, undefined),
    case session_server_api:heartbeat(Key, Device, ?B2I(Timeout)) of
        ok ->
            {ok, ReqData, State};
        {error, application_not_found} ->
            ?ERR("Heartbeat for ~s/~s failed: application not found", [Key, Device]),
            session_api_response:error(application_not_found, 404, ReqData, State);
        {error, Reason} ->
            ?ERR("Heartbeat for ~s/~s failed: ~p", [Key, Device, Reason]),
            session_api_response:error(internal_error, 500, ReqData, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################
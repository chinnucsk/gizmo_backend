-module(session_api_heartbeat_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [test_heartbeat_ok, test_heartbeat_app_not_found, test_heartbeat_error].

init_per_suite(Config) ->
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),
    ok = application:start(inets),
    application:set_env(session_api, http_server_addr, {127,0,0,1}),
    application:set_env(session_api, http_server_port, 8888),
    ok = application:start(session_api),
    ok = application:start(meck),
    Config.

end_per_suite(_) ->
    ok = application:stop(meck),
    ok = application:stop(session_api),
    ok = application:stop(inets),
    ok = application:stop(cowboy),
    ok = application:stop(crypto),
    ok = application:stop(ranch).

%% ###############################################################
%% TESTS
%% ###############################################################

test_heartbeat_ok(_) ->
    meck:new(application_obj),
    meck:expect(application_obj, exists, fun(_) -> true end),
    meck:new(session_server_api),
    meck:expect(session_server_api, heartbeat, fun(_,_,_) -> ok end),
    {ok, {{"HTTP/1.1", 204, "No Content"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/device/1234/heartbeat?timeout=1000"),
    [{_, {application_obj, exists, [<<"key">>]}, true}] = meck:history(application_obj),
    [{_, {session_server_api, heartbeat, [<<"key">>, device1234, 1000]}, ok}] = meck:history(session_server_api),
    meck:validate(application_obj),
    meck:validate(session_server_api),
    meck:unload(application_obj),
    meck:unload(session_server_api).

test_heartbeat_app_not_found(_) ->
    meck:new(application_obj),
    meck:expect(application_obj, exists, fun(_) -> false end),
    {ok, {{"HTTP/1.1", 404, "Not Found"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/device/1234/heartbeat?timeout=1000"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"application_not_found">>}]} = mochijson2:decode(Body),
    [{_, {application_obj, exists, [<<"key">>]}, false}] = meck:history(application_obj),
    meck:validate(application_obj),
    meck:unload(application_obj).

test_heartbeat_error(_) ->
    meck:new(application_obj),
    meck:expect(application_obj, exists, fun(_) -> true end),
    meck:new(session_server_api),
    meck:expect(session_server_api, heartbeat, fun(_,_,_) -> {error, reason} end),
    {ok, {{"HTTP/1.1", 500, "Internal Server Error"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/device/1234/heartbeat?timeout=1000"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"internal_error">>}]} = mochijson2:decode(Body),
    [{_, {application_obj, exists, [<<"key">>]}, true}] = meck:history(application_obj),
    [{_, {session_server_api, heartbeat, [<<"key">>, device1234, 1000]}, {error, reason}}] = meck:history(session_server_api),
    meck:validate(application_obj),
    meck:validate(session_server_api),
    meck:unload(application_obj),
    meck:unload(session_server_api).

%% ###############################################################
%% ###############################################################
%% ###############################################################
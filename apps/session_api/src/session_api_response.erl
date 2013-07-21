-module(session_api_response).
-author('mkorszun@gmail.com').

-export([to_json/2, error/4]).

%% ###############################################################
%% ERROR FORMATTERS
%% ###############################################################

to_json(Content, ReqData) ->
    cowboy_req:set_resp_header(
        <<"content-type">>, <<"application/json">>,
        cowboy_req:set_resp_body(Content, ReqData)).

error(Reason, Code, ReqData, State) ->
    Body = mochijson2:encode({struct, [{error, Reason}]}),
    {ok, ReqData1} = cowboy_req:reply(Code, [], Body, ReqData),
    {halt, ReqData1, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################
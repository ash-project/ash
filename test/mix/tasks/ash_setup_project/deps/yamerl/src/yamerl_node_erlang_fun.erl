%-
% Copyright (c) 2012-2014 Yakaz
% Copyright (c) 2016-2022 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

%% @private

-module(yamerl_node_erlang_fun).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:yamerl,2012:fun").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

construct_token(#yamerl_constr{detailed_constr = Detailed},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} ->
                    {value, Fun, _} = erl_eval:exprs(Exprs, []),
                    Node = if
                        not Detailed ->
                            Fun;
                        true ->
                            Pres = yamerl_constr:get_pres_details(Token),
                            #yamerl_erlang_fun{
                              module   = ?MODULE,
                              tag      = ?TAG,
                              pres     = Pres,
                              function = Fun,
                              text     = Text
                            }
                    end,
                    {finished, Node};
                {error, {Line, _, Desc}} when Line >= 1 ->
                    Error = #yamerl_parsing_error{
                      name   = invalid_erlang_fun2,
                      token  = Token,
                      text   = lists:flatten(erl_parse:format_error(Desc)),
                      line   = ?TOKEN_LINE(Token) + Line - 1
                    },
                    throw(Error)
            end;
        {error, {Line, _, Desc}, _} when Line >= 1 ->
            Error = #yamerl_parsing_error{
              name   = invalid_erlang_fun1,
              token  = Token,
              text   = lists:flatten(erl_scan:format_error(Desc)),
              line   = ?TOKEN_LINE(Token) + Line - 1
            },
            throw(Error)
    end;
construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_erlang_fun,
      token  = Token,
      text   = "Invalid Erlang anonymous function",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).

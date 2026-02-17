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

-module(yamerl_node_int).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1,
    string_to_integer/1
  ]).

%% Internal use only.
-export([
    base8_to_integer/2,
    base10_to_integer/2,
    base16_to_integer/2
  ]).

-define(TAG, "tag:yaml.org,2002:int").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yamerl_parsing_error{name = not_an_integer} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{detailed_constr = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
        error ->
            exception(Token);
        Int ->
            {finished, Int}
    end;
construct_token(#yamerl_constr{detailed_constr = true},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
        error ->
            exception(Token);
        Int ->
            Pres = yamerl_constr:get_pres_details(Token),
            Node = #yamerl_int{
              module = ?MODULE,
              tag    = ?TAG,
              pres   = Pres,
              value  = Int
            },
            {finished, Node}
    end;

construct_token(_, _, Token) ->
    exception(Token).

node_pres(Node) ->
    ?NODE_PRES(Node).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

%% Sign.
string_to_integer([$+ | Text]) ->
    string_to_integer2(Text);
string_to_integer([$- | Text]) ->
    case string_to_integer2(Text) of
        error -> error;
        Int   -> -Int
    end;
string_to_integer(Text) ->
    string_to_integer2(Text).

%% Base.
string_to_integer2("0x" ++ Text) ->
    base16_to_integer(Text, 0);
string_to_integer2("0o" ++ Text) ->
    base8_to_integer(Text, 0);
string_to_integer2(Text) ->
    base10_to_integer(Text, 0).

%% Parsing.
base10_to_integer([C | Rest], Int) when C >= $0 andalso C =< $9 ->
    Int1 = (Int * 10) + (C - $0),
    base10_to_integer(Rest, Int1);
base10_to_integer([], Int) ->
    Int;
base10_to_integer(_, _) ->
    error.

base8_to_integer([C | Rest], Int) when C >= $0 andalso C =< $7 ->
    Int1 = (Int * 8) + (C - $0),
    base8_to_integer(Rest, Int1);
base8_to_integer([], Int) ->
    Int;
base8_to_integer(_, _) ->
    error.

base16_to_integer([C | Rest], Int) when C >= $0 andalso C =< $9 ->
    Int1 = (Int * 16) + (C - $0),
    base16_to_integer(Rest, Int1);
base16_to_integer([C | Rest], Int) when C >= $a andalso C =< $f ->
    Int1 = (Int * 16) + (C - $a + 10),
    base16_to_integer(Rest, Int1);
base16_to_integer([C | Rest], Int) when C >= $A andalso C =< $F ->
    Int1 = (Int * 16) + (C - $A + 10),
    base16_to_integer(Rest, Int1);
base16_to_integer([], Int) ->
    Int;
base16_to_integer(_, _) ->
    error.

exception(Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_integer,
      token  = Token,
      text   = "Invalid integer",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

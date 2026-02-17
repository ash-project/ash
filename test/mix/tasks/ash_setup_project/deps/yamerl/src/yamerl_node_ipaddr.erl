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

-module(yamerl_node_ipaddr).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("yamerl_nodes_yamerl_extensions.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1,
    parse/1
  ]).

-define(TAG, "tag:yamerl,2012:ipaddr").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yamerl_parsing_error{name = not_an_ip_address} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{detailed_constr = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_ip(Token, Text) of
        {range, IP1, IP2} ->
            {finished, {IP1, IP2}};
        {netmask, IP, Mask} ->
            {finished, {IP, Mask}};
        IP ->
            {finished, IP}
    end;
construct_token(#yamerl_constr{detailed_constr = true},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = case string_to_ip(Token, Text) of
        {range, IP1, IP2} ->
            #yamerl_ip_range{
              module  = ?MODULE,
              tag     = ?TAG,
              pres    = Pres,
              from    = IP1,
              to      = IP2
            };
        {netmask, IP, Mask} ->
            #yamerl_ip_netmask{
              module  = ?MODULE,
              tag     = ?TAG,
              pres    = Pres,
              address = IP,
              mask    = Mask
            };
        IP ->
            #yamerl_ip_addr{
              module  = ?MODULE,
              tag     = ?TAG,
              pres    = Pres,
              address = IP
            }
    end,
    {finished, Node};

construct_token(_, _, Token) ->
    exception(Token).

node_pres(Node) ->
    ?NODE_PRES(Node).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

string_to_ip(Token, Text) ->
    %% Check fir IP addresses range: "${IP1} ${IP2}".
    case string:tokens(Text, " ") of
        [_] ->
            %% Maybe a netmask.
            string_to_ip2(Token, Text);
        [Start, End] ->
            %% Range.
            {range,
              string_to_ip3(Token, Start),
              string_to_ip3(Token, End)};
        _ ->
            Error = #yamerl_parsing_error{
              name   = not_an_ip_address,
              token  = Token,
              text   = "Invalid IP addresses range",
              line   = ?TOKEN_LINE(Token),
              column = ?TOKEN_COLUMN(Token)
            },
            throw(Error)
    end.

string_to_ip2(Token, Text) ->
    %% Check for a netmask: "${IP}/${Mask}".
    case string:tokens(Text, "/") of
        [_] ->
            %% Probably an IP.
            string_to_ip3(Token, Text);
        [IP, Mask] ->
            try
                {netmask,
                  string_to_ip3(Token, IP),
                  list_to_integer(Mask)}
            catch
                _:badarg ->
                    Error = #yamerl_parsing_error{
                      name   = not_an_ip_address,
                      token  = Token,
                      text   = "Invalid netmask",
                      line   = ?TOKEN_LINE(Token),
                      column = ?TOKEN_COLUMN(Token)
                    },
                    throw(Error)
            end;
        _ ->
            Error = #yamerl_parsing_error{
              name   = not_an_ip_address,
              token  = Token,
              text   = "Invalid netmask",
              line   = ?TOKEN_LINE(Token),
              column = ?TOKEN_COLUMN(Token)
            },
            throw(Error)
    end.

string_to_ip3(Token, Text) ->
    case parse(Text) of
        {ok, Address} ->
            Address;
        _ ->
            Error = #yamerl_parsing_error{
              name   = not_an_ip_address,
              token  = Token,
              text   = "Invalid IP address",
              line   = ?TOKEN_LINE(Token),
              column = ?TOKEN_COLUMN(Token)
            },
            throw(Error)
    end.

parse(Text) ->
    case inet_parse:ipv4strict_address(Text) of
        {ok, IP} -> {ok, IP};
        _        -> inet_parse:ipv6strict_address(Text)
    end.

exception(Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_ip_address,
      token  = Token,
      text   = "Invalid IP address",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

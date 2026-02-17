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

%% @author Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
%% @copyright
%% 2012-2014 Yakaz,
%% 2016-2022 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
%%
%% @doc {@module} implements the <em>yamerl</em> API above {@link
%% yamerl_constr}.
%%
%% See <em>yamler</em> documentation: [https://github.com/goertzenator/yamler].

-module(yamerl_yamler_compat).

-include("yamerl_errors.hrl").

%% Public API.
-export([
    load/1,
    load/2,
    load_file/1,
    load_file/2,
    convert_options/1,
    convert_options/2
  ]).

-type yamler_schema() :: yaml_schema_failsafe
                       | yaml_schema_json
                       | yaml_schema_core
                       | yaml_schema_erlang.
-type yamler_option() :: {schema, yamler_schema()}
                       | {implicit_atoms, boolean()}
                       | implicit_atoms.

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

%% @equiv load(String, [])

-spec load(String) ->
        Result | Error when
          String  :: binary(),
          Result  :: {ok, [yamerl_constr:yamerl_simple_doc()]},
          Error   :: {error, Message},
          Message :: string().

load(String) ->
    load(String, []).

%% @doc Constructs a YAML document from an in-memory string.
%%
%% The `yamerl' application must be started to use this wrapper.

-spec load(String, Options) ->
        Result | Error when
          String  :: binary(),
          Options :: [yamler_option()],
          Result  :: {ok, [yamerl_constr:yamerl_simple_doc()]},
          Error   :: {error, Message},
          Message :: string().

load(String, Options) ->
    Options1 = convert_options(Options),
    try
        Result = yamerl_constr:string(String, Options1),
        {ok, Result}
    catch
        throw:#yamerl_exception{errors = [Error]} ->
            format_error(Error)
    end.

%% @equiv load_file(Filename, [])

-spec load_file(Filename) ->
        Result | Error when
          Filename :: string(),
          Result   :: {ok, [yamerl_constr:yamerl_simple_doc()]},
          Error    :: {error, Message},
          Message  :: string().

load_file(Filename) ->
    load_file(Filename, []).

%% @doc Constructs a YAML document from a regular file.

-spec load_file(Filename, Options) ->
        Result | Error when
          Filename :: string(),
          Options  :: [yamler_option()],
          Result   :: {ok, [yamerl_constr:yamerl_simple_doc()]},
          Error    :: {error, Message},
          Message  :: string().

load_file(Filename, Options) ->
    Options1 = convert_options(Options),
    try
        Result = yamerl_constr:file(Filename, Options1),
        {ok, Result}
    catch
        throw:#yamerl_exception{errors = [Error]} ->
            format_error(Error)
    end.

%% @private

-spec convert_options(Options) ->
        Converted when
          Options   :: [yamler_option()],
          Converted :: [
            yamerl_parser:yamerl_parser_option() |
            yamerl_constr:yamerl_constr_option() |
            proplists:property()
          ].

convert_options(Options) ->
    convert_options(Options, []).

%% @private

convert_options([{schema, yaml_schema_failsafe} | Rest], Converted) ->
    Converted1 = [
      {schema, failsafe}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_json} | Rest], Converted) ->
    Converted1 = [
      {schema, json}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_core} | Rest], Converted) ->
    Converted1 = [
      {schema, yaml11}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_erlang} | Rest], Converted) ->
    Converted1 = [
      {schema, yaml11},
      {node_mods, [yamerl_node_erlang_atom]}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([implicit_atoms | Rest], Converted) ->
    Converted1 = [
      {node_mods, [yamerl_node_erlang_atom]},
      erlang_atom_autodetection
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{implicit_atoms, true} | Rest], Converted) ->
    Converted1 = [
      {node_mods, [yamerl_node_erlang_atom]},
      erlang_atom_autodetection
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([], Converted) ->
    Converted1 = case proplists:get_value(schema, Converted) of
        undefined -> [{schema, yaml11} | Converted];
        _         -> Converted
    end,
    [
      {doc_version, {1, 1}},
      {detailed_constr, false},
      str_node_as_binary,
      inf_float_node_like_yamler
      | Converted1
    ].

format_error(#yamerl_parsing_error{text = Text, line = Line, column = Col}) ->
    Message = lists:flatten(
      io_lib:format("~s, Line ~b, Column ~b", [Text, Line, Col])),
    {error, Message};
format_error(#yamerl_invalid_option{text = Text}) ->
    {error, Text}.

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
%% @doc {@module} implements a YAML constructor. It uses {@link
%% yamerl_parser} as the underlying parser. The parser emits YAML nodes
%% which are assembled as structured YAML documents by the constructor.
%%
%% It is able to construct YAML documents from in-memory strings (see
%% {@link string/1} and {@link string/2}), regular files (see {@link
%% file/1} and {@link file/2}) or streams (see {@link new/1}, {@link
%% new/2} and {@link next_chunk/3}).
%%
%% YAML documents can be constructed in simple or detailed modes. In
%% simple mode, they are made of simple builting Erlang types. In
%% detailed mode, they are made of records, holding more information
%% about YAML nodes and their presentation.
%%
%% The `yamerl' application must be started to use the constructor.
%%
%% <strong>Example: parse a string in simple mode</strong>
%% ```
%% yamerl_constr:string("Hello!").
%% '''
%%
%% It returns:
%% ```
%% % List of documents; here, only one.
%% [
%%   % Document root node: a string.
%%   "Hello!"
%% ].
%% '''
%%
%% <strong>Example: parse a stream in detailed mode</strong>
%% ```
%% Stream_St1 = yamerl_constr:new({file, "<stdin>"}, [{detailed_constr, true}]),
%% {continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1, <<"He">>),
%% {continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2, <<"ll">>),
%% yamerl_constr:last_chunk(Stream_St3, <<"o!">>).
%% '''
%%
%% It returns:
%% ```
%% % List of documents; here, only one.
%% [
%%   % Document #1.
%%   {yamerl_doc,
%%     % Document root node: a string.
%%     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str",
%%       [{line, 1}, {column, 1}], % Node location in the original string.
%%       "Hello!"                  % String value.
%%     }
%%   }
%% ].
%% '''

-module(yamerl_constr).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    new/1,
    new/2,
    string/1,
    string/2,
    file/1,
    file/2,
    next_chunk/3,
    next_chunk/2,
    last_chunk/2,
    get_pres_details/1,
    node_line/1,
    node_column/1,
    option_names/0
  ]).

%% -------------------------------------------------------------------
%% Exported types.
%% -------------------------------------------------------------------

%% FIXME:
%% This type should be "-opaque". However, up-to Erlang R15B03, an issue
%% with either this code or Dialyzer prevents us from declaring it
%% properly: Dialyzer reports warning regarding the stream_state_fun()
%% type and several guard expression which will never match.
-type yamerl_constr() :: #yamerl_constr{}.

-export_type([
    yamerl_constr/0,
    yamerl_constr_option/0,

    yamerl_node/0,
    yamerl_seq/0,
    yamerl_map/0,
    yamerl_str/0,
    yamerl_null/0,
    yamerl_bool/0,
    yamerl_int/0,
    yamerl_float/0,
    yamerl_binary/0,
    yamerl_timestamp/0,
    yamerl_erlang_atom/0,
    yamerl_erlang_fun/0,
    yamerl_user_node/0,
    yamerl_doc/0,

    yamerl_simple_node/0,
    yamerl_simple_seq/0,
    yamerl_simple_map/0,
    yamerl_simple_str/0,
    yamerl_simple_null/0,
    yamerl_simple_bool/0,
    yamerl_simple_int/0,
    yamerl_simple_float/0,
    yamerl_simple_timestamp/0,
    yamerl_simple_erlang_atom/0,
    yamerl_simple_erlang_fun/0,
    yamerl_user_simple_node/0,
    yamerl_simple_doc/0
  ]).

%% -------------------------------------------------------------------
%% Public API: chunked stream scanning.
%% -------------------------------------------------------------------

%% @equiv new(Source, [])

-spec new(Source) ->
        Constr | no_return() when
          Source :: term(),
          Constr :: yamerl_parser:yamerl_parser().

new(Source) ->
    new(Source, []).

%% @doc Creates and returns a new YAML construction state.
%%
%% When you want to parse a stream (as opposed to in-memory strings or
%% regular files), this is the first function you call before feeding
%% the constructor with stream "chunks".
%%
%% `Source' can be any term describing the stream. {@link string/1} and
%% {@link string/2} sets it to the atom `string'. {@link file/1} and
%% {@link file/2} sets it to `{file, Filename}'. The constructor doesn't
%% use that value.
%%
%% `Options' is a list of options for the parser and the constructor.
%% Valid options are:
%%
%% <dl>
%% <dt>`{detailed_constr, boolean()}'</dt>
%% <dd>Flag to enable/disable the detailed construction mode. In simple
%% construction mode, YAML nodes are returned as Erlang integers,
%% strings, lists, proplists, etc. In other words, only simple builtin
%% types. In detailed construction mode, YAML nodes are returned using
%% records. Those records gives additional information such as the YAML
%% node type, the location in the stream (line and column number) and so
%% on.</dd>
%% <dt>`{ignore_unrecognized_tags, boolean()}'</dt>
%% <dd>Indicate if unrecognized tags should be ignored. When `false'
%% (the default), a node with an unrecognized tag can't be constructed
%% because yamerl doesn't know how to interpret the node. When this
%% happens an exception is raised. When set to `true', the node is
%% constructed as if it was a plain YAML node without any tag.</dd>
%% <dd>Default: `false'.</dd>
%% <dt>`{keep_duplicate_keys, boolean()}'</dt>
%% <dd>Flag to keep duplicate keys in maps. By default all duplicate keys
%% in maps/proplists will be ignored and the last occurence of a key will
%% prevail. If this flag is enabled all keys will remain. This flag only
%% works when the `detailed_constr' flag is set to `true' or proplists
%% are used instead of maps.</dd>
%% <dd>Default: `false'</dd>
%% <dt>`{node_mods, Mods_List}'</dt>
%% <dd>List of Erlang modules to extend support node types.</dd>
%% <dd>Default: `[]'.</dd>
%% <dt>`{schema, failsafe | json | core | yaml11}'</dt>
%% <dd>Name of the official schema to use.</dd>
%% <dd>Default: `core'.</dd>
%% </dl>
%%
%% The returned state is opaque value. You then pass it to {@link
%% next_chunk/2}, {@link next_chunk/3} and {@link last_chunk/2}.
%%
%% If an option is invalid, an exception is thrown.
%%
%% <strong>Example: parse a valid stream</strong>
%% ```
%% Stream_St1 = yamerl_constr:new({file, "<stdin>"}),
%% {continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1, <<"He">>),
%% {continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2, <<"ll">>),
%% yamerl_constr:last_chunk(Stream_St3, <<"o!">>).
%% '''
%% It returns:
%%
%% ```
%% % List of documents; here, only one.
%% [
%%   % Document root node: a string.
%%   "Hello!"
%% ].
%% '''
%%
%% <strong>Example: parse an invalid stream</strong>
%% ```
%% Stream_St1 = yamerl_constr:new({file, "<stdin>"}),
%% {continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1, <<"'He">>),
%% {continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2, <<"ll">>),
%% yamerl_constr:last_chunk(Stream_St3, <<"o!">>) % Unfinished single-quoted scalar.
%% '''
%%
%% It throws:
%% ```
%% {yamerl_exception,
%%   % List of warnings and errors; here, one fatal error.
%%   [
%%     % Error #1.
%%     {yamerl_parsing_error, error,
%%       "Unexpected end-of-stream while parsing flow scalar",          % Human-readable message.
%%       1, 8,                                                          % Error location.
%%       unexpected_eos,
%%       {yamerl_scalar, 1, 1, {yamerl_tag, 1, 1, {non_specific, "!"}}, % Token being parsed.
%%         flow, single_quoted,
%%         "Hello!"},
%%       []
%%     }
%%   ]
%% }
%% '''
%%
%% @see new/1.

-spec new(Source, Options) ->
    Constr | no_return() when
      Source  :: term(),
      Options :: [
        yamerl_constr_option() |
        yamerl_parser:yamerl_parser_option() |
        proplists:property()
      ],
      Constr  :: yamerl_parser:yamerl_parser().

new(Source, Options) ->
    Parser_Options = initialize(Options),
    yamerl_parser:new(Source, Parser_Options).

%% @equiv next_chunk(Constr, Chunk, false)

-spec next_chunk(Constr, Chunk) ->
    Ret | no_return() when
      Constr     :: yamerl_parser:yamerl_parser(),
      Chunk      :: unicode_binary(),
      Ret        :: {continue, New_Constr},
      New_Constr :: yamerl_parser:yamerl_parser().

next_chunk(Constr, Chunk) ->
next_chunk(Constr, Chunk, false).

%% @doc Feeds the constructor with the next chunk from the YAML stream.
%%
%% `Constr' is the constructor state returned by a previous call
%% to {@link new/1}, {@link new/2}, {@link next_chunk/2} or {@link
%% next_chunk/3}.
%%
%% `Chunk' must be an Erlang binary using the UTF-8, UTF-16 or UTF-32
%% Unicode encoding. A leading BOM character in the first chunk is used
%% to determine the encoding and endianness. If no BOM is present, UTF-8
%% is assumed.
%%
%% `EOS' indicates the constructor if this is the last chunk from the
%% stream.
%%
%% If this is not the last chunk (`EOS = false'), it returns `{continue,
%% New_Constr}' where `New_Constr' is an updated state which replaces
%% `Constr'. The new state is to be passed to future calls to {@link
%% next_chunk/2}, {@link next_chunk/3} or {@link last_chunk/2}.
%%
%% If this is the last chunk (`EOS = true'), it returns a list of YAML
%% documents. Documents are made of simple builtin Erlang types if the
%% detailed construction mode is disabled, or records if the detailed
%% construction mode is enabled (`{detailed_constr, boolean()}' passed
%% as an option; default is `false').
%%
%% It throws an exception if there is a parsing or construction error.

-spec next_chunk(Constr, Chunk, false) ->
        Ret | no_return() when
          Constr     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Ret        :: {continue, New_Constr},
          New_Constr :: yamerl_parser:yamerl_parser();
      (Constr, Chunk, true) ->
        Result | no_return() when
          Constr     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Result     :: [yamerl_doc()]
                      | [yamerl_simple_doc()].

next_chunk(Constr, Chunk, EOS) ->
    Ret = yamerl_parser:next_chunk(Constr, Chunk, EOS),
    if
        EOS  -> get_docs(Ret);
        true -> Ret
    end.

%% @equiv next_chunk(Constr, Chunk, true)

-spec last_chunk(Constr, Chunk) ->
        Result | no_return() when
          Constr     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Result     :: [yamerl_doc()]
                      | [yamerl_simple_doc()].

last_chunk(Constr, Chunk) ->
    next_chunk(Constr, Chunk, true).

-spec get_docs(Constr) ->
        Docs | no_return() when
          Constr :: yamerl_parser:yamerl_parser(),
          Docs   :: [yamerl_doc()]
                  | [yamerl_simple_doc()].

get_docs(Constr) ->
    case yamerl_parser:get_token_fun(Constr) of
        Not_Fun when Not_Fun == acc orelse Not_Fun == drop ->
            Error = #yamerl_parsing_error{
              name = token_fun_cleared
            },
            yamerl_errors:throw(Error);
        Token_Fun ->
            Token_Fun(get_docs)
    end.

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

%% @equiv string(String, [])

-spec string(String) ->
        Result | no_return() when
          String :: unicode_data(),
          Result :: [yamerl_doc()]
                  | [yamerl_simple_doc()]
                  | term().

string(String) ->
    string(String, []).

%% @doc Constructs a YAML document from an in-memory YAML string.
%%
%% `String' must be an Erlang list or binary containing one or more YAML
%% documents. If it is a binary, it must be encoded using UTF-8, UTF-16
%% or UTF-32. A leading BOM character is used to determine the encoding
%% and endianness. If no BOM is present, UTF-8 is assumed.
%%
%% `Options' is a list of options for the parser and the constructor.
%% See {@link new/2} for valid options.
%%
%% It returns a list of YAML documents. See {@link next_chunk/3} for
%% more details about the returned documents.
%%
%% It throws an exception if there is a parsing or construction error.
%%
%% <strong>Example: parse an Erlang list</strong>
%% ```
%% yamerl_constr:string("This is a string").
%% '''
%%
%% <strong>Example: parse an UTF-8-encoded Erlang binary</strong>
%% ```
%% yamerl_constr:string(<<50,32,226,130,172>>). % The string "2 €" encoded in UTF-8.
%% '''
%%
%% <strong>Example: parse a string in simple mode</strong>
%% ```
%% yamerl_constr:string("Hello!").
%% '''
%%
%% It returns:
%% ```
%% % List of documents; here, only one.
%% [
%%   % Document root node: a string.
%%   "Hello!"
%% ].
%% '''
%%
%% <strong>Example: parse a string in detailed mode</strong>
%% ```
%% yamerl_constr:string("Hello!", [{detailed_constr, true}]).
%% '''
%%
%% It returns:
%% ```
%% % List of documents; here, only one.
%% [
%%   % Document #1.
%%   {yamerl_doc,
%%     % Document root node: a string.
%%     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str",
%%       [{line, 1}, {column, 1}], % Node location in the original string.
%%       "Hello!"                  % String value.
%%     }
%%   }
%% ].
%% '''
%%
%% <strong>Example: parse an invalid document</strong>
%% ```
%% yamerl_constr:string(<<"'Oh-oh...">>). % Unfinished single-quoted scalar.
%% '''
%%
%% It throws:
%% ```
%% {yamerl_exception,
%%   % List of warnings and errors; here, one fatal error.
%%   [
%%     % Error #1.
%%     {yamerl_parsing_error, error,
%%       "Unexpected end-of-stream while parsing flow scalar",          % Human-readable message.
%%       1, 10,                                                         % Error location.
%%       unexpected_eos,
%%       {yamerl_scalar, 1, 1, {yamerl_tag, 1, 1, {non_specific, "!"}}, % Token being parsed.
%%         flow, single_quoted,
%%         "Oh-oh..."},
%%       []
%%     }
%%   ]
%% }.
%% '''

-spec string(String, Options) ->
        Result | no_return() when
          String  :: unicode_data(),
          Options :: [ yamerl_parser:yamerl_parser_option()
                     | yamerl_constr_option()
                     | proplists:property()],
          Result  :: [yamerl_doc()]
                   | [yamerl_simple_doc()]
                   | term().

string(String, Options) ->
    Parser_Options = initialize(Options),
    Constr = yamerl_parser:string(String, Parser_Options),
    get_docs(Constr).

-spec file(Filename) ->
        Result | no_return() when
          Filename :: string(),
          Result   :: [yamerl_doc()]
                    | [yamerl_simple_doc()]
                    | term().

%% @equiv file(Filename, [])

file(Filename) ->
    file(Filename, []).

%% @doc Constructs a YAML document from a regular file.
%%
%% `Filename' must be a string indicating the filename. The file must
%% contain one or more YAML documents. The file must be encoded using
%% UTF-8, UTF-16 or UTF-32. A leading BOM character is used to determine
%% the encoding and endianness. If no BOM is present, UTF-8 is assumed.
%%
%% `Options' is a list of options for the parser and the constructor.
%% See {@link new/2} for valid options.
%%
%% It returns a list of YAML documents. See {@link next_chunk/3} for
%% more details about the returned documents.
%%
%% It throws an exception if there is a parsing or construction error.
%%
%% See {@link string/2} for some examples.

-spec file(Filename, Options) ->
        Result | no_return() when
          Filename :: string(),
          Options  :: [ yamerl_parser:yamerl_parser_option()
                      | yamerl_constr_option()
                      | proplists:property()],
          Result   :: [yamerl_doc()]
                    | [yamerl_simple_doc()]
                    | term().

file(Filename, Options) ->
    Parser_Options = initialize(Options),
    Constr = yamerl_parser:file(Filename, Parser_Options),
    get_docs(Constr).

%% -------------------------------------------------------------------
%% Presentation details.
%% -------------------------------------------------------------------

%% @doc Returns presentation information in the stream for the given
%% node.
%%
%% This only makes sense when the detailed construction mode is enabled
%% (ie. `{detailed_constr, true}' was passed as an option to {@link
%% new/2}, {@link file/2} or {@link string/2}).

get_pres_details(Token) ->
    Line   = ?TOKEN_LINE(Token),
    Column = ?TOKEN_COLUMN(Token),
    [{line, Line}, {column, Column}].

%% -------------------------------------------------------------------
%% Node information.
%% -------------------------------------------------------------------

%% @doc Returns the line number in the stream for the given node.
%%
%% This only makes sense when the detailed construction mode is enabled
%% (ie. `{detailed_constr, true}' was passed as an option to {@link
%% new/2}, {@link file/2} or {@link string/2}).

node_line(Node) ->
    case node_pres(Node) of
        undefined -> undefined;
        Pres      -> proplists:get_value(line, Pres)
    end.

%% @doc Returns the column number in the stream for the given node.
%%
%% This only makes sense when the detailed construction mode is enabled
%% (ie. `{detailed_constr, true}' was passed as an option to {@link
%% new/2}, {@link file/2} or {@link string/2}).

node_column(Node) ->
    case node_pres(Node) of
        undefined -> undefined;
        Pres      -> proplists:get_value(column, Pres)
    end.

node_pres(Node) when
  is_record(Node, yamerl_seq) orelse
  is_record(Node, yamerl_map) orelse
  is_record(Node, yamerl_str) orelse
  is_record(Node, yamerl_null) orelse
  is_record(Node, yamerl_bool) orelse
  is_record(Node, yamerl_int) orelse
  is_record(Node, yamerl_binary) orelse
  is_record(Node, yamerl_timestamp) orelse
  is_record(Node, yamerl_erlang_atom) orelse
  is_record(Node, yamerl_erlang_fun) ->
    ?NODE_PRES(Node);
node_pres(Node) when is_tuple(Node) ->
    %% For user-defined nodes, we call the module responsible for it.
    Mod = ?NODE_MOD(Node),
    try
        Mod:node_pres(Node)
    catch
        error:undef ->
            undefined
    end.

%% -------------------------------------------------------------------
%% Construction.
%% -------------------------------------------------------------------

construct(Constr, #yamerl_doc_start{version = Version}) ->
    %% Select schema and associated modules, possibly based on the
    %% document version.
    Constr1 = setup_node_mods(Constr, Version),
    %% Prepare a document node.
    Doc = #yamerl_doc{},
    Constr2 = Constr1#yamerl_constr{
      current_doc = [Doc]
    },
    return_new_fun(Constr2);

construct(_, Token) when
  is_record(Token, yamerl_stream_start) orelse
  is_record(Token, yamerl_stream_end) orelse
  is_record(Token, yamerl_yaml_directive) orelse
  is_record(Token, yamerl_tag_directive) orelse
  is_record(Token, yamerl_reserved_directive) orelse
  is_record(Token, yamerl_doc_end) ->
    %% This token doesn't start a node: ignore it.
    ok;

construct(
  #yamerl_constr{current_doc = Doc, current_node_is_leaf = false,
    mods = Mods, tags = Tags} = Constr,
  Token) when Doc /= undefined andalso
  (is_record(Token, yamerl_collection_start) orelse
   is_record(Token, yamerl_scalar)) ->
    %% This token starts a node. We must determine the module to use to
    %% construct this node.
    Tag = case Token of
        #yamerl_collection_start{tag = T} -> T;
        #yamerl_scalar{tag = T}           -> T
    end,
    Ret = case Tag of
        #yamerl_tag{uri = {non_specific, _}} ->
            %% The node has a non-specific tag. We let each module
            %% decides if they want to construct the node.
            try_construct(Constr, Mods, Token);
        #yamerl_tag{uri = URI} ->
            %% We look up this URI in the tag's index.
            IgnoreUnrecognizedTags = proplists:get_value(
              ignore_unrecognized_tags, Constr#yamerl_constr.options, false),
            case proplists:get_value(URI, Tags) of
                Mod when Mod /= undefined ->
                    Mod:construct_token(Constr, undefined, Token);
                undefined when IgnoreUnrecognizedTags ->
                    try_construct(Constr, Mods, Token);
                undefined ->
                    %% This tag isn't handled by anything!
                    Error = #yamerl_parsing_error{
                      name   = unrecognized_node,
                      token  = Tag,
                      line   = ?TOKEN_LINE(Tag),
                      column = ?TOKEN_COLUMN(Tag)
                    },
                    Error1 = yamerl_errors:format(Error,
                      "Tag \"~s\" unrecognized by any module", [URI]),
                    yamerl_errors:throw(Error1)
            end
    end,
    handle_construct_return(Constr, Doc, Ret);

construct(
  #yamerl_constr{current_doc = Doc, current_node_is_leaf = false} = Constr,
  #yamerl_anchor{name = Anchor}) when Doc /= undefined ->
    handle_construct_return(Constr, Doc, #node_anchor{name = Anchor});

construct(
  #yamerl_constr{current_doc = Doc, anchors = Anchors} = Constr,
  #yamerl_alias{name = Alias} = Token) when Doc /= undefined ->
    try
        Node = dict:fetch(Alias, Anchors),
        handle_construct_return(Constr, Doc, {finished, Node})
    catch
        _:_ ->
            %% This alias references a non-existent anchor!
            Error = #yamerl_parsing_error{
              name   = no_matching_anchor,
              token  = Token,
              line   = ?TOKEN_LINE(Token),
              column = ?TOKEN_COLUMN(Token)
            },
            Error1 = yamerl_errors:format(Error,
              "No anchor corresponds to alias \"~s\"", [Alias]),
            yamerl_errors:throw(Error1)
    end;

construct(
  #yamerl_constr{current_doc =
    [#unfinished_node{module = Mod} = Node | Doc]} = Constr,
  Token) ->
    %% This token continues a node. We call the current node's module to
    %% handle it.
    Ret = Mod:construct_token(Constr, Node, Token),
    handle_construct_return(Constr, Doc, Ret).

try_construct(Constr, [Mod | Rest], Token) ->
    case Mod:try_construct_token(Constr, undefined, Token) of
        unrecognized -> try_construct(Constr, Rest, Token);
        Ret          -> Ret
    end;
try_construct(_, [], Token) ->
    Error = #yamerl_parsing_error{
      name   = unrecognized_node,
      token  = Token,
      text   = "No module found to handle node",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    yamerl_errors:throw(Error).

construct_parent(#yamerl_constr{anchors = Anchors} = Constr,
  [#node_anchor{name = Anchor} | Doc], Child) ->
    Anchors1 = dict:store(Anchor, Child, Anchors),
    Constr1  = Constr#yamerl_constr{
      anchors = Anchors1
    },
    construct_parent(Constr1, Doc, Child);
construct_parent(#yamerl_constr{docs = Docs, docs_count = Count} = Constr,
  [#yamerl_doc{} = Doc], Root) ->
    %% This node is the root of the document.
    Doc1 = Doc#yamerl_doc{
      root = Root
    },
    Constr1 = Constr#yamerl_constr{
      docs                 = Docs ++ [Doc1],
      docs_count           = Count + 1,
      current_doc          = undefined,
      current_node_is_leaf = false,
      anchors              = dict:new()
    },
    return_new_fun(Constr1);
construct_parent(Constr, [#unfinished_node{module = Mod} = Node | Doc],
  Child) ->
    %% We call the parent node's module to handle this new child node.
    Ret = Mod:construct_node(Constr, Node, Child),
    handle_construct_return(Constr, Doc, Ret).

handle_construct_return(Constr, Doc, {finished, Node}) ->
    %% Give this node to the parent node.
    construct_parent(Constr, Doc, Node);
handle_construct_return(Constr, Doc, {unfinished, Node, Is_Leaf}) ->
    %% Unfinished node, wait for the next tokens.
    Constr1 = Constr#yamerl_constr{
      current_doc          = [Node | Doc],
      current_node_is_leaf = Is_Leaf
    },
    return_new_fun(Constr1);
handle_construct_return(Constr, Doc, #node_anchor{} = Anchor) ->
    %% Anchor before a (not-yet-started) node, wait this node.
    Constr1 = Constr#yamerl_constr{
      current_doc = [Anchor | Doc]
    },
    return_new_fun(Constr1).

return_new_fun(#yamerl_constr{detailed_constr = Detailed} = Constr) ->
    Fun = fun
        (get_docs) when not Detailed ->
            [Doc#yamerl_doc.root || Doc <- Constr#yamerl_constr.docs];
        (get_docs) ->
            Constr#yamerl_constr.docs;
        (get_constr) ->
            Constr;
        (T) ->
            construct(Constr, T)
    end,
    {ok, Fun}.

%% -------------------------------------------------------------------
%% Node modules.
%% -------------------------------------------------------------------

setup_node_mods(Constr, Version) ->
    Mods1 = umerge_unsorted(
      proplists:get_value(node_mods, Constr#yamerl_constr.options, []),
      yamerl_app:get_param(node_mods)
    ),
    DefaultSchema = case Version of
        {1, 0} -> ?YAML11_SCHEMA_MODS;
        {1, 1} -> ?YAML11_SCHEMA_MODS;
        _      -> ?CORE_SCHEMA_MODS
    end,
    Schema = proplists:get_value(schema, Constr#yamerl_constr.options, auto),
    Mods   = case Schema of
        failsafe -> umerge_unsorted(Mods1, ?FAILSAFE_SCHEMA_MODS);
        json     -> umerge_unsorted(Mods1, ?JSON_SCHEMA_MODS);
        core     -> umerge_unsorted(Mods1, ?CORE_SCHEMA_MODS);
        yaml11   -> umerge_unsorted(Mods1, ?YAML11_SCHEMA_MODS);
        auto     -> umerge_unsorted(Mods1, DefaultSchema)
    end,
    Auto    = filter_autodetection_capable_mods(Mods, []),
    Tags    = index_tags(Mods, []),
    Constr#yamerl_constr{
      mods   = Auto,
      tags   = Tags
    }.

umerge_unsorted(List1, List2) ->
    Fun = fun(Mod, List) ->
        case lists:member(Mod, List) of
            true  -> List;
            false -> List ++ [Mod]
        end
    end,
    lists:foldl(Fun, List1, List2).

filter_autodetection_capable_mods([Mod | Rest], Auto) ->
    catch Mod:module_info(),
    Auto1 = case erlang:function_exported(Mod, try_construct_token, 3) of
        true  -> [Mod | Auto];
        false -> Auto
    end,
    filter_autodetection_capable_mods(Rest, Auto1);
filter_autodetection_capable_mods([], Auto) ->
    lists:reverse(Auto).

index_tags([Mod | Rest], Tags) ->
    try
        Tags1 = index_tags2(Tags, Mod:tags(), Mod),
        index_tags(Rest, Tags1)
    catch
        _:_ ->
            index_tags(Rest, Tags)
    end;
index_tags([], Tags) ->
    Tags.

index_tags2(Tags, [Tag | Rest], Mod) ->
    Tags1 = case lists:keymember(Tag, 1, Tags) of
        false -> [{Tag, Mod} | Tags];
        true  -> Tags
    end,
    index_tags2(Tags1, Rest, Mod);
index_tags2(Tags, [], _) ->
    Tags.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

initialize(Options) ->
    Options0 = proplists:unfold(Options),
    {Constr_Options, Parser_Options, Ext_Options} = filter_options(Options0),
    check_options(Constr_Options),
    Detailed = proplists:get_value(detailed_constr, Constr_Options, false),
    Constr = #yamerl_constr{
      options         = Constr_Options,
      ext_options     = Ext_Options,
      detailed_constr = Detailed
    },
    {ok, Token_Fun} = return_new_fun(Constr),
    [{token_fun, Token_Fun} | Parser_Options].

filter_options(Options) ->
    Constr_Option_Names = option_names(),
    Parser_Option_Names = yamerl_parser:option_names(),
    filter_options2(Options, Constr_Option_Names, Parser_Option_Names,
      [], [], []).

filter_options2([{Name, _} = Option | Rest],
  Constr_Option_Names, Parser_Option_Names,
  Constr_Options, Parser_Options, Ext_Options) ->
    case lists:member(Name, Constr_Option_Names) of
        true ->
            filter_options2(Rest,
              Constr_Option_Names, Parser_Option_Names,
              [Option | Constr_Options], Parser_Options, Ext_Options);
        false ->
            case lists:member(Name, Parser_Option_Names) of
                true ->
                    filter_options2(Rest,
                      Constr_Option_Names, Parser_Option_Names,
                      Constr_Options, [Option | Parser_Options], Ext_Options);
                false ->
                    filter_options2(Rest,
                      Constr_Option_Names, Parser_Option_Names,
                      Constr_Options, Parser_Options, [Option | Ext_Options])
            end
    end;
filter_options2([], _, _, Constr_Options, Parser_Options, Ext_Options) ->
    {
      lists:reverse(Constr_Options),
      lists:reverse(Parser_Options),
      lists:reverse(Ext_Options)
    }.

%% @private

option_names() ->
    [
      node_mods,
      schema,
      detailed_constr,
      ignore_unrecognized_tags
    ].

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({detailed_constr, Flag}) when is_boolean(Flag) ->
    true;
is_option_valid({node_mods, Mods}) when is_list(Mods) ->
    Fun = fun(Mod) ->
        not yamerl_app:is_node_mod(Mod)
    end,
    case lists:filter(Fun, Mods) of
        [] -> true;
        _  -> false
    end;
is_option_valid({schema, Schema})
when Schema == failsafe
orelse Schema == json
orelse Schema == core
orelse Schema == yaml11
orelse Schema == auto ->
    true;
is_option_valid({ignore_unrecognized_tags, Flag}) when is_boolean(Flag) ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yamerl_invalid_option{
      option = Option
    },
    Error1 = case Option of
        {detailed_constr, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"detailed_constr\": "
              "it must be a boolean"
            };
        {node_mods, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"node_mods\": "
              "it must be a list of modules"
            };
        _ ->
            yamerl_errors:format(Error, "Unknown option \"~w\"", [Option])
    end,
    yamerl_errors:throw(Error1).

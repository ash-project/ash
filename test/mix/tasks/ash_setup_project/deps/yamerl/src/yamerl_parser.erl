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
%% @doc {@module} implements a YAML parser. It is not meant to be used
%% directly. Instead, you should use {@link yamerl_constr}.
%%
%% The `yamerl' application must be started to use the parser.

-module(yamerl_parser).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("internal/yamerl_parser.hrl").

%% Public API.
-export([
    new/1,
    new/2,
    string/1,
    string/2,
    file/1,
    file/2,
    next_chunk/2,
    next_chunk/3,
    last_chunk/2,
    get_token_fun/1,
    set_token_fun/2,
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
-type yamerl_parser() :: #yamerl_parser{}.

-export_type([
    yamerl_parser/0,
    yamerl_parser_option/0,
    yamerl_parser_token_fun/0
  ]).

%% -------------------------------------------------------------------
%% Secondary records to store the scanner state.
%% -------------------------------------------------------------------

-record(directive_ctx, {
    line = 1  :: position(),
    col  = 1  :: position(),
    name = "" :: string()
  }).

-record(yaml_directive_ctx, {
    line  = 1 :: position(),
    col   = 1 :: position(),
    major     :: non_neg_integer() | undefined,
    minor     :: non_neg_integer() | undefined
  }).

-record(tag_directive_ctx, {
    line  = 1 :: position(),
    col   = 1 :: position(),
    handle    :: tag_handle() | [] | undefined,
    prefix    :: tag_prefix() | [] | undefined
  }).

-record(reserved_directive_ctx, {
    line       = 1  :: position(),
    col        = 1  :: position(),
    name       = "" :: string(),
    current         :: string() | undefined,
    args       = [] :: [string()],
    args_count = 0  :: non_neg_integer()
  }).

-type whitespace() :: [9 | 10 | 32].

-record(block_scalar_hd_ctx, {
    style      = literal  :: literal | folded,
    line       = 1        :: position(),      %% Line where the token starts.
    col        = 1        :: position(),      %% Column where the token starts.
    chomp                 :: strip | keep | undefined, %% Chomping indicator.
    indent                :: pos_integer()    %% Indentation indicator.
                           | {tmp, pos_integer()} | undefined,
    in_comment = false    :: boolean()        %% Trailing comment.
  }).

-record(block_scalar_ctx, {
    style         = literal :: literal | folded,
    line          = 1       :: position(),    %% Line where the token starts.
    col           = 1       :: position(),    %% Column where the token starts.
    endline       = 1       :: position(),    %% Line where the token ends.
    endcol        = 1       :: position(),    %% Column where the token ends.
    chomp         = clip    :: strip | keep | clip, %% Chomping method.
    indent                  :: pos_integer() | undefined, %% Block indent.
    longest_empty = 0       :: non_neg_integer(), %% Longest leading empty line.
    newline       = false   :: boolean(),     %% Met a newline character.
    spaces        = ""      :: whitespace(),  %% Last white spaces seen.
    more_indent   = false   :: boolean(),     %% Last line is more indented.
    output        = ""      :: string()       %% Already parsed characters.
  }).

-record(flow_scalar_ctx, {
    style        = plain :: double_quoted | single_quoted | plain,
    line         = 1     :: position(),   %% Line where the token starts.
    col          = 1     :: position(),   %% Column where the token starts.
    endline      = 1     :: position(),   %% Line where the token ends.
    endcol       = 1     :: position(),   %% Column where the token ends.
    surrogate            :: 16#d800..16#dbff | undefined, %% High surrogate.
    newline      = false :: boolean(),    %% Met a newline character.
    spaces       = ""    :: whitespace(), %% Last white spaces seen.
    output       = ""    :: string()      %% Already parsed characters.
  }).

-record(anchor_ctx, {
    type        :: anchor | alias,
    line        :: position(),
    col         :: position(),
    output = "" :: string()
  }).

-record(tag_ctx, {
    line   :: position(),
    col    :: position(),
    prefix :: string() | undefined,
    suffix :: string() | tag_uri()
  }).

-define(IO_BLOCKSIZE, 4096). %% Common filesystem blocksize.

-define(FAKE_IMPL_KEY, #impl_key{}).

-define(IN_BLOCK_CTX(P),      (is_record(P#yamerl_parser.cur_coll, bcoll))).
-define(IN_FLOW_CTX(P),       (is_record(P#yamerl_parser.cur_coll, fcoll))).

-define(IS_SPACE(C),          (C == $\s orelse C == $\t)).
-define(IS_NEWLINE(C),        (C == $\n orelse C == $\r)).
-define(IS_NEWLINE_11(C),
  (C == 16#85 orelse C == 16#2028 orelse C == 16#2029)).

-define(IS_FLOW_INDICATOR(C), (
    C == $[ orelse C == $] orelse
    C == ${ orelse C == $} orelse
    C == $,)).

-define(IS_HEXADECIMAL(C), (
    (O1 >= $0 andalso O1 =< $9) orelse
    (O1 >= $a andalso O1 =< $f) orelse
    (O1 >= $A andalso O1 =< $F)
  )).

-define(IS_URI_CHAR(C),
  (
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    C == $- orelse
    C == $% orelse C == $# orelse C == $; orelse C == $/ orelse C == $? orelse
    C == $: orelse C == $@ orelse C == $& orelse C == $= orelse C == $+ orelse
    C == $$ orelse C == $, orelse C == $_ orelse C == $. orelse C == $! orelse
    C == $~ orelse C == $* orelse C == $' orelse C == $( orelse C == $) orelse
    C == $[ orelse C == $]
  )).

-define(IS_BOM(C),            (C == 16#feff)).
-define(IS_HIGH_SURROGATE(C), (C >= 16#d800 andalso C =< 16#dbff)).
-define(IS_LOW_SURROGATE(C),  (C >= 16#dc00 andalso C =< 16#dfff)).

-define(MISSING_ENTRY(S), (
    S#yamerl_parser.pending_entry andalso
    S#yamerl_parser.last_tag == undefined
  )).
-define(MISSING_KVPAIR(S), (
    S#yamerl_parser.waiting_for_kvpair andalso
    not element(#impl_key.possible, hd(S#yamerl_parser.ik_stack))
  )).

-define(IS_JSON_LIKE(T), (
    (is_record(T, yamerl_scalar) andalso
     (T#yamerl_scalar.substyle == single_quoted orelse
      T#yamerl_scalar.substyle == double_quoted)) orelse
    (is_record(T, yamerl_collection_end) andalso
     T#yamerl_collection_end.style == flow)
  )).

-define(DEFAULT_TAG(U, L, C),
  #yamerl_tag{
    uri    = U,
    line   = L,
    column = C
  }).

-define(NEXT_COL(Co, De, Count),
  {Co + Count, De + Count}).

-define(NEXT_LINE(Ch, Li, De, P),
  case Ch of
      [$\r, $\n | R] -> {R, Li + 1, 1, De + 2};
      [_ | R]        -> {R, Li + 1, 1, De + 1}
  end).

%%
%% We use macros instead of functions for a few #yamerl_parser updates
%% to take advantage of the optimization described in §3.5 in the
%% Efficiency Guide.
%%

-define(PUSH_FAKE_IMPL_KEY(P),
  P#yamerl_parser{ik_stack = [?FAKE_IMPL_KEY | P#yamerl_parser.ik_stack]}).

-define(POP_IMPL_KEY(P),
  P#yamerl_parser{ik_stack = tl(P#yamerl_parser.ik_stack)}).

-define(ALLOW_IMPL_KEY(P, F),
  P#yamerl_parser{ik_allowed = F}).

-define(FLUSH_TO_PARSER(Ch, Li, Co, De, P),
  P#yamerl_parser{
    chars     = Ch,
    chars_len = P#yamerl_parser.chars_len - De,
    chars_idx = P#yamerl_parser.chars_idx + De,
    line      = Li,
    col       = Co
  }).

-define(WARN_IF_NON_ASCII_LINE_BREAK(Ch, Li, Co, P),
  case Ch of
      [NL | _] when ?IS_NEWLINE_11(NL) ->
          %% Non-ASCII line break in a YAML 1.2 document.
          Err = #yamerl_parsing_error{
            type   = warning,
            name   = non_ascii_line_break,
            line   = Li,
            column = Co
          },
          add_error(P, Err,
            "Use of non-ASCII line break is not supported anymore starting "
            "with YAML 1.2; treated as non-break character", []);
      _ ->
          P
  end).

-define(BLOCK_SCALAR_DEFAULT_TAG(L, C),
  ?DEFAULT_TAG({non_specific, "!"}, L, C)).
-define(PLAIN_SCALAR_DEFAULT_TAG(L, C),
  ?DEFAULT_TAG({non_specific, "?"}, L, C)).
-define(FLOW_SCALAR_DEFAULT_TAG(L, C),
  ?DEFAULT_TAG({non_specific, "!"}, L, C)).
-define(COLL_SCALAR_DEFAULT_TAG(L, C),
  ?DEFAULT_TAG({non_specific, "?"}, L, C)).

%% -------------------------------------------------------------------
%% Public API: chunked stream scanning.
%% -------------------------------------------------------------------

%% @equiv new(Source, [])

-spec new(Source) ->
        Parser | no_return() when
          Source :: term(),
          Parser :: yamerl_parser().

new(Source) ->
    new(Source, []).

%% @doc Creates and returns a new YAML parser state.

-spec new(Source, Options) ->
        Parser | no_return() when
          Source  :: term(),
          Options :: [yamerl_parser_option()],
          Parser  :: yamerl_parser().

new(Source, Options) ->
    Options0 = proplists:unfold(Options),
    check_options(Options0),
    #yamerl_parser{
      source       = Source,
      options      = Options0,
      stream_state = fun start_stream/5,
      token_fun    = proplists:get_value(token_fun, Options0, acc)
    }.

%% @equiv next_chunk(Parser, Chunk, false)

-spec next_chunk(Parser, Chunk) ->
        Ret | no_return() when
          Parser     :: yamerl_parser(),
          Chunk      :: unicode_binary(),
          Ret        :: {continue, New_Parser},
          New_Parser :: yamerl_parser().

next_chunk(Parser, Chunk) ->
    next_chunk(Parser, Chunk, false).

%% @doc Feeds the parser with the next chunk from the YAML stream.

-spec next_chunk(Parser, Chunk, Last_Chunk) ->
        Ret | no_return() when
          Parser     :: yamerl_parser(),
          Chunk      :: unicode_binary(),
          Last_Chunk :: boolean(),
          Ret        :: {continue, New_Parser} | New_Parser,
          New_Parser :: yamerl_parser().

next_chunk(Parser, <<>>, false) ->
    %% No need to proceed further without any data.
    do_return(Parser);
next_chunk(#yamerl_parser{raw_data = Data} = Parser, Chunk, EOS) ->
    %% Append new data to the remaining data. Those data must then be
    %% decoded to Unicode characters.
    New_Data = list_to_binary([Data, Chunk]),
    Parser1  = Parser#yamerl_parser{
      raw_data = New_Data,
      raw_eos  = EOS
    },
    decode_unicode(Parser1).

%% @equiv next_chunk(Parser, Chunk, true)

-spec last_chunk(Parser, Chunk) ->
        Ret | no_return() when
          Parser     :: yamerl_parser(),
          Chunk      :: unicode_binary(),
          Ret        :: {continue, New_Parser} | New_Parser,
          New_Parser :: yamerl_parser().

last_chunk(Parser, Chunk) ->
    next_chunk(Parser, Chunk, true).

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

%% @equiv string(String, [])

-spec string(String) ->
        Parser | no_return() when
          String :: unicode_data(),
          Parser :: yamerl_parser().

string(String) ->
    string(String, []).

%% @doc Parses a YAML document from an in-memory YAML string.

-spec string(String, Options) ->
        Parser | no_return() when
          String  :: unicode_data(),
          Options :: [yamerl_parser_option()],
          Parser  :: yamerl_parser().

string(String, Options) when is_binary(String) ->
    Parser = new(string, Options),
    next_chunk(Parser, String, true);
string(String, Options) when is_list(String) ->
    string(unicode:characters_to_binary(String), Options).

%% @equiv file(Filename, [])

-spec file(Filename) ->
        Parser | no_return() when
          Filename :: string(),
          Parser   :: yamerl_parser().

file(Filename) ->
    file(Filename, []).

%% @doc Parses a YAML document from a regular file.

-spec file(Filename, Options) ->
        Parser | no_return() when
          Filename :: string(),
          Options  :: [yamerl_parser_option()],
          Parser   :: yamerl_parser().

file(Filename, Options) ->
    Parser    = new({file, Filename}, Options),
    Blocksize = proplists:get_value(io_blocksize, Options, ?IO_BLOCKSIZE),
    case file:open(Filename, [read, binary]) of
        {ok, FD} ->
            %% The file is read in binary mode. The scanner is
            %% responsible for determining the encoding and converting
            %% the stream accordingly.
            file2(Parser, FD, Blocksize);
        {error, Reason} ->
            Error2 = #yamerl_parsing_error{
              name  = file_open_failure,
              extra = [{error, Reason}]
            },
            Parser2 = add_error(Parser, Error2,
              "Failed to open file \"~s\": ~s",
              [Filename, file:format_error(Reason)]),
            do_return(Parser2)
    end.

file2(#yamerl_parser{source = {file, Filename}} = Parser, FD, Blocksize) ->
    case file:read(FD, Blocksize) of
        {ok, Data} ->
            %% If the chunk is smaller than the requested size, we
            %% reached EOS.
            EOS = byte_size(Data) < Blocksize,
            if
                EOS  -> file:close(FD);
                true -> ok
            end,
            try
                case next_chunk(Parser, Data, EOS) of
                    {continue, Parser1} ->
                        file2(Parser1, FD, Blocksize);
                    Parser1 ->
                        Parser1
                end
            catch
                throw:{yamerl_parser, _} = Exception ->
                    %% Close the file and throw the exception again.
                    file:close(FD),
                    throw(Exception)
            end;
        eof ->
            file:close(FD),
            next_chunk(Parser, <<>>, true);
        {error, Reason} ->
            Error = #yamerl_parsing_error{
              name = file_read_failure,
              extra = [{error, Reason}]
            },
            Parser1 = add_error(Parser, Error,
              "Failed to read file \"~s\": ~s",
              [Filename, file:format_error(Reason)]),
            do_return(Parser1)
    end.

%% -------------------------------------------------------------------
%% Public API: get/set the token function.
%% -------------------------------------------------------------------

%% @doc Returns the constructor callback function

get_token_fun(#yamerl_parser{token_fun = Fun}) ->
    Fun.

%% @doc Sets the constructor callback function

set_token_fun(Parser, Fun) when is_function(Fun, 1) ->
    Parser#yamerl_parser{token_fun = Fun}.

%% -------------------------------------------------------------------
%% Determine encoding and decode Unicode.
%% -------------------------------------------------------------------

decode_unicode(#yamerl_parser{stream_state = State,
    encoding = Encoding, raw_data = Data, raw_idx = Raw_Index,
    chars = Chars, chars_len = Chars_Count} = Parser)
  when Encoding /= undefined ->
    %% We have previously determined the encoding of the stream. We can
    %% decode the Unicode characters from the raw data.
    Ret = unicode:characters_to_list(Data, Encoding),
    {Parser2, Chars2} = case Ret of
        {Reason, New_Chars, Remaining_Data} ->
            %% Ok, we have more characters to scan!
            Raw_Index1 = Raw_Index +
              (byte_size(Data) - byte_size(Remaining_Data)),
            Parser1    = Parser#yamerl_parser{
              raw_data  = Remaining_Data,
              raw_idx   = Raw_Index1,
              chars_len = Chars_Count + length(New_Chars)
            },
            Chars1 = Chars ++ New_Chars,
            case Reason of
                incomplete ->
                    {Parser1, Chars1};
                error ->
                    Error = #yamerl_parsing_error{
                      name  = invalid_unicode,
                      extra = [{byte, Raw_Index1 + 1}]
                    },
                    {
                      add_error(Parser1, Error,
                        "Invalid Unicode character at byte #~b",
                        [Raw_Index1 + 1]),
                      Chars1
                    }
            end;
        New_Chars ->
            %% Ok, we have more characters to scan!
            Raw_Index1 = Raw_Index + byte_size(Data),
            Parser1    = Parser#yamerl_parser{
              raw_data  = <<>>,
              raw_idx   = Raw_Index1,
              chars_len = Chars_Count + length(New_Chars)
            },
            Chars1 = Chars ++ New_Chars,
            {Parser1, Chars1}
    end,
    State(Chars2, Parser2#yamerl_parser.line, Parser2#yamerl_parser.col, 0,
      Parser2);
decode_unicode(#yamerl_parser{raw_data = Data, raw_eos = EOS} = Parser)
  when ((EOS == false andalso byte_size(Data) >= 4) orelse EOS == true) ->
    %% We have enough (maybe even all) data to determine the encoding.
    %% Let's check if the stream starts with a BOM.
    {Encoding, Length} = get_encoding(Data),
    %% The stream may start with a BOM: remove it.
    <<_:Length/binary, New_Data/binary>> = Data,
    Parser1 = Parser#yamerl_parser{
      encoding  = Encoding,
      raw_data  = New_Data,
      raw_idx   = Length,
      chars_idx = 1
    },
    decode_unicode(Parser1);
decode_unicode(Parser) ->
    %% We don't have enough data to determine the encoding. We ask for
    %% more data.
    do_return(Parser).

get_encoding(<<16#00, 16#00, 16#fe, 16#ff, _/binary>>) -> {{utf32, big},    4};
get_encoding(<<16#00, 16#00, 16#00, _,     _/binary>>) -> {{utf32, big},    0};
get_encoding(<<16#ff, 16#fe, 16#00, 16#00, _/binary>>) -> {{utf32, little}, 4};
get_encoding(<<_,     16#00, 16#00, 16#00, _/binary>>) -> {{utf32, little}, 0};
get_encoding(<<16#fe, 16#ff, _,     _,     _/binary>>) -> {{utf16, big},    2};
get_encoding(<<16#00, _,     _,     _,     _/binary>>) -> {{utf16, big},    0};
get_encoding(<<16#ff, 16#fe, _,     _,     _/binary>>) -> {{utf16, little}, 2};
get_encoding(<<_,     16#00, _,     _,     _/binary>>) -> {{utf16, little}, 0};
get_encoding(<<16#ef, 16#bb, 16#bf, _,     _/binary>>) -> {utf8,            3};
get_encoding(_)                                        -> {utf8,            0}.

%% -------------------------------------------------------------------
%% Scan characters and emit tokens.
%% -------------------------------------------------------------------

%%
%% Stream start/end.
%%

start_stream(Chars, Line, Col, Delta,
  #yamerl_parser{encoding = Encoding} = Parser) ->
    %% The very first token to emit is the stream start. The stream
    %% encoding is provided as an attribute. The encoding may appear at
    %% the start of each document but can't be changed: all documents
    %% must have the same encoding!
    Parser1 = ?PUSH_FAKE_IMPL_KEY(Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, true),
    Parser3 = setup_default_tags(Parser2),
    Token   = #yamerl_stream_start{
      encoding = Encoding,
      line     = Line,
      column   = Col
    },
    Parser4 = queue_token(Parser3, Token),
    find_next_token(Chars, Line, Col, Delta, Parser4).

end_stream(Chars, Line, Col, Delta,
  #yamerl_parser{last_token_endline = Last_Line,
  last_token_endcol = Last_Col} = Parser) ->
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Chars, Line, Col, Delta,
      Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = ?ALLOW_IMPL_KEY(Parser2, false),
    %% Set the line and column number to the last token endline/endcol
    %% number. This is useful when parsing a file: the last line is
    %% often terminated by a newline character. Thanks to this, the
    %% stream_end token will be on the last token line.
    Token = #yamerl_stream_end{
      line   = Last_Line,
      column = Last_Col
    },
    Parser4 = queue_token(Parser3, Token),
    return(Chars, Line, Col, Delta, Parser4).

%%
%% Next token.
%%

find_next_token(Chars, Line, Col, Delta,
  #yamerl_parser{endpos_set_by_token = true} = Parser) ->
    %% The line and column numbers where the last token ends was already
    %% set during token parsing.
    Parser1 = Parser#yamerl_parser{
      endpos_set_by_token = false
    },
    do_find_next_token(Chars, Line, Col, Delta, Parser1);
find_next_token(Chars, Line, Col, Delta, Parser) ->
    %% Record the line and columns numbers where the last token ends.
    %% It's used to determine if an implicit key would span several
    %% lines and therefore would be unacceptable.
    Parser1 = Parser#yamerl_parser{
      endpos_set_by_token = false,
      last_token_endline  = Line,
      last_token_endcol   = Col
    },
    do_find_next_token(Chars, Line, Col, Delta, Parser1).

%% Skip spaces.
do_find_next_token([$\s | Rest], Line, Col, Delta, Parser) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_find_next_token(Rest, Line, Col1, Delta1, Parser);

%% Skip tabs only when they're separation spaces, not indentation.
do_find_next_token([$\t | Rest], Line, Col, Delta,
  #yamerl_parser{ik_allowed = IK_Allowed} = Parser)
  when ?IN_FLOW_CTX(Parser) orelse not IK_Allowed ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_find_next_token(Rest, Line, Col1, Delta1, Parser);

%% Skip comments.
do_find_next_token([$# | _] = Chars, Line, Col, Delta, Parser) ->
    parse_comment(Chars, Line, Col, Delta, Parser);

%% Continue with next line.
do_find_next_token(Chars, Line, Col, Delta,
  #yamerl_parser{missed_nl = true} = Parser) ->
    Parser1 = Parser#yamerl_parser{
      missed_nl = false
    },
    Parser2 = if
        ?IN_BLOCK_CTX(Parser1) -> ?ALLOW_IMPL_KEY(Parser1, true);
        true                   -> Parser1
    end,
    do_find_next_token(Chars, Line, Col, Delta, Parser2);

do_find_next_token([$\r] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    suspend_parsing(Chars, Line, Col, Delta, Parser, fun do_find_next_token/5);
do_find_next_token([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Chars1, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Parser1 = if
        ?IN_BLOCK_CTX(Parser) -> ?ALLOW_IMPL_KEY(Parser, true);
        true                  -> Parser
    end,
    do_find_next_token(Chars1, Line1, Col1, Delta1, Parser1);

%% End-of-stream reached.
do_find_next_token([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser) ->
    end_stream(Chars, Line, Col, Delta, Parser);

%% Wait for more data.
do_find_next_token([] = Chars, Line, Col, Delta, Parser) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser, fun do_find_next_token/5);

%% Next token found!
do_find_next_token(Chars, Line, Col, Delta, Parser) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    Parser2 = check_for_closed_block_collections(Chars, Line, Col, Delta,
      Parser1, Col),
    determine_token_type(Chars, Line, Col, Delta, Parser2).

%%
%% Token type.
%%

%% Not enough data to determine the token type.
determine_token_type(Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = false} = Parser)
  when (Len - Delta) < 4 ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun determine_token_type/5);

%% BOM, before a document only!
determine_token_type([C | Rest], Line, Col, Delta,
  #yamerl_parser{doc_started = false} = Parser)
  when ?IS_BOM(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser);
determine_token_type([C | Rest], Line, Col, Delta,
  #yamerl_parser{doc_started = true} = Parser)
  when ?IS_BOM(C) ->
    %% A BOM is forbidden after the document start. Because it's not
    %% fatal during parsing, we only add a warning. Note that the YAML
    %% specification considers this to be an error.
    Error = #yamerl_parsing_error{
      type   = warning,
      name   = bom_after_doc_start,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "A BOM must not appear inside a document", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser1);

%% Directives end indicator.
determine_token_type([$-, $-, $-, C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_document_sep(Chars, Line, Col, Delta, Parser, directives_end);
determine_token_type([$-, $-, $-] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser) ->
    parse_document_sep(Chars, Line, Col, Delta, Parser, directives_end);

%% Document end indicator.
determine_token_type([$., $., $., C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_document_sep(Chars, Line, Col, Delta, Parser, document_end);
determine_token_type([$., $., $.] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser) ->
    parse_document_sep(Chars, Line, Col, Delta, Parser, document_end);

%% Directive indicator.
determine_token_type([$% | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_started = false} = Parser) ->
    parse_directive(Chars, Line, Col, Delta, Parser);

%% Flow sequence indicators.
determine_token_type([$[ | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_collection_start(Chars, Line, Col, Delta, Parser, sequence);
determine_token_type([$] | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_collection_end(Chars, Line, Col, Delta, Parser, sequence);

%% Flow mapping indicators.
determine_token_type([${ | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_collection_start(Chars, Line, Col, Delta, Parser, mapping);
determine_token_type([$} | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_collection_end(Chars, Line, Col, Delta, Parser, mapping);

%% Flow collection entry indicator.
determine_token_type([$, | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_entry(Chars, Line, Col, Delta, Parser);

%% Block collection entry indicator.
determine_token_type([$-, C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_block_entry(Chars, Line, Col, Delta, Parser);

%% Mapping key indicator.
determine_token_type([$?, C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_mapping_key(Chars, Line, Col, Delta, Parser);

%% Mapping value indicator.
determine_token_type([$:, C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse ?IS_FLOW_INDICATOR(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_mapping_value(Chars, Line, Col, Delta, Parser);
determine_token_type([$: | _] = Chars, Line, Col, Delta,
  #yamerl_parser{last_is_json_like = true} = Parser)
  when ?IN_FLOW_CTX(Parser) ->
    %% This is a key: value pair indicator only when the last token is
    %% JSON-like and we're in flow context.
    parse_mapping_value(Chars, Line, Col, Delta, Parser);
determine_token_type([$:] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    parse_mapping_value(Chars, Line, Col, Delta, Parser);

%% Anchor and alias indicator.
determine_token_type([$& | _] = Chars, Line, Col, Delta, Parser) ->
    parse_anchor_or_alias(Chars, Line, Col, Delta, Parser, anchor);
determine_token_type([$* | _] = Chars, Line, Col, Delta, Parser) ->
    parse_anchor_or_alias(Chars, Line, Col, Delta, Parser, alias);

%% Tag indicator.
determine_token_type([$! | _] = Chars, Line, Col, Delta, Parser) ->
    parse_tag(Chars, Line, Col, Delta, Parser);

%% Block scalar.
determine_token_type([$| | _] = Chars, Line, Col, Delta, Parser) ->
    parse_block_scalar(Chars, Line, Col, Delta, Parser, literal);
determine_token_type([$> | _] = Chars, Line, Col, Delta, Parser) ->
    parse_block_scalar(Chars, Line, Col, Delta, Parser, folded);

%% Single-quoted flow scalar.
determine_token_type([$' | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_scalar(Chars, Line, Col, Delta, Parser, single_quoted);

%% Double-quoted flow scalar.
determine_token_type([$" | _] = Chars, Line, Col, Delta, Parser) ->
    parse_flow_scalar(Chars, Line, Col, Delta, Parser, double_quoted);

%% Reserved indicators.
%% We add a warning and parse it as a plain scalar.
determine_token_type([C | _] = Chars, Line, Col, Delta, Parser)
  when C == $@ orelse C == $` ->
    Error = #yamerl_parsing_error{
      name   = reserved_indicator,
      type   = warning,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "The reserved indicator \"~c\" is not allowed at the "
      "beginning of a plain scalar", [C]),
    parse_flow_scalar(Chars, Line, Col, Delta, Parser1, plain);

%% Plain flow scalar.
determine_token_type(Chars, Line, Col, Delta, Parser) ->
    parse_flow_scalar(Chars, Line, Col, Delta, Parser, plain).

%% -------------------------------------------------------------------
%% Directives and document ends.
%% -------------------------------------------------------------------

parse_document_sep([_, _, _ | Rest] = Chars, Line, Col, Delta, Parser, Type) ->
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Chars, Line, Col, Delta,
      Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = ?ALLOW_IMPL_KEY(Parser2, false),
    Parser4 = case Type of
        directives_end -> start_doc(Parser3, Line, Col, tail);
        document_end   -> end_doc(Parser3, Line, Col, tail)
    end,
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 3),
    find_next_token(Rest, Line, Col1, Delta1, Parser4).

start_doc(#yamerl_parser{doc_started = true} = Parser,
  Line, Col, Insert_At) ->
    %% A document is already opened: we must close it before starting a
    %% new one.
    Parser1 = end_doc(Parser, Line, Col, Insert_At),
    start_doc(Parser1, Line, Col, next_insert_at(Insert_At, 1));
start_doc(
  #yamerl_parser{options = Options, doc_version = Version,
    tags = Tags} = Parser,
  Line, Col, Insert_At) ->
    %% When a document starts, we set the version to
    %% ?IMPLICIT_DOC_VERSION if no YAML directive were specified.
    Forced   = proplists:get_value(doc_version, Options),
    Version1 = case Version of
        _ when Forced /= undefined -> Forced;
        undefined                  -> ?IMPLICIT_YAML_VERSION;
        _                          -> Version
    end,
    Token = #yamerl_doc_start{
      version = Version1,
      tags    = Tags,
      line    = Line,
      column  = Col
    },
    Parser1 = case Version1 of
        {Major, Minor} when Major < ?MIN_YAML_MAJOR_VERSION_SUPPORTED orelse
        (Major == ?MIN_YAML_MAJOR_VERSION_SUPPORTED andalso
         Minor < ?MIN_YAML_MINOR_VERSION_SUPPORTED) ->
            %% The document's version is not supported at all (below
            %% minimum supported version).
            Error = #yamerl_parsing_error{
              name   = version_not_supported,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (minimum version ~b.~b)",
              [
                Major, Minor,
                ?MIN_YAML_MAJOR_VERSION_SUPPORTED,
                ?MIN_YAML_MINOR_VERSION_SUPPORTED
              ]),
            %% Caution: Chars/Line/Col/Delta aren't flushed to Parser.
            do_return(Parser0);
        {Major, Minor} when
        Major <  ?MAX_YAML_MAJOR_VERSION_SUPPORTED orelse
        (Major == ?MAX_YAML_MAJOR_VERSION_SUPPORTED andalso
         Minor =< ?MAX_YAML_MINOR_VERSION_SUPPORTED) ->
            %% Version supported.
            Parser;
        {Major, Minor} when Major > ?MAX_YAML_MAJOR_VERSION_SUPPORTED ->
            %% The document's version is not supported at all (major
            %% above maximum supported major).
            Error = #yamerl_parsing_error{
              name   = version_not_supported,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (maximum version ~b.~b)",
              [
                Major, Minor,
                ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
                ?MAX_YAML_MINOR_VERSION_SUPPORTED
              ]),
            %% Caution: Chars/Line/Col/Delta aren't flushed to Parser.
            do_return(Parser0);
        {Major, Minor} when Minor > ?MAX_YAML_MINOR_VERSION_SUPPORTED ->
            %% The document's minor version is greater than the
            %% supported version. Add a warning and continue anyway.
            Error = #yamerl_parsing_error{
              name   = version_not_supported,
              type   = warning,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (maximum version ~b.~b); "
              "parsing may fail",
              [
                Major, Minor,
                ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
                ?MAX_YAML_MINOR_VERSION_SUPPORTED
              ]),
            Parser0
    end,
    Parser2 = Parser1#yamerl_parser{
      doc_started = true,
      doc_version = Version1
    },
    %% Emit a token with the determined version and the tags table.
    queue_token(Parser2, Token, Insert_At).

end_doc(#yamerl_parser{doc_started = false} = Parser, _, _, _) ->
    %% No document to end.
    Parser;
end_doc(Parser, Line, Col, Insert_At) ->
    %% At the end of the document, we reset the version and the tags
    %% table.
    Parser1 = Parser#yamerl_parser{
      doc_started = false,
      doc_version = undefined
    },
    Parser2 = setup_default_tags(Parser1),
    Token = #yamerl_doc_end{
      line   = Line,
      column = Col
    },
    %% We check if there is an unfinished flow collection.
    Parser3 = case ?IN_FLOW_CTX(Parser) of
        false ->
            Parser2;
        true ->
            Error = #yamerl_parsing_error{
              name   = unfinished_flow_collection,
              token  = Token,
              line   = Line,
              column = Col
            },
            add_error(Parser2, Error, "Unfinished flow collection", [])
    end,
    queue_token(Parser3, Token, Insert_At).

%% -------------------------------------------------------------------
%% Directives.
%% -------------------------------------------------------------------

parse_directive([_ | Rest] = Chars, Line, Col, Delta, Parser) ->
    Ctx = #directive_ctx{
      line = Line,
      col  = Col
    },
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Chars, Line, Col, Delta,
      Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = ?ALLOW_IMPL_KEY(Parser2, false),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_directive(Rest, Line, Col1, Delta1, Parser3, Ctx).

do_parse_directive([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_directive2(Chars, Line, Col, Delta, Parser, Ctx);

do_parse_directive([C | Rest] = Chars, Line, Col, Delta, Parser, Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#directive_ctx{
      name = [C | Ctx#directive_ctx.name]
    },
    do_parse_directive(Rest, Line, Col1, Delta1, Parser1, Ctx1);

do_parse_directive([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    parse_directive2(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_directive([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_directive/6, Ctx).

parse_directive2(Chars, Line, Col, Delta, Parser, Ctx) ->
    Name = lists:reverse(Ctx#directive_ctx.name),
    Ctx1 = Ctx#directive_ctx{
      name = Name
    },
    case Name of
        "YAML" ->
            parse_yaml_directive(Chars, Line, Col, Delta, Parser, Ctx1);
        "TAG" ->
            parse_tag_directive(Chars, Line, Col, Delta, Parser, Ctx1);
        _ ->
            parse_reserved_directive(Chars, Line, Col, Delta, Parser, Ctx1)
    end.

skip_directive_trailing_ws([C | Rest], Line, Col, Delta, Parser)
  when ?IS_SPACE(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    skip_directive_trailing_ws(Rest, Line, Col1, Delta1, Parser);
skip_directive_trailing_ws([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_NEWLINE(C) orelse C == $# orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    find_next_token(Chars, Line, Col, Delta, Parser);
skip_directive_trailing_ws([] = Chars, Line, Col, Delta, Parser) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun skip_directive_trailing_ws/5);

skip_directive_trailing_ws([_ | _] = Chars, Line, Col, Delta, Parser) ->
    Error = #yamerl_parsing_error{
      name   = unexpected_directive_extra_params,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected directive extra parameters", []),
    return(Chars, Line, Col, Delta, Parser1).

%%
%% YAML directive.
%%

parse_yaml_directive(Chars, Line, Col, Delta, Parser,
  #directive_ctx{line = Dir_Line, col = Dir_Col}) ->
    Ctx = #yaml_directive_ctx{
      line = Dir_Line,
      col  = Dir_Col
    },
    parse_yaml_directive_major(Chars, Line, Col, Delta, Parser, Ctx).

%% Major version number.
parse_yaml_directive_major([C | Rest], Line, Col, Delta, Parser,
  #yaml_directive_ctx{major = Major} = Ctx) when C >= $0 andalso C =< $9 ->
    Major1 = case Major of
        undefined -> C - $0;
        _         -> Major * 10 + C - $0
    end,
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#yaml_directive_ctx{
      major = Major1
    },
    parse_yaml_directive_major(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_yaml_directive_major([$. | Rest], Line, Col, Delta, Parser,
  #yaml_directive_ctx{major = Major} = Ctx) when is_integer(Major) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    %% End of the major part. Continue with the minor version number.
    parse_yaml_directive_minor(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_yaml_directive_major([C | Rest], Line, Col, Delta, Parser,
  #yaml_directive_ctx{major = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_yaml_directive_major(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_yaml_directive_major([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_yaml_directive_major/6, Ctx);

parse_yaml_directive_major([_ | _] = Chars, Line, Col, Delta, Parser,
  #yaml_directive_ctx{line = Dir_Line, col = Dir_Col}) ->
    %% Invalid character (or end of directive) while parsing major
    %% version number.
    Token = #yamerl_yaml_directive{
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid major version number in YAML directive", []),
    return(Chars, Line, Col, Delta, Parser1);
parse_yaml_directive_major([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #yaml_directive_ctx{line = Dir_Line, col = Dir_Col}) ->
    %% Invalid end-of-stream while parsing major version number.
    Token = #yamerl_yaml_directive{
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing YAML directive", []),
    return(Chars, Line, Col, Delta, Parser1).

%% Minor version number.
parse_yaml_directive_minor([C | Rest], Line, Col, Delta, Parser,
  #yaml_directive_ctx{minor = Minor} = Ctx) when C >= $0 andalso C =< $9 ->
    Minor1 = case Minor of
        undefined -> C - $0;
        _         -> Minor * 10 + C - $0
    end,
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#yaml_directive_ctx{
      minor = Minor1
    },
    parse_yaml_directive_minor(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_yaml_directive_minor([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #yaml_directive_ctx{minor = Minor} = Ctx)
  when is_integer(Minor) andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    %% Directive end.
    queue_yaml_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_yaml_directive_minor([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    %% Directive end.
    queue_yaml_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_yaml_directive_minor([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_yaml_directive_minor/6, Ctx);

parse_yaml_directive_minor([_ | _] = Chars, Line, Col, Delta, Parser,
  #yaml_directive_ctx{line = Dir_Line, col = Dir_Col}) ->
    %% Invalid character while parsing minor version number.
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    Token = #yamerl_yaml_directive{
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid minor version number in YAML directive", []),
    return(Chars, Line, Col, Delta, Parser2).

%% Queue token.
queue_yaml_directive(Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = undefined} = Parser,
  #yaml_directive_ctx{major = Major, minor = Minor,
  line = Dir_Line, col = Dir_Col}) ->
    Version = {Major, Minor},
    Token = #yamerl_yaml_directive{
      version = Version,
      line    = Dir_Line,
      column  = Dir_Col
    },
    Parser1 = queue_token(Parser, Token),
    Parser2 = Parser1#yamerl_parser{
      doc_version = Version
    },
    skip_directive_trailing_ws(Chars, Line, Col, Delta, Parser2);
queue_yaml_directive(Chars, Line, Col, Delta, Parser,
  #yaml_directive_ctx{major = Major, minor = Minor,
  line = Dir_Line, col = Dir_Col} = Ctx) ->
    %% Warning: repeated YAML directive.
    Version = {Major, Minor},
    Token   = #yamerl_yaml_directive{
      version = Version,
      line    = Dir_Line,
      column  = Dir_Col
    },
    Error = #yamerl_parsing_error{
      type   = warning,
      name   = multiple_yaml_directives,
      token  = Token,
      line   = Dir_Line,
      column = Dir_Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple YAML directives found: the last one will be used", []),
    Parser2 = Parser1#yamerl_parser{
      doc_version = undefined
    },
    queue_yaml_directive(Chars, Line, Col, Delta, Parser2, Ctx).

%%
%% TAG directive.
%%

parse_tag_directive(Chars, Line, Col, Delta, Parser,
  #directive_ctx{line = Dir_Line, col = Dir_Col}) ->
    Ctx = #tag_directive_ctx{
      line = Dir_Line,
      col  = Dir_Col
    },
    parse_tag_directive_handle(Chars, Line, Col, Delta, Parser, Ctx).

%% Tag handle.
parse_tag_directive_handle([$! | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = undefined} = Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_directive_ctx{
      handle = "!"
    },
    parse_tag_directive_handle(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_directive_handle([C | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = Handle} = Ctx)
  when is_list(Handle) andalso
  ((C >= $a andalso C =< $z) orelse
   (C >= $A andalso C =< $Z) orelse
   (C >= $0 andalso C =< $9) orelse
   (C == $-)) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_directive_ctx{
      handle = [C | Handle]
    },
    parse_tag_directive_handle(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_directive_handle([$! | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = Handle} = Ctx) when is_list(Handle) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_directive_ctx{
      handle = [$! | Handle]
    },
    parse_tag_directive_prefix(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_directive_handle([C | _] = Chars, Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = "!"} = Ctx) when ?IS_SPACE(C) ->
    parse_tag_directive_prefix(Chars, Line, Col, Delta, Parser, Ctx);
parse_tag_directive_handle([C | Rest], Line, Col, Delta,  Parser,
  #tag_directive_ctx{handle = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_tag_directive_handle(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_tag_directive_handle([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_tag_directive_handle/6, Ctx);

parse_tag_directive_handle([_ | _] = Chars, Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = Handle, line = Dir_Line, col = Dir_Col}) ->
    %% Invalid character (or end of directive) while parsing tag handle.
    Handle1 = case Handle of
        undefined -> Handle;
        _         -> lists:reverse(Handle)
    end,
    Token = #yamerl_tag_directive{
      handle = Handle1,
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid tag handle in TAG directive", []),
    return(Chars, Line, Col, Delta, Parser1);
parse_tag_directive_handle([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #tag_directive_ctx{handle = Handle, line = Dir_Line, col = Dir_Col}) ->
    %% Invalid end-of-stream while parsing major version number.
    Handle1 = case Handle of
        undefined -> Handle;
        _         -> lists:reverse(Handle)
    end,
    Token = #yamerl_tag_directive{
      handle = Handle1,
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing TAG directive", []),
    return(Chars, Line, Col, Delta, Parser1).

%% Tag prefix.
parse_tag_directive_prefix([C | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{prefix = Prefix} = Ctx)
  when is_list(Prefix) andalso ?IS_URI_CHAR(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_directive_ctx{
      prefix = [C | Prefix]
    },
    parse_tag_directive_prefix(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_directive_prefix([C | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{prefix = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_directive_ctx{
      prefix = ""
    },
    parse_tag_directive_prefix(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_directive_prefix([C | Rest], Line, Col, Delta, Parser,
  #tag_directive_ctx{prefix = ""} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_tag_directive_prefix(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_tag_directive_prefix([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #tag_directive_ctx{prefix = Prefix} = Ctx)
  when is_list(Prefix) andalso Prefix /= "" andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    queue_tag_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_tag_directive_prefix([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    queue_tag_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_tag_directive_prefix([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_tag_directive_prefix/6, Ctx);

parse_tag_directive_prefix([_ | _] = Chars, Line, Col, Delta, Parser,
  #tag_directive_ctx{handle = Handle, prefix = Prefix,
  line = Dir_Line, col = Dir_Col}) ->
    %% Invalid character while parsing tag prefix.
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    Token = #yamerl_tag_directive{
      handle = lists:reverse(Handle),
      prefix = lists:reverse(Prefix),
      line   = Dir_Line,
      column = Dir_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid tag prefix in TAG directive", []),
    return(Chars, Line, Col, Delta, Parser2).

%% Queue token.
queue_tag_directive(Chars, Line, Col, Delta,
  #yamerl_parser{tags = Tags} = Parser,
  #tag_directive_ctx{handle = Handle, prefix = Prefix,
  line = Dir_Line, col = Dir_Col}) ->
    Handle1 = lists:reverse(Handle),
    Prefix1 = lists:reverse(Prefix),
    Token   = #yamerl_tag_directive{
      handle = Handle1,
      prefix = Prefix1,
      line   = Dir_Line,
      column = Dir_Col
    },
    Parser1 = is_uri_valid(Parser, Token),
    Parser2 = case dict:is_key(Handle1, Tags) of
        false ->
            Parser1;
        true ->
            Error = #yamerl_parsing_error{
              type   = warning,
              name   = multiple_tag_handle_declarations,
              token  = Token,
              line   = Dir_Line,
              column = Dir_Col
            },
            add_error(Parser1, Error,
              "Multiple declarations of the same handle found: "
              "the last one will be used", [])
    end,
    Parser3 = queue_token(Parser2, Token),
    Tags1   = dict:store(Handle1, Prefix1, Tags),
    Parser4 = Parser3#yamerl_parser{
      tags = Tags1
    },
    skip_directive_trailing_ws(Chars, Line, Col, Delta, Parser4).

%%
%% Reserved directive.
%%

parse_reserved_directive(Chars, Line, Col, Delta, Parser,
  #directive_ctx{name = Name, line = Dir_Line, col = Dir_Col}) ->
    Ctx = #reserved_directive_ctx{
      name = Name,
      line = Dir_Line,
      col  = Dir_Col
    },
    parse_reserved_directive_arg(Chars, Line, Col, Delta, Parser, Ctx).

parse_reserved_directive_arg([C | Rest], Line, Col, Delta, Parser,
  #reserved_directive_ctx{current = undefined} = Ctx)
  when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_reserved_directive_arg(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_reserved_directive_arg([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #reserved_directive_ctx{current = undefined} = Ctx)
  when ?IS_NEWLINE(C) orelse C == $# orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% End of directive.
    queue_reserved_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_reserved_directive_arg([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #reserved_directive_ctx{current = Current,
  args = Args, args_count = Count} = Ctx)
  when is_list(Current) andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    %% Current argument finished.
    Current1 = lists:reverse(Current),
    Ctx1     = Ctx#reserved_directive_ctx{
      current    = undefined,
      args       = [Current1 | Args],
      args_count = Count + 1
    },
    parse_reserved_directive_arg(Chars, Line, Col, Delta, Parser, Ctx1);
parse_reserved_directive_arg([C | Rest] = Chars, Line, Col, Delta, Parser,
  #reserved_directive_ctx{current = Current} = Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Current1 = case Current of
        undefined -> [C];
        _         -> [C | Current]
    end,
    Ctx1 = Ctx#reserved_directive_ctx{
      current = Current1
    },
    parse_reserved_directive_arg(Rest, Line, Col1, Delta1, Parser1, Ctx1);
parse_reserved_directive_arg([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
  %% End of directive.
    queue_reserved_directive(Chars, Line, Col, Delta, Parser, Ctx);
parse_reserved_directive_arg([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_reserved_directive_arg/6, Ctx).

queue_reserved_directive(Chars, Line, Col, Delta, Parser,
  #reserved_directive_ctx{name = Name, current = Current,
  args = Args, args_count = Count, line = Dir_Line, col = Dir_Col}) ->
    {Args1, Count1} = case Current of
        undefined -> {Args, Count};
        _         -> {[lists:reverse(Current) | Args], Count + 1}
    end,
    Token = #yamerl_reserved_directive{
      name       = Name,
      args       = lists:reverse(Args1),
      args_count = Count1,
      line       = Dir_Line,
      column     = Dir_Col
    },
    Error = #yamerl_parsing_error{
      type   = warning,
      name   = reserved_directive,
      token  = Token,
      line   = Dir_Line,
      column = Dir_Col
    },
    Parser1 = add_error(Parser, Error,
      "Reserved directive \"~s\" ignored", [Name]),
    Parser2 = queue_token(Parser1, Token),
    skip_directive_trailing_ws(Chars, Line, Col, Delta, Parser2).

%% -------------------------------------------------------------------
%% Block sequences.
%% -------------------------------------------------------------------

%% We found a new block sequence entry.
parse_block_entry(Chars, Line, Col, Delta,
  #yamerl_parser{ik_allowed = true} = Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    queue_block_sequence_entry_token(Chars, Line, Col, Delta, Parser);
parse_block_entry(Chars, Line, Col, Delta, Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yamerl_parsing_error{
      name   = block_sequence_entry_not_allowed,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Block sequence entry not allowed here", []),
    return(Chars, Line, Col, Delta, Parser1);
parse_block_entry(Chars, Line, Col, Delta, Parser) when ?IN_FLOW_CTX(Parser) ->
    Error = #yamerl_parsing_error{
      name   = block_collection_in_flow_context,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Block collection not allowed inside flow collection", []),
    return(Chars, Line, Col, Delta, Parser1).

queue_block_sequence_entry_token([_ | Rest], Line, Col, Delta, Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, true),
    Token   = #yamerl_sequence_entry{
      line   = Line,
      column = Col
    },
    Parser3 = queue_token(Parser2, Token),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser3).

%% -------------------------------------------------------------------
%% Flow collections.
%% -------------------------------------------------------------------

parse_flow_collection_start([_ | Rest] = Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = Cur_Coll, parent_colls = Colls} = Parser,
  Kind) ->
    Parser1 = save_impl_key_pos(Chars, Line, Col, Delta, Parser),
    Parser2 = ?PUSH_FAKE_IMPL_KEY(Parser1),
    Parser3 = ?ALLOW_IMPL_KEY(Parser2, true),
    Token = #yamerl_collection_start{
      style  = flow,
      kind   = Kind,
      line   = Line,
      column = Col,
      tag    = ?COLL_SCALAR_DEFAULT_TAG(Line, Col)
    },
    Parser4  = queue_token(Parser3, Token),
    New_Coll = #fcoll{kind = Kind},
    Parser5  = case Kind of
        sequence ->
            Parser4#yamerl_parser{
              cur_coll      = New_Coll,
              parent_colls  = [Cur_Coll | Colls],
              pending_entry = true
            };
        mapping ->
            Parser4#yamerl_parser{
              cur_coll           = New_Coll,
              parent_colls       = [Cur_Coll | Colls],
              waiting_for_kvpair = true
            }
    end,
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser5).

parse_flow_collection_end(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = single_mapping}} = Parser,
  Kind) ->
    Parser1 = finish_incomplete_flow_entries(Line, Col, Delta, Parser),
    parse_flow_collection_end(Chars, Line, Col, Delta, Parser1, Kind);
parse_flow_collection_end([_ | Rest], Line, Col, Delta,
  #yamerl_parser{
    cur_coll = #fcoll{kind = Kind}, parent_colls = [Coll | Colls]} = Parser,
  Kind) ->
    Parser1 = finish_incomplete_flow_entries(Line, Col, Delta, Parser),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = ?POP_IMPL_KEY(Parser2),
    Parser4 = Parser3#yamerl_parser{
      cur_coll           = Coll,
      parent_colls       = Colls,
      pending_entry      = false,
      waiting_for_kvpair = false
    },
    Parser5 = ?ALLOW_IMPL_KEY(Parser4, false),
    Token    = #yamerl_collection_end{
      style  = flow,
      kind   = Kind,
      line   = Line,
      column = Col
    },
    Parser6 = queue_token(Parser5, Token),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser6);
parse_flow_collection_end(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = Expected}} = Parser, Kind) ->
    %% Closing a different-type collection.
    Error = #yamerl_parsing_error{
      name   = closing_non_matching_flow_collection_type,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "A ~s closing character is used to close a ~s collection",
      [Kind, Expected]),
    parse_flow_collection_end(Chars, Line, Col, Delta, Parser1, Expected);
parse_flow_collection_end([_ | Rest], Line, Col, Delta,
  #yamerl_parser{cur_coll = #bcoll{}} = Parser, Kind) ->
    %% Closing a never-opened collection.
    Error = #yamerl_parsing_error{
      name   = closing_never_opened_flow_collection,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "The ~s closing character doesn't match any opening character",
      [Kind]),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser1).

parse_flow_entry(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = single_mapping}} = Parser) ->
    Parser1 = finish_incomplete_flow_entries(Line, Col, Delta, Parser),
    parse_flow_entry(Chars, Line, Col, Delta, Parser1);
parse_flow_entry([_ | Rest], Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = Kind}} = Parser) when
  (Kind == sequence andalso ?MISSING_ENTRY(Parser)) orelse
  (Kind == mapping  andalso ?MISSING_KVPAIR(Parser)) ->
    %% In a flow collection, the "," entry indicator immediately follows a
    %% collection-start or a previous entry indicator.
    Error = #yamerl_parsing_error{
      name   = flow_collection_entry_not_allowed,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Empty flow collection entry not allowed", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser1);
parse_flow_entry([_ | Rest], Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = Kind}} = Parser) ->
    Parser1 = finish_incomplete_flow_entries(Line, Col, Delta, Parser),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = ?ALLOW_IMPL_KEY(Parser2, true),
    Parser4 = case Kind of
        sequence ->
            Parser3#yamerl_parser{
              pending_entry = true
            };
        mapping ->
            Parser3#yamerl_parser{
              waiting_for_kvpair = true
            }
    end,
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser4);
parse_flow_entry(Chars, Line, Col, Delta, Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yamerl_parsing_error{
      name   = flow_collection_entry_not_allowed,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Flow collection entry not allowed outside any flow collection", []),
    return(Chars, Line, Col, Delta, Parser1).

%% -------------------------------------------------------------------
%% Block or flow mapping key.
%% -------------------------------------------------------------------

parse_mapping_key(Chars, Line, Col, Delta,
  #yamerl_parser{ik_allowed = true} = Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    %% A mapping key is allowed here.
    Parser1 = finish_incomplete_block_entries(Line, Col, Parser),
    queue_mapping_key_token(Chars, Line, Col, Delta, Parser1);
parse_mapping_key(Chars, Line, Col, Delta, Parser)
  when ?IN_FLOW_CTX(Parser) ->
    %% A mapping key is always allowed in flow context.
    queue_mapping_key_token(Chars, Line, Col, Delta, Parser);
parse_mapping_key(Chars, Line, Col, Delta, Parser) ->
    %% A mapping key is NOT allowed here.
    Error = #yamerl_parsing_error{
      name   = block_mapping_key_not_allowed,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping key not allowed here", []),
    return(Chars, Line, Col, Delta, Parser1).

queue_mapping_key_token([_ | Rest], Line, Col, Delta, Parser) ->
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, ?IN_BLOCK_CTX(Parser1)),
    Token   = #yamerl_mapping_key{
      line   = Line,
      column = Col
    },
    Parser3 = queue_token(Parser2, Token),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser3).

%% -------------------------------------------------------------------
%% Block or flow mapping value.
%% -------------------------------------------------------------------

%% We found a new block mapping value. We must check if an implicit key
%% is pending.
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_stack = [#impl_key{possible = true, line = Impl_Line} | _],
  last_token_endline = Endline} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso
  Impl_Line < Endline andalso Endline == Line ->
    %% The key of this mapping is an implicit key spanning over several lines.
    %% This will raise a warning.
    Parser1 = queue_impl_key(Delta, Parser),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser1);
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_stack =
    [#impl_key{possible = true, line = Impl_Line} = Impl_Key | Rest]} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso
  Impl_Line < Line ->
    %% This is not an implicit key.
    Impl_Key1 = Impl_Key#impl_key{possible = false},
    Parser1   = Parser#yamerl_parser{
      ik_stack = [Impl_Key1 | Rest]
    },
    parse_mapping_value(Chars, Line, Col, Delta, Parser1);
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_stack = [#impl_key{possible = true} | _]} = Parser) ->
    %% The key of this mapping is an implicit key.
    Parser1 = queue_impl_key(Delta, Parser),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser1);

parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_allowed = true,
    cur_coll = #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso KIdx > VIdx ->
    %% The key of this mapping is an explicit key, already queued.
    %% In block context, an implicit key may follow.
    Parser1 = ?ALLOW_IMPL_KEY(Parser, true),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser1);
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_allowed = true,
    cur_coll = #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso KIdx =< VIdx ->
    %% The key of this mapping is an empty node. We queue a mapping-key
    %% token followed; the empty scalar will be automatically queued.
    Token = #yamerl_mapping_key{
      line   = Line,
      column = Col
    },
    Parser1 = queue_token(Parser, Token),
    %% In block context, an implicit key may follow.
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, true),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser2);
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{ik_allowed = false} = Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yamerl_parsing_error{
      name   = block_mapping_value_not_allowed,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping value not allowed here", []),
    return(Chars, Line, Col, Delta, Parser1);

parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll =
    #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_FLOW_CTX(Parser) andalso
  (Kind == mapping orelse Kind == single_mapping) andalso KIdx > VIdx ->
    %% The key of this mapping is an explicit key, already queued.
    %% In flow context, an implicit key may not follow.
    Parser1 = ?ALLOW_IMPL_KEY(Parser, false),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser1);
parse_mapping_value(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll =
    #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_FLOW_CTX(Parser) andalso
  (((Kind == mapping orelse Kind == single_mapping) andalso KIdx =< VIdx) orelse
   Kind == sequence) ->
    %% The key of this mapping is an empty node.
    Token = #yamerl_mapping_key{
      line   = Line,
      column = Col
    },
    Parser1 = queue_token(Parser, Token),
    %% In flow context, an implicit key may not follow.
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, false),
    queue_mapping_value_token(Chars, Line, Col, Delta, Parser2).

queue_mapping_value_token(Chars, Line, Col, Delta,
  #yamerl_parser{last_token_endline = Endline,
    cur_coll = #bcoll{indent = Indent}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso Line > Endline andalso Col > Indent ->
    Token = #yamerl_mapping_value{
      line   = Line,
      column = Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_block_mapping_value_indentation,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping value's indentation (column #~b) "
      "greater than expected (column #~b)",
      [Col, Indent]),
    queue_mapping_value_token2(Chars, Line, Col, Delta, Parser1);
queue_mapping_value_token(Chars, Line, Col, Delta, Parser) ->
    queue_mapping_value_token2(Chars, Line, Col, Delta, Parser).

queue_mapping_value_token2([_ | Rest], Line, Col, Delta, Parser) ->
    Token = #yamerl_mapping_value{
      line   = Line,
      column = Col
    },
    Parser1 = queue_token(Parser, Token),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    find_next_token(Rest, Line, Col1, Delta1, Parser1).

finish_incomplete_block_entries(Line, Col,
  #yamerl_parser{cur_coll =
    #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when KIdx > VIdx ->
    %% The last block mapping key has an empty node as value (and the
    %% ":" value indicator was never used). Queue a value token. The
    %% empty scalar will be automatically queued.
    Token = #yamerl_mapping_value{
      line   = Line,
      column = Col
    },
    queue_token(Parser, Token);
finish_incomplete_block_entries(_, _, Parser) ->
    Parser.

finish_incomplete_flow_entries(Line, Col, _,
  #yamerl_parser{cur_coll = #fcoll{kind = single_mapping},
  parent_colls = [Coll | Colls]} = Parser) ->
    %% Close single key: value pair.
    Token = #yamerl_collection_end{
      style  = flow,
      kind   = mapping,
      line   = Line,
      column = Col
    },
    Parser1 = queue_token(Parser, Token),
    Parser1#yamerl_parser{
      cur_coll     = Coll,
      parent_colls = Colls
    };
finish_incomplete_flow_entries(Line, Col, Delta,
  #yamerl_parser{cur_coll = #fcoll{kind = mapping},
  ik_stack = [#impl_key{possible = true} | _]} = Parser) ->
    %% Queue implicit key token.
    Parser1 = queue_impl_key(Delta, Parser),
    finish_incomplete_flow_entries(Line, Col, Delta, Parser1);
finish_incomplete_flow_entries(Line, Col, Delta,
  #yamerl_parser{
    cur_coll = #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when (Kind == mapping orelse Kind == single_mapping) andalso KIdx > VIdx ->
    %% In a flow mapping, the last entry was a key without the ":" value
    %% indicator. Queue the value token and the implicit empty node. If
    %% the key was an empty node (ie. only a tag node property), it'll
    %% be added automatically by queue_token/2.
    Token = #yamerl_mapping_value{
      line   = Line,
      column = Col
    },
    Parser1 = queue_token(Parser, Token),
    finish_incomplete_flow_entries(Line, Col, Delta, Parser1);
finish_incomplete_flow_entries(_, _, _, Parser) ->
    Parser.

%% -------------------------------------------------------------------
%% Anchors and aliases.
%% -------------------------------------------------------------------

parse_anchor_or_alias([_ | Rest] = Chars, Line, Col, Delta, Parser, Type) ->
    Ctx = #anchor_ctx{
      type = Type,
      line = Line,
      col  = Col
    },
    Parser1 = save_impl_key_pos(Chars, Line, Col, Delta, Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, false),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_anchor_or_alias(Rest, Line, Col1, Delta1, Parser2, Ctx).

%% White spaces and flow indicators are forbidden inside anchor or alias
%% names.
do_parse_anchor_or_alias([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx) when
  ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse ?IS_FLOW_INDICATOR(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_anchor_or_alias_token(Chars, Line, Col, Delta, Parser, Ctx);

do_parse_anchor_or_alias([C | Rest] = Chars, Line, Col, Delta, Parser, Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#anchor_ctx{
      output = [C | Ctx#anchor_ctx.output]
    },
    do_parse_anchor_or_alias(Rest, Line, Col1, Delta1, Parser1, Ctx1);

do_parse_anchor_or_alias([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    queue_anchor_or_alias_token(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_anchor_or_alias([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_anchor_or_alias/6, Ctx).

queue_anchor_or_alias_token(Chars, Line, Col, Delta, Parser,
  #anchor_ctx{type = Type, line = Anc_Line, col = Anc_Col, output = Name}) ->
    Token = case Type of
        anchor ->
            #yamerl_anchor{
              name   = lists:reverse(Name),
              line   = Anc_Line,
              column = Anc_Col
            };
        alias ->
            #yamerl_alias{
              name   = lists:reverse(Name),
              line   = Anc_Line,
              column = Anc_Col
            }
    end,
    Parser1 = queue_token(Parser, Token),
    find_next_token(Chars, Line, Col, Delta, Parser1).

%% -------------------------------------------------------------------
%% Tags.
%% -------------------------------------------------------------------

parse_tag([_ | Rest] = Chars, Line, Col, Delta,
  #yamerl_parser{last_tag = undefined} = Parser) ->
    Ctx = #tag_ctx{
      line   = Line,
      col    = Col,
      prefix = "!",
      suffix = ""
    },
    Parser1 = save_impl_key_pos(Chars, Line, Col, Delta, Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, false),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    determine_tag_type(Rest, Line, Col1, Delta1, Parser2, Ctx);
parse_tag(Chars, Line, Col, Delta, #yamerl_parser{last_tag = Tag} = Parser)
  when Tag /= undefined ->
    Error = #yamerl_parsing_error{
      name   = multiple_tag_properties,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple tag properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yamerl_parser{
      last_tag = undefined
    },
    parse_tag(Chars, Line, Col, Delta, Parser2).

%% Determine token type: verbatim tag or tag shorthand.
determine_tag_type([$< | Rest], Line, Col, Delta, Parser, Ctx) ->
    %% Verbatim tag.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_ctx{
      prefix = undefined
    },
    parse_verbatim_tag(Rest, Line, Col1, Delta1, Parser, Ctx1);
determine_tag_type([_ | _] = Chars, Line, Col, Delta, Parser, Ctx) ->
    parse_tag_shorthand(Chars, Line, Col, Delta, Parser, Ctx);
determine_tag_type([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    expand_tag(Chars, Line, Col, Delta, Parser, Ctx);
determine_tag_type([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun determine_tag_type/6, Ctx).

%%
%% Verbatim tag
%%

parse_verbatim_tag([$> | Rest], Line, Col, Delta, Parser, Ctx) ->
    %% End of the verbatim tag.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    expand_tag(Rest, Line, Col1, Delta1, Parser, Ctx);
parse_verbatim_tag([$! | Rest], Line, Col, Delta, Parser,
  #tag_ctx{suffix = ""} = Ctx) ->
    %% Local tag.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_ctx{
      suffix = "!"
    },
    parse_verbatim_tag(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_verbatim_tag([$! | Rest], Line, Col, Delta, Parser,
  #tag_ctx{suffix = Suffix, line = Tag_Line, col = Tag_Col} = Ctx) ->
    %% "!" forbidden in verbatim tag.
    Token = #yamerl_tag{
      uri    = lists:reverse(Suffix),
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_verbatim_tag(Rest, Line, Col1, Delta1, Parser1, Ctx);
parse_verbatim_tag([C | Rest], Line, Col, Delta, Parser, Ctx)
  when ?IS_URI_CHAR(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_ctx{
      suffix = [C | Ctx#tag_ctx.suffix]
    },
    parse_verbatim_tag(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_verbatim_tag([_ | Rest], Line, Col, Delta, Parser,
  #tag_ctx{suffix = Suffix, line = Tag_Line, col = Tag_Col} = Ctx) ->
    %% Character not allowed in a URI.
    Token = #yamerl_tag{
      uri    = lists:reverse(Suffix),
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_verbatim_tag(Rest, Line, Col1, Delta1, Parser1, Ctx);
parse_verbatim_tag([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_verbatim_tag/6, Ctx);
parse_verbatim_tag([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #tag_ctx{suffix = Suffix, line = Tag_Line, col = Tag_Col}) ->
    %% Unexpected end-of-stream
    Token = #yamerl_tag{
      uri    = lists:reverse(Suffix),
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing tag handle", []),
    return(Chars, Line, Col, Delta, Parser1).

%%
%% Tag shorthand.
%%

%% Tag prefix.
parse_tag_shorthand([$! | Rest], Line, Col, Delta, Parser,
  #tag_ctx{prefix = "!", suffix = Suffix} = Ctx) ->
    %% Separator between the prefix and the suffix.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_ctx{
      prefix = "!" ++ lists:reverse(Suffix) ++ "!",
      suffix = ""
    },
    parse_tag_shorthand(Rest, Line, Col1, Delta1, Parser, Ctx1);
parse_tag_shorthand([$! | Rest], Line, Col, Delta, Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix,
  line = Tag_Line, col = Tag_Col} = Ctx) ->
    %% "!" forbidden in tag.
    Token = #yamerl_tag{
      uri    = Prefix ++ lists:reverse(Suffix),
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_tag_shorthand(Rest, Line, Col1, Delta1, Parser1, Ctx);

%% Tag suffix.
parse_tag_shorthand([C | _] = Chars, Line, Col, Delta, Parser, Ctx)
  when ?IS_FLOW_INDICATOR(C) ->
    %% The next character starts another token.
    expand_tag(Chars, Line, Col, Delta, Parser, Ctx);
parse_tag_shorthand([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    expand_tag(Chars, Line, Col, Delta, Parser, Ctx);

parse_tag_shorthand([C | Rest], Line, Col, Delta, Parser,
  #tag_ctx{suffix = Suffix} = Ctx) when ?IS_URI_CHAR(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#tag_ctx{
      suffix = [C | Suffix]
    },
    parse_tag_shorthand(Rest, Line, Col1, Delta1, Parser, Ctx1);

parse_tag_shorthand([_ | Rest] = Chars, Line, Col, Delta, Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix,
  line = Tag_Line, col = Tag_Col} = Ctx) ->
    %% Character not allowed in a URI.
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    Token = #yamerl_tag{
      uri    = Prefix ++ lists:reverse(Suffix),
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid character in tag handle", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_tag_shorthand(Rest, Line, Col1, Delta1, Parser2, Ctx);

parse_tag_shorthand([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    expand_tag(Chars, Line, Col, Delta, Parser, Ctx);
parse_tag_shorthand([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun parse_tag_shorthand/6, Ctx).

%% Verbatim tag.
expand_tag(Chars, Line, Col, Delta, Parser,
  #tag_ctx{prefix = undefined, suffix = Suffix} = Ctx) ->
    Ctx1 = Ctx#tag_ctx{
      suffix = lists:reverse(Suffix)
    },
    queue_tag_token(Chars, Line, Col, Delta, Parser, Ctx1);

%% Non-specific tag.
expand_tag(Chars, Line, Col, Delta, Parser,
  #tag_ctx{prefix = "!", suffix = ""} = Ctx) ->
    Ctx1 = Ctx#tag_ctx{
      prefix = undefined,
      suffix = {non_specific, "!"}
    },
    queue_tag_token(Chars, Line, Col, Delta, Parser, Ctx1);

%% Tag shorthand.
expand_tag(Chars, Line, Col, Delta, Parser,
  #tag_ctx{line = Tag_Line, col = Tag_Col,
  prefix = Prefix, suffix = ""} = Ctx) ->
    Token = #yamerl_tag{
      uri    = Prefix,
      line   = Tag_Line,
      column = Tag_Col
    },
    Error1 = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error1,
      "Tag suffix mandatory", []),
    expand_tag2(Chars, Line, Col, Delta, Parser1, Ctx);
expand_tag(Chars, Line, Col, Delta, Parser, Ctx) ->
    expand_tag2(Chars, Line, Col, Delta, Parser, Ctx).

expand_tag2(Chars, Line, Col, Delta, #yamerl_parser{tags = Tags} = Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix,
  line = Tag_Line, col = Tag_Col} = Ctx) ->
    Suffix1 = lists:reverse(Suffix),
    {Parser1, URI} = try
        case dict:is_key(Prefix, Tags) of
            true  -> {Parser, dict:fetch(Prefix, Tags) ++ Suffix1};
            false -> {Parser, dict:fetch({default, Prefix}, Tags) ++ Suffix1}
        end
    catch
        _:_ ->
            Bad_URI = Prefix ++ Suffix1,
            Token = #yamerl_tag{
              uri    = Bad_URI,
              line   = Tag_Line,
              column = Tag_Col
            },
            Error = #yamerl_parsing_error{
              name   = undeclared_tag_handle,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Tag handle \"~s\" never declared", [Prefix]),
            {Parser0, Bad_URI}
    end,
    Ctx1 = Ctx#tag_ctx{
      prefix = undefined,
      suffix = URI
    },
    queue_tag_token(Chars, Line, Col, Delta, Parser1, Ctx1).

queue_tag_token(Chars, Line, Col, Delta, Parser,
  #tag_ctx{suffix = "!", line = Tag_Line, col = Tag_Col} = Ctx) ->
    Token = #yamerl_tag{
      uri    = "!",
      line   = Tag_Line,
      column = Tag_Col
    },
    Error = #yamerl_parsing_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Local tag suffix mandatory", []),
    queue_tag_token2(Chars, Line, Col, Delta, Parser1, Ctx);
queue_tag_token(Chars, Line, Col, Delta, Parser, Ctx) ->
    queue_tag_token2(Chars, Line, Col, Delta, Parser, Ctx).

queue_tag_token2(Chars, Line, Col, Delta, Parser,
  #tag_ctx{suffix = URI, line = Tag_Line, col = Tag_Col}) ->
    Token = #yamerl_tag{
      uri    = URI,
      line   = Tag_Line,
      column = Tag_Col
    },
    Parser1 = is_uri_valid(Parser, Token),
    Parser2 = queue_token(Parser1, Token),
    find_next_token(Chars, Line, Col, Delta, Parser2).

%% -------------------------------------------------------------------
%% Block scalars.
%% -------------------------------------------------------------------

parse_block_scalar([_ | Rest], Line, Col, Delta, Parser,
  Style) ->
    Ctx = #block_scalar_hd_ctx{
      style = Style,
      line  = Line,
      col   = Col
    },
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, true),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser2, Ctx).

%%
%% Header parsing.
%%

%% Newline, header termination.
do_parse_block_scalar_header([$\r] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_block_scalar_header/6, Ctx);

do_parse_block_scalar_header([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    prepare_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx);

%% Comments.
do_parse_block_scalar_header([$# | Rest], Line, Col, Delta, Parser, Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      in_comment = true
    },
    Ctx2 = final_indent(Parser, Ctx1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx2);
do_parse_block_scalar_header([_ | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{in_comment = true} = Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx);

%% Chomping indicator.
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{chomp = undefined} = Ctx)
  when C == $- orelse C == $+ ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Chomp = case C of
        $- -> strip;
        $+ -> keep
    end,
    Ctx1 = Ctx#block_scalar_hd_ctx{
      chomp = Chomp
    },
    Ctx2 = final_indent(Parser, Ctx1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx2);
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{style = Style, line = Sc_Line, col = Sc_Col} = Ctx)
  when C == $- orelse C == $+ ->
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = multiple_chomping_indicators,
      type   = warning,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple chomping indicators specified: the last one will be used",
      []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Chomp = case C of
        $- -> strip;
        $+ -> keep
    end,
    Ctx1 = Ctx#block_scalar_hd_ctx{
      chomp = Chomp
    },
    Ctx2 = final_indent(Parser1, Ctx1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser1, Ctx2);

%% Explicit indentation indicator.
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{indent = undefined} = Ctx)
  when C >= $1 andalso C =< $9 ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      indent = {tmp, C - $0}
    },
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx1);
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{indent = {tmp, Indent}} = Ctx)
  when C >= $1 andalso C =< $9 ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      indent = {tmp, Indent * 10 + C - $0}
    },
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx1);
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{style = Style, line = Sc_Line, col = Sc_Col} = Ctx)
  when C >= $1 andalso C =< $9 ->
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = multiple_indent_indicators,
      type   = warning,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple indent indicators specified: the last one will be used",
      []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      indent = {tmp, C - $0}
    },
    Ctx2 = final_indent(Parser1, Ctx1),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser1, Ctx2);

%% Trailing spaces.
do_parse_block_scalar_header([C | Rest], Line, Col, Delta, Parser, Ctx)
  when ?IS_SPACE(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = final_indent(Parser, Ctx),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser, Ctx1);

%% Invalid characters.
do_parse_block_scalar_header([_ | Rest] = Chars, Line, Col, Delta, Parser,
  #block_scalar_hd_ctx{style = Style, line = Sc_Line, col = Sc_Col} = Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_block_scalar_header,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid character in block scalar header", []),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = final_indent(Parser2, Ctx),
    do_parse_block_scalar_header(Rest, Line, Col1, Delta1, Parser2, Ctx1);

do_parse_block_scalar_header([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    %% End-of-stream reached while parsing block scalar header. Assume
    %% an empty string.
    prepare_parse_block_scalar(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_block_scalar_header([] = Chars, Line, Col, Delta, Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_block_scalar_header/6, Ctx).

final_indent(
  #yamerl_parser{cur_coll = #bcoll{kind = root}},
  #block_scalar_hd_ctx{indent = {tmp, Add_Indent}} = Ctx) ->
    Ctx#block_scalar_hd_ctx{indent = 1 + Add_Indent};
final_indent(
  #yamerl_parser{cur_coll = #bcoll{indent = Indent}},
  #block_scalar_hd_ctx{indent = {tmp, Add_Indent}} = Ctx) ->
    Ctx#block_scalar_hd_ctx{indent = Indent + Add_Indent};
final_indent(_, Ctx) ->
    Ctx.

prepare_parse_block_scalar(Chars, Line, Col, Delta, Parser, Ctx) ->
    Ctx1  = final_indent(Parser, Ctx),
    Chomp = case Ctx1#block_scalar_hd_ctx.chomp of
        undefined -> clip;
        C         -> C
    end,
    Next_Ctx = #block_scalar_ctx{
      style   = Ctx1#block_scalar_hd_ctx.style,
      line    = Ctx1#block_scalar_hd_ctx.line,
      col     = Ctx1#block_scalar_hd_ctx.col,
      endline = Line,
      endcol  = Col,
      chomp   = Chomp,
      indent  = Ctx1#block_scalar_hd_ctx.indent,
      newline = Ctx1#block_scalar_hd_ctx.indent /= undefined
    },
    do_parse_block_scalar(Chars, Line, Col, Delta, Parser, Next_Ctx).

%%
%% Newlines.
%%

%% Can't be sure it's a newline. It may be followed by a LF.
do_parse_block_scalar([$\r] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_block_scalar/6, Ctx);

%% This is an empty line just after the header.
do_parse_block_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #block_scalar_ctx{newline = false, spaces = Spaces, output = ""} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%% Literal style: no line folding.
do_parse_block_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #block_scalar_ctx{spaces = Spaces, style = literal} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1 = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%% Folded style: a newline at the end of a normal-indented line.
do_parse_block_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #block_scalar_ctx{spaces = Spaces, newline = false,
  more_indent = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1 = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\s | Spaces]
    },
    do_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%% Folded style: an empty line after a normal-indented line.
do_parse_block_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #block_scalar_ctx{spaces = Spaces, newline = true,
  more_indent = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Spaces1 = case Spaces of
        [$\s | S] -> S;
        _         -> Spaces
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces1]
    },
    do_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%% Folded style: a newline in a more-indented paragraph.
do_parse_block_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #block_scalar_ctx{spaces = Spaces, more_indent = true} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%%
%% Indentation.
%%

%% First non-space character: set indentation.
do_parse_block_scalar([C | _] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest} = Ctx)
  when C /= $\s andalso Longest < Col ->
    Ctx1 = Ctx#block_scalar_ctx{
      indent  = Col,
      newline = true
    },
    do_parse_block_scalar(Chars, Line, Col, Delta, Parser, Ctx1);
do_parse_block_scalar([C | _] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest,
  style = Style, line = Sc_Line, col = Sc_Col} = Ctx)
  when C /= $\s andalso Longest >= Col ->
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      type   = warning,
      name   = leading_empty_lines_too_long,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "A leading all-space line has too many spaces (~b) "
      "compared to detected indentation (~b)", [Longest, Col - 1]),
    Ctx1 = Ctx#block_scalar_ctx{
      longest_empty = 0
    },
    do_parse_block_scalar(Chars, Line, Col, Delta, Parser1, Ctx1);
do_parse_block_scalar([$\s | Rest], Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest} = Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1    = if
        Col > Longest -> Ctx#block_scalar_ctx{longest_empty = Col};
        true          -> Ctx
    end,
    do_parse_block_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);

%% Skip indentation spaces.
do_parse_block_scalar([$\s | Rest], Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = Indent, newline = true} = Ctx)
  when Indent == undefined orelse Col < Indent ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_block_scalar(Rest, Line, Col1, Delta1, Parser, Ctx);

%% The next line is less indented than the block scalar: end it.
do_parse_block_scalar([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #bcoll{indent = Indent}} = Parser, Ctx)
  when C /= $\s andalso Col =< Indent ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);

%% The next line is less indented than the block scalar, but more than
%% the parent node. However, it's a comment, so we end the scalar.
do_parse_block_scalar([$# | _] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = Indent} = Ctx)
  when Col < Indent ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);

%% The next line has a directives end or document end marker: end the
%% scalar.
do_parse_block_scalar([C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = false} = Parser,
  Ctx) when (C == $- orelse C == $.) andalso (Len - Delta) < 4 ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_block_scalar/6, Ctx);
do_parse_block_scalar([$-, $-, $-, C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_block_scalar([$., $., $., C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser, Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_block_scalar([$-, $-, $-] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_block_scalar([$., $., $.] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);

%% The next line is less indented than the block scalar, but more than
%% the parent node: it's an error.
do_parse_block_scalar([C | _] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{indent = Indent, style = Style,
    line = Token_Line, col = Token_Col, output = Output})
  when C /= $\s andalso Col < Indent ->
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Token_Line,
      column   = Token_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Token_Line, Token_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_block_scalar_indentation,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid block scalar indentation", []),
    return(Chars, Line, Col, Delta, Parser1);

%%
%% Content.
%%

%% Literal style: everything after the indentation spaces is kept.
do_parse_block_scalar([C | Rest] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{style = literal, spaces = Spaces} = Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Output1 = case Spaces of
        "" -> [C | Ctx#block_scalar_ctx.output];
        _  -> [C | Spaces ++ Ctx#block_scalar_ctx.output]
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = Output1,
      endline = Line,
      endcol  = Col1
    },
    do_parse_block_scalar(Rest, Line, Col1, Delta1, Parser1, Ctx1);

%% Folded style: a normal-indented line.
do_parse_block_scalar([C | _] = Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{style = folded, more_indent = true, indent = Indent} = Ctx)
  when not ?IS_SPACE(C) andalso Col == Indent ->
    %% This line uses the default indentation: end the more indented
    %% paragraph.
    Ctx1 = Ctx#block_scalar_ctx{
      more_indent = false
    },
    do_parse_block_scalar(Chars, Line, Col, Delta, Parser, Ctx1);
do_parse_block_scalar([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_ctx{style = folded, newline = Newline, spaces = Spaces,
    output = Output} = Ctx)
  when not ?IS_SPACE(C) orelse
  (?IS_SPACE(C) andalso (not Newline orelse Output == "")) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Output1 = case Spaces of
        "" -> [C | Ctx#block_scalar_ctx.output];
        _  -> [C | Spaces ++ Ctx#block_scalar_ctx.output]
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = Output1,
      endline = Line,
      endcol  = Col1
    },
    do_parse_block_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);

%% Folded style: a more-indented line.
do_parse_block_scalar([C | Rest], Line, Col, Delta, Parser,
  #block_scalar_ctx{style = folded, newline = true, spaces = Spaces,
    more_indent = More_Indented} = Ctx)
  when ?IS_SPACE(C) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Spaces1 = case Spaces of
        [$\s | S]            -> [$\n | S];
        _ when More_Indented -> Spaces;
        _                    -> [$\n | Spaces]
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      spaces      = "",
      newline     = false,
      more_indent = true,
      output      = [C | Spaces1 ++ Ctx#block_scalar_ctx.output],
      endline     = Line,
      endcol      = Col1
    },
    do_parse_block_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);

do_parse_block_scalar([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser, Ctx) ->
    %% End-of-stream reached.
    queue_block_scalar_token(Chars, Line, Col, Delta, Parser, Ctx);
do_parse_block_scalar([] = Chars, Line, Col, Delta, Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_block_scalar/6, Ctx).

queue_block_scalar_token(Chars, Line, Col, Delta, Parser,
  #block_scalar_ctx{style = Style, output = Output, spaces = Spaces,
  chomp = Chomp, newline = Newline, line = Sc_Line, col = Sc_Col,
  endline = Endline, endcol = Endcol}) ->
    {Text, Endline1, Endcol1} = case Chomp of
        strip                   -> {Output, Endline, Endcol};
        clip when Output == ""  -> {Output, Endline, Endcol};
        clip when Spaces == ""  -> {Output, Endline, Endcol};
        clip                    -> {[$\n | Output], Endline + 1, 1};
        keep                    -> {Spaces ++ Output, Line, 1}
    end,
    Token = #yamerl_scalar{
      style    = block,
      substyle = Style,
      text     = lists:reverse(Text),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?BLOCK_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Parser1 = queue_token(Parser, Token),
    Parser2 = Parser1#yamerl_parser{
      endpos_set_by_token = true,
      last_token_endline  = Endline1,
      last_token_endcol   = Endcol1,
      missed_nl           = Newline
    },
    find_next_token(Chars, Line, Col, Delta, Parser2).

%% -------------------------------------------------------------------
%% Flow scalars.
%% -------------------------------------------------------------------

parse_flow_scalar([C | Rest] = Chars, Line, Col, Delta, Parser, Style) ->
    %% We start a flow scalar parsing: initialize the context.
    Ctx = #flow_scalar_ctx{
      style = Style,
      line  = Line,
      col   = Col
    },
    Parser1 = save_impl_key_pos(Chars, Line, Col, Delta, Parser),
    Parser2 = ?ALLOW_IMPL_KEY(Parser1, false),
    {Rest1, {Col1, Delta1}} = case C of
        $' -> {Rest, ?NEXT_COL(Col, Delta, 1)};
        $" -> {Rest, ?NEXT_COL(Col, Delta, 1)};
        _  -> {Chars, {Col, Delta}}
    end,
    do_parse_flow_scalar(Rest1, Line, Col1, Delta1, Parser2, Ctx).

do_parse_flow_scalar([$\r] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar/6, Ctx);

%%
%% Leading white spaces (plain scalar).
%%

do_parse_flow_scalar([C | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = plain, output = "", surrogate = undefined} = Ctx)
  when ?IS_SPACE(C) ->
    %% Skip leading white spaces in a plain scalar. We must update the
    %% position of beginning of the scalar and thus the implicit key.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Parser1 = save_impl_key_pos(Rest, Line, Col1, Delta1, Parser),
    Ctx1    = Ctx#flow_scalar_ctx{
      line = Line,
      col  = Col1
    },
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser1, Ctx1);

%%
%% Escaped characters [62].
%% Only supported by double-quoted strings.
%%

%% The next character is escaped.
do_parse_flow_scalar([$\\ | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = double_quoted,
  spaces = Spaces} = Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Output1 = case Spaces of
        "" -> Ctx#flow_scalar_ctx.output;
        _  -> Spaces ++ Ctx#flow_scalar_ctx.output
    end,
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces       = "",
      newline      = false,
      output       = Output1
    },
    do_parse_flow_scalar_escaped(Rest, Line, Col1, Delta1, Parser, Ctx1);

%% Invalid surrogate pair.
do_parse_flow_scalar([_ | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{surrogate = High, style = Style,
    line = Sc_Line, col = Sc_Col, output = Output} = Ctx)
  when High /= undefined ->
    %% The next character isn't the expected low surrogate.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_surrogate_pair,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Invalid UTF-16 surrogate pair", []),
    Ctx1 = Ctx#flow_scalar_ctx{
      surrogate = undefined
    },
    do_parse_flow_scalar(Chars, Line, Col, Delta, Parser1, Ctx1);

%% In a single-quoted string, a single quote is escaped by doubling
%% it.
do_parse_flow_scalar([$'] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser,
  #flow_scalar_ctx{style = single_quoted} = Ctx) ->
    %% Can't be sure it's an escaped single quote.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar/6, Ctx);
do_parse_flow_scalar([$', $' | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = single_quoted, spaces = Spaces} = Ctx) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 2),
    Output1 = case Spaces of
        "" -> Ctx#flow_scalar_ctx.output;
        _  -> Spaces ++ Ctx#flow_scalar_ctx.output
    end,
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = [$' | Output1]
    },
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);

%%
%% Line folding.
%% Leading and trailing white spaces are dropped, except when the
%% newline character is escaped, a previous white spaces is escaped or
%% at the beginning of the first line.
%%

do_parse_flow_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{newline = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1    = Ctx#flow_scalar_ctx{
      spaces  = " ",
      newline = true
    },
    do_parse_flow_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

do_parse_flow_scalar([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{newline = true, spaces = Spaces} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Spaces1 = case Spaces of
        [$\s | S] -> S;
        _         -> Spaces
    end,
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces = [$\n | Spaces1]
    },
    do_parse_flow_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

do_parse_flow_scalar([C | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{spaces = Spaces, newline = false} = Ctx)
  when ?IS_SPACE(C) ->
    %% Keep white spaces in a separate buffer. If we find content later,
    %% this buffer will be merged with the result buffer. Otherwise, the
    %% white spaces buffer may be trimmed or dropped.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces = [C | Spaces]
    },
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);

do_parse_flow_scalar([C | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{newline = true} = Ctx)
  when ?IS_SPACE(C) ->
    %% Drop leading white spaces when not on the first line.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx);

%%
%% Flow scalar end character.
%%

do_parse_flow_scalar([C | Rest], Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = Style} = Ctx) when
  (Style == double_quoted andalso C == $") orelse
  (Style == single_quoted andalso C == $') ->
    %% Found the end of this flow scalar. Next step: find the next
    %% token.
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Ctx1 = Ctx#flow_scalar_ctx{
      endline = Line,
      endcol  = Col1
    },
    queue_flow_scalar_token(Rest, Line, Col1, Delta1, Parser, Ctx1);

do_parse_flow_scalar([$# | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = plain, spaces = Spaces} = Ctx) when Spaces /= [] ->
    %% A '#' character preceded by white spaces is a comment. The plain
    %% scalar terminates with the first white spaces because trailing
    %% white spaces are ignored. [130]
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar([$:] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% We consider the end-of-stream as a "white space" and use the ':'
    %% character as the end character for this plain scalar. [130]
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar([$:] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar/6, Ctx);
do_parse_flow_scalar([$:, C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A ':' character followed by white spaces is not allowed in a
    %% plain scalar: end it. Only one character is available but it's
    %% enough to take a decision. The next state will handle the newline
    %% properly. [130]
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar([$:, C | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IN_FLOW_CTX(Parser) andalso ?IS_FLOW_INDICATOR(C) ->
    %% A ':' character followed by an flow indicator character in flow
    %% context ends the plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar([C | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IN_FLOW_CTX(Parser) andalso ?IS_FLOW_INDICATOR(C) ->
    %% The characters '[', ']', '{', '}' and ',' are forbidden in plain
    %% scalar because they are used as flow collection separation
    %% characters. [129]
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar([C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = false} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when (C == $- orelse C == $.) andalso (Len - Delta) < 4 ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar/6, Ctx);
do_parse_flow_scalar([$-, $-, $-, C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A directives end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar([$., $., $., C | _] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A document end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar([$-, $-, $-] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% A directives end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar([$., $., $.] = Chars, Line, 1 = Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% A document end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar([_ | _] = Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #bcoll{indent = Indent}} = Parser,
  #flow_scalar_ctx{style = plain, newline = true} = Ctx)
  when ?IN_BLOCK_CTX(Parser) andalso Col =< Indent ->
    %% The continuation line is as or less indented than the current
    %% block collection. Therefore, it's not a continuation line and we
    %% end the flow scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% End of stream = end of plain scalar.
    queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
      Ctx#flow_scalar_ctx{spaces = ""});

%%
%% JSON acceptable characters range [2].
%%

do_parse_flow_scalar([C | Rest] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{spaces = Spaces} = Ctx)
  when C == 16#9 orelse (C >= 16#20 andalso C =< 16#10FFFF) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    Output1 = case Spaces of
        "" -> [C | Ctx#flow_scalar_ctx.output];
        _  -> [C | Spaces ++ Ctx#flow_scalar_ctx.output]
    end,
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = Output1,
      endline = Line,
      endcol  = Col1
    },
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser1, Ctx1);

do_parse_flow_scalar([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Sc_Line, col = Sc_Col,
  output = Output}) ->
    Tag = case Style of
        plain -> ?PLAIN_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col);
        _     -> ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    end,
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = Tag
    },
    Error  = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Chars, Line, Col, Delta, Parser1);
do_parse_flow_scalar([] = Chars, Line, Col, Delta, Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar/6, Ctx).

%%
%% Escaped characters [62].
%% Only supported by double-quoted strings.
%%

%% Escaped 16-bit Unicode character, \uFFFF [60].
do_parse_flow_scalar_escaped([$u | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Sc_Line, col = Sc_Col,
  output = Output})
  when (Len - Delta) < 5 ->
    %% Unexpected enf-of-stream.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Chars, Line, Col, Delta, Parser1);
do_parse_flow_scalar_escaped([$u | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len} = Parser, Ctx)
  when (Len - Delta) < 5 ->
    %% Can't be sure it's an escaped Unicode character.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar_escaped/6, Ctx);
do_parse_flow_scalar_escaped([$u, O1, O2, O3, O4 | Rest], Line, Col, Delta,
  Parser, #flow_scalar_ctx{surrogate = High} = Ctx) when
  ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) andalso
  ?IS_HEXADECIMAL(O3) andalso ?IS_HEXADECIMAL(O4) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 5),
    C    = hex_to_dec([O1, O2, O3, O4], 0),
    Ctx1 = case High of
        undefined ->
            if
                ?IS_HIGH_SURROGATE(C) ->
                    %% This is the high part of a UTF-16 surrogate pair.
                    Ctx#flow_scalar_ctx{
                      surrogate = C
                    };
                true ->
                    %% Normal character.
                    Ctx#flow_scalar_ctx{
                      output = [C | Ctx#flow_scalar_ctx.output]
                    }
            end;
        _ ->
            if
                ?IS_LOW_SURROGATE(C) ->
                    %% This is the low part of a UTF-16 surrogate pair.
                    C1 = 16#10000 + (High - 16#d800) * 16#400 + (C - 16#dc00),
                    Ctx#flow_scalar_ctx{
                      output    = [C1 | Ctx#flow_scalar_ctx.output],
                      surrogate = undefined
                    };
                true ->
                    %% Error: high surrogate without a low surrogate.
                    %% The error is generated by the next clause.
                    Ctx
            end
    end,
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);
do_parse_flow_scalar_escaped([$u | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{line = Sc_Line, col = Sc_Col, style = Style,
  output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token,
      line   = Line,
      column = Col - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    do_parse_flow_scalar(Chars, Line, Col, Delta, Parser1, Ctx);

%% Escaped 32-bit Unicode character, \UFFFFFFFF [61].
do_parse_flow_scalar_escaped([$U | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Sc_Line, col = Sc_Col,
  output = Output})
  when (Len - Delta) < 9 ->
    %% Unexpected enf-of-stream.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Chars, Line, Col, Delta, Parser1);
do_parse_flow_scalar_escaped([$U | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len} = Parser, Ctx)
  when (Len - Delta) < 9 ->
    %% Can't be sure it's an escaped Unicode character.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar_escaped/6, Ctx);
do_parse_flow_scalar_escaped([$U, O1, O2, O3, O4, O5, O6, O7, O8 | Rest],
  Line, Col, Delta, Parser, #flow_scalar_ctx{surrogate = High} = Ctx) when
  ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) andalso
  ?IS_HEXADECIMAL(O3) andalso ?IS_HEXADECIMAL(O4) andalso
  ?IS_HEXADECIMAL(O5) andalso ?IS_HEXADECIMAL(O6) andalso
  ?IS_HEXADECIMAL(O7) andalso ?IS_HEXADECIMAL(O8) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 9),
    C    = hex_to_dec([O1, O2, O3, O4, O5, O6, O7, O8], 0),
    Ctx1 = case High of
        undefined ->
            if
                ?IS_HIGH_SURROGATE(C) ->
                    %% This is the high part of a UTF-16 surrogate pair.
                    Ctx#flow_scalar_ctx{
                      surrogate = C
                    };
                true ->
                    %% Normal character.
                    Ctx#flow_scalar_ctx{
                      output = [C | Ctx#flow_scalar_ctx.output]
                    }
            end;
        _ ->
            if
                ?IS_LOW_SURROGATE(C) ->
                    %% This is the low part of a UTF-16 surrogate pair.
                    C1 = 16#10000 + (High - 16#d800) * 16#400 + (C - 16#dc00),
                    Ctx#flow_scalar_ctx{
                      output    = [C1 | Ctx#flow_scalar_ctx.output],
                      surrogate = undefined
                    };
                true ->
                    %% Error: high surrogate without a low surrogate.
                    %% The error is generated by the next clause.
                    Ctx
            end
    end,
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);
do_parse_flow_scalar_escaped([$U | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{line = Sc_Line, col = Sc_Col,
  style = Style, output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token,
      line   = Line,
      column = Col - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    do_parse_flow_scalar(Chars, Line, Col, Delta, Parser1, Ctx);

%% Escaped 8-bit Unicode character, \xFF [59].
do_parse_flow_scalar_escaped([$x | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Sc_Line, col = Sc_Col,
  output = Output})
  when (Len - Delta) < 3 ->
    %% Unexpected enf-of-stream.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Chars, Line, Col, Delta, Parser1);
do_parse_flow_scalar_escaped([$x | _] = Chars, Line, Col, Delta,
  #yamerl_parser{chars_len = Len} = Parser, Ctx)
  when (Len - Delta) < 3 ->
    %% Can't be sure it's an escaped Unicode character.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar_escaped/6, Ctx);
do_parse_flow_scalar_escaped([$x, O1, O2 | Rest], Line, Col, Delta, Parser, Ctx)
  when ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) ->
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 3),
    C    = hex_to_dec([O1, O2], 0),
    Ctx1 = Ctx#flow_scalar_ctx{
      output       = [C | Ctx#flow_scalar_ctx.output]
    },
    do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser, Ctx1);
do_parse_flow_scalar_escaped([$x | _] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{line = Sc_Line, col = Sc_Col, style = Style,
  output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token,
      line   = Line,
      column = Col - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    do_parse_flow_scalar(Chars, Line, Col, Delta, Parser1, Ctx);

%% Escaped newline.
%% All trailing whitespaces are kept as content before an escaped
%% newline: this is handled in the $\\ clause above.
do_parse_flow_scalar_escaped([$\r] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser, Ctx) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar_escaped/6, Ctx);
do_parse_flow_scalar_escaped([C | _] = Chars, Line, _, Delta,
  #yamerl_parser{doc_version = Version} = Parser,
  #flow_scalar_ctx{style = double_quoted} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    {Rest, Line1, Col1, Delta1} = ?NEXT_LINE(Chars, Line, Delta, Parser),
    Ctx1 = Ctx#flow_scalar_ctx{
      newline = true
    },
    do_parse_flow_scalar(Rest, Line1, Col1, Delta1, Parser, Ctx1);

%% Other escaped characters.
do_parse_flow_scalar_escaped([C | Rest] = Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{line = Sc_Line, col = Sc_Col, style = Style,
  output = Output} = Ctx) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    case unescape_char(C) of
        undefined ->
            %% Invalid escaped character.
            Token = #yamerl_scalar{
              style    = flow,
              substyle = Style,
              text     = lists:reverse(Output),
              line     = Sc_Line,
              column   = Sc_Col,
              tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
            },
            Error  = #yamerl_parsing_error{
              name   = invalid_escaped_character,
              type   = warning,
              token  = Token,
              line   = Line,
              column = Col - 1
            },
            Parser2 = add_error(Parser1, Error,
              "Invalid escaped character", []),
            do_parse_flow_scalar(Chars, Line, Col, Delta, Parser2, Ctx);
        C1 ->
            {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
            Ctx1 = Ctx#flow_scalar_ctx{
              output       = [C1 | Ctx#flow_scalar_ctx.output]
            },
            do_parse_flow_scalar(Rest, Line, Col1, Delta1, Parser1, Ctx1)
    end;

do_parse_flow_scalar_escaped([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Sc_Line, col = Sc_Col,
  output = Output}) ->
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    },
    Error  = #yamerl_parsing_error{
      name   = unexpected_eos,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Chars, Line, Col, Delta, Parser1);
do_parse_flow_scalar_escaped([] = Chars, Line, Col, Delta, Parser, Ctx) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser,
      fun do_parse_flow_scalar_escaped/6, Ctx).

queue_flow_scalar_token(Chars, Line, Col, Delta, Parser,
  #flow_scalar_ctx{style = Style, output = Output, spaces = Spaces,
  newline = Newline, line = Sc_Line, col = Sc_Col,
  endline = Endline, endcol = Endcol}) ->
    Output1 = case Spaces of
        "" -> Output;
        _  -> Spaces ++ Output
    end,
    Tag = case Style of
        plain -> ?PLAIN_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col);
        _     -> ?FLOW_SCALAR_DEFAULT_TAG(Sc_Line, Sc_Col)
    end,
    Token = #yamerl_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output1),
      line     = Sc_Line,
      column   = Sc_Col,
      tag      = Tag
    },
    Parser1 = queue_token(Parser, Token),
    Parser2 = Parser1#yamerl_parser{
      endpos_set_by_token = true,
      last_token_endline  = Endline,
      last_token_endcol   = Endcol,
      missed_nl           = (Style == plain andalso Newline)
    },
    find_next_token(Chars, Line, Col, Delta, Parser2).

unescape_char($0)  -> 16#0;    %% \0 = NUL                        [42]
unescape_char($a)  -> 16#7;    %% \a = BELL                       [43]
unescape_char($b)  -> $\b;     %% \b = BS                         [44]
unescape_char($t)  -> $\t;     %% \t = TAB                        [45]
unescape_char($n)  -> $\n;     %% \n = LF                         [46]
unescape_char($v)  -> $\v;     %% \v = VT                         [47]
unescape_char($f)  -> $\f;     %% \f = FF                         [48]
unescape_char($r)  -> $\r;     %% \r = CR                         [49]
unescape_char($e)  -> $\e;     %% \e = ESC                        [50]
unescape_char($N)  -> 16#85;   %% \N = Unicode next line          [55]
unescape_char($_)  -> 16#A0;   %% \_ = Unicode non-breaking space [56]
unescape_char($L)  -> 16#2028; %% \L = Unicode line sep.          [57]
unescape_char($P)  -> 16#2029; %% \P = Unicode paragraph sep.     [58]
unescape_char($\s) -> $\s;     %% \  = SPC                        [51]
unescape_char($")  -> $";      %% \" = "                          [52]
unescape_char($/)  -> $/;      %% \/ = /                          [53]
unescape_char($\\) -> $\\;     %% \\ = \                          [54]
unescape_char(_)   -> undefined.

hex_to_dec([C | Rest], Number) ->
    C_Dec = if
        C >= $0, C =< $9 -> C - $0;
        C >= $a, C =< $f -> C - $a + 10;
        C >= $A, C =< $F -> C - $A + 10
    end,
    hex_to_dec(Rest, Number * 16 + C_Dec);
hex_to_dec([], Number) ->
    Number.

%% -------------------------------------------------------------------
%% Comments.
%% -------------------------------------------------------------------

parse_comment([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{doc_version = Version} = Parser)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A comment ends at the end of the line.
    %% This clause also takes care of DOS newline (even if the buffer
    %% contains only \r and not \n yet). It doesn't matter because we
    %% let the next state handle the newline properly; the cursor is not
    %% moved forward.
    find_next_token(Chars, Line, Col, Delta, Parser);
parse_comment([_ | Rest] = Chars, Line, Col, Delta, Parser) ->
    Parser1 = ?WARN_IF_NON_ASCII_LINE_BREAK(Chars, Line, Col, Parser),
    {Col1, Delta1} = ?NEXT_COL(Col, Delta, 1),
    parse_comment(Rest, Line, Col1, Delta1, Parser1);
parse_comment([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = false} = Parser) ->
    suspend_parsing(Chars, Line, Col, Delta, Parser, fun parse_comment/5);
parse_comment([] = Chars, Line, Col, Delta,
  #yamerl_parser{raw_eos = true} = Parser) ->
    find_next_token(Chars, Line, Col, Delta, Parser).

%% -------------------------------------------------------------------
%% Implicit key handling.
%% -------------------------------------------------------------------

save_impl_key_pos(Chars, Line, Col, Delta,
  #yamerl_parser{chars_idx = Chars_Index,
  tks_first_idx = First, tks_queued = Queued,
  cur_coll = Cur_Coll, ik_stack = [_ | Rest]} = Parser) ->
    Required = ?IN_BLOCK_CTX(Parser) andalso Cur_Coll#bcoll.indent == Col,
    if
        Parser#yamerl_parser.ik_allowed ->
            Impl_Key    = #impl_key{
              possible  = true,
              required  = Required,
              line      = Line,
              col       = Col,
              chars_idx = Chars_Index + Delta,
              token_idx = First + Queued
            },
            Parser#yamerl_parser{ik_stack = [Impl_Key | Rest]};
        Required ->
            Error = #yamerl_parsing_error{
              name   = required_implicit_key_not_allowed,
              line   = Line,
              column = Col
            },
            Parser1 = add_error(Parser, Error,
              "Required implicit key not allowed here", []),
            return(Chars, Line, Col, Delta, Parser1);
        true ->
            Parser
    end.

queue_impl_key(_, #yamerl_parser{last_token_endline = Line,
    ik_stack = [#impl_key{line = Impl_Line} = Impl_Key | _]} = Parser)
  when Line > Impl_Line andalso ?IN_BLOCK_CTX(Parser) ->
    %% An implicit key must not span several lines.
    Error = #yamerl_parsing_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not span several lines", []),
    queue_impl_key2(Parser1);
queue_impl_key(_, #yamerl_parser{last_token_endline = Line,
    ik_stack = [#impl_key{line = Impl_Line} = Impl_Key | _]} = Parser)
  when Line > Impl_Line andalso ?IN_FLOW_CTX(Parser) ->
    %% An implicit key must not span several lines.
    Error = #yamerl_parsing_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not span several lines", []),
    queue_impl_key2(Parser1);
queue_impl_key(Delta, #yamerl_parser{chars_idx = Index,
    ik_stack = [#impl_key{chars_idx = Impl_Index} = Impl_Key | _]} = Parser)
  when (Index + Delta) > Impl_Index + 1024 ->
    %% An implicit key must not take more than 1024 characters.
    Error = #yamerl_parsing_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not take more than 1024 characters", []),
    queue_impl_key2(Parser1);
queue_impl_key(_, Parser) ->
    queue_impl_key2(Parser).

queue_impl_key2(
  #yamerl_parser{ik_stack = [Impl_Key | Rest]} = Parser) ->
    Token = #yamerl_mapping_key{
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = queue_token(Parser, Token, Impl_Key#impl_key.token_idx),
    Parser1#yamerl_parser{
      ik_stack   = [?FAKE_IMPL_KEY | Rest],
      ik_allowed = false
    }.

remove_impl_key_pos(
  #yamerl_parser{ik_stack = [
  #impl_key{required = true, line = Line, col = Col} | _]} = Parser) ->
    %% This error is raised with the following examples:
    %%
    %% - entry
    %% unexpected-scalar
    %%
    %% ? key
    %% unexpected-scalar
    Error = #yamerl_parsing_error{
      name   = expected_sequence_entry_or_mapping_key_not_found,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Expected sequence entry or mapping implicit key not found", []),
    do_return(Parser1);
remove_impl_key_pos(
  #yamerl_parser{ik_stack = [_ | Rest]} = Parser) ->
    Parser#yamerl_parser{ik_stack = [?FAKE_IMPL_KEY | Rest]}.

%% -------------------------------------------------------------------
%% Tokens queueing.
%% -------------------------------------------------------------------

-spec queue_token(Parser, Token) ->
        New_Parser when
          Parser     :: yamerl_parser(),
          Token      :: yamerl_token(),
          New_Parser :: yamerl_parser().

queue_token(Parser, Token) ->
    queue_token_check_doc(Parser, Token, tail).

-spec queue_token(Parser, Token, Insert_At) ->
        New_Parser when
          Parser     :: yamerl_parser(),
          Token      :: yamerl_token(),
          Insert_At  :: position() | tail,
          New_Parser :: yamerl_parser().

queue_token(Parser, Token, Insert_At) ->
    queue_token_check_doc(Parser, Token, Insert_At).

%%
%% Handle document start/end.
%%

queue_token_check_doc(
  #yamerl_parser{doc_started = false} = Parser, Token, Insert_At)
  when is_record(Token, yamerl_stream_start) orelse
  is_record(Token, yamerl_stream_end) orelse
  is_record(Token, yamerl_doc_end) orelse
  is_record(Token, yamerl_yaml_directive) orelse
  is_record(Token, yamerl_tag_directive) orelse
  is_record(Token, yamerl_reserved_directive) ->
    %% Directives token outside a document are perfectly allowed.
    queue_token_pending_entry(Parser, Token, Insert_At);
queue_token_check_doc(
  #yamerl_parser{doc_started = false, last_tag = Tag} = Parser,
  Token, Insert_At) ->
    %% Other tokens starts the document automatically.
    {Line, Col} = case Tag of
        #yamerl_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        _ ->
            {?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)}
    end,
    %% We may need to shift the implicit key's index.
    Parser1 = update_impl_key_index(Parser, Line, Col),
    Parser2 = start_doc(Parser1, Line, Col, Insert_At),
    queue_token_pending_entry(Parser2, Token, next_insert_at(Insert_At, 1));
queue_token_check_doc(
  #yamerl_parser{doc_started = true} = Parser, Token, Insert_At)
  when is_record(Token, yamerl_stream_end) ->
    %% A document is automatically ended when we reach the end of the stream.
    Parser1 = end_doc(Parser, ?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token),
      Insert_At),
    queue_token_check_doc(Parser1, Token, Insert_At);
queue_token_check_doc(
  #yamerl_parser{doc_started = true} = Parser, Token, Insert_At) ->
    %% Document already started.
    queue_token_pending_entry(Parser, Token, Insert_At).

%%
%% Pending entries.
%%

queue_token_pending_entry(
  #yamerl_parser{pending_entry = false} = Parser,
  Token, Insert_At) ->
    queue_token_check_collection_start(Parser, Token, Insert_At);
queue_token_pending_entry(
  #yamerl_parser{pending_entry = true, last_tag = Tag} = Parser,
  Token, Insert_At) ->
    %% There's a pending entry: queue it now.
    Parser1 = Parser#yamerl_parser{
      pending_entry = false
    },
    {Line, Col} = case Tag of
        #yamerl_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        _ ->
            {?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)}
    end,
    Entry = #yamerl_sequence_entry{
      line   = Line,
      column = Col
    },
    %% We may need to shift the implicit key's index.
    Parser2 = update_impl_key_index(Parser1,
      ?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)),
    Parser3 = queue_token(Parser2, Entry, Insert_At),
    queue_token_check_collection_start(Parser3, Token,
      next_insert_at(Insert_At, 1)).

%%
%% Handle collection start.
%%

queue_token_check_collection_start(
  #yamerl_parser{
    cur_coll = #bcoll{kind = Kind, indent = Indent} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yamerl_sequence_entry{line = Line, column = Col} = Token, Insert_At)
  when ?IN_BLOCK_CTX(Parser) andalso
  (Col > Indent orelse (Kind == mapping andalso Col == Indent)) ->
    %% This is the first entry of a block sequence collection. Queue a
    %% collection-start token.
    Collection_Start = #yamerl_collection_start{
      style  = block,
      kind   = sequence,
      line   = Line,
      column = Col,
      tag    = ?COLL_SCALAR_DEFAULT_TAG(Line, Col)
    },
    Parser1 = queue_token(Parser, Collection_Start, Insert_At),
    %% Record the new block indent.
    New_Coll = #bcoll{kind = sequence, indent = Col},
    Parser2  = Parser1#yamerl_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token, next_insert_at(Insert_At, 1));
queue_token_check_collection_start(
  #yamerl_parser{
    cur_coll = #bcoll{indent = Indent} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yamerl_mapping_key{line = Line, column = Col} = Token, Insert_At)
  when ?IN_BLOCK_CTX(Parser) andalso Col > Indent ->
    %% This is the first key: value pair of a block mapping collection. Queue
    %% a collection-start token.
    Collection_Start = #yamerl_collection_start{
      style  = block,
      kind   = mapping,
      line   = Line,
      column = Col,
      tag    = ?COLL_SCALAR_DEFAULT_TAG(Line, Col)
    },
    Parser1 = queue_token(Parser, Collection_Start, Insert_At),
    %% Record the new block indent.
    New_Coll = #bcoll{kind = mapping, indent = Col},
    Parser2  = Parser1#yamerl_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token, next_insert_at(Insert_At, 1));
queue_token_check_collection_start(
  #yamerl_parser{
    cur_coll = #fcoll{kind = sequence} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yamerl_mapping_key{line = Line, column = Col} = Token, Insert_At)
  when ?IN_FLOW_CTX(Parser) ->
    %% This is a single key: value pair inside a flow sequence. Queue
    %% a collection-start token.
    Collection_Start = #yamerl_collection_start{
      style  = flow,
      kind   = mapping,
      line   = Line,
      column = Col,
      tag    = ?COLL_SCALAR_DEFAULT_TAG(Line, Col)
    },
    Parser1 = queue_token(Parser, Collection_Start, Insert_At),
    %% Flag this mapping as single pair inside flow sequence.
    New_Coll = #fcoll{kind = single_mapping},
    Parser2  = Parser1#yamerl_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token, next_insert_at(Insert_At, 1));
queue_token_check_collection_start(Parser, Token, Insert_At) ->
    queue_token_keep_last_pos(Parser, Token, Insert_At).

%%
%% Remember last sequence entry, mapping key and mapping value
%% positions.
%%

queue_token_keep_last_pos(
  #yamerl_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yamerl_sequence_entry{line = Line, column = Col} = Token, Insert_At) ->
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{kidx = Index, kline = Line, kcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{kidx = Index, kline = Line, kcol = Col}
    end,
    Parser1 = Parser#yamerl_parser{
      cur_coll = Coll1
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(
  #yamerl_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yamerl_mapping_key{line = Line, column = Col} = Token, Insert_At) ->
    %% While we're handling a mapping key, tell that we're not waiting
    %% for a key: value pair anymore.
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{kidx = Index, kline = Line, kcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{kidx = Index, kline = Line, kcol = Col}
    end,
    Parser1 = Parser#yamerl_parser{
      cur_coll           = Coll1,
      waiting_for_kvpair = false
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(
  #yamerl_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yamerl_mapping_value{line = Line, column = Col} = Token, Insert_At) ->
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{vidx = Index, vline = Line, vcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{vidx = Index, vline = Line, vcol = Col}
    end,
    Parser1 =Parser#yamerl_parser{
      cur_coll = Coll1
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(Parser, Token, Insert_At) ->
    queue_token_json_like(Parser, Token, Insert_At).

%%
%% JSON-like tokens.
%%

queue_token_json_like(Parser, Token, tail)
  when ?IS_JSON_LIKE(Token) ->
    Parser1 = Parser#yamerl_parser{
      last_is_json_like = true
    },
    do_queue_token(Parser1, Token, tail);
queue_token_json_like(Parser, Token, tail) ->
    Parser1 = Parser#yamerl_parser{
      last_is_json_like = false
    },
    do_queue_token(Parser1, Token, tail);
queue_token_json_like(Parser, Token, Insert_At) ->
    do_queue_token(Parser, Token, Insert_At).

%%
%% Insert the token at the end of the queue or a given index.
%%

do_queue_token(#yamerl_parser{tokens = Tokens, tks_queued = Queued} = Parser,
  Token, tail) ->
    Tokens1 = [Token | Tokens],
    Parser1 = Parser#yamerl_parser{
      tokens     = Tokens1,
      tks_queued = Queued + 1
    },
    emit_tokens(Parser1);
do_queue_token(#yamerl_parser{tokens = Tokens, tks_queued = Queued,
    tks_first_idx = First} = Parser,
  Token, Insert_At) ->
    Split        = Queued - (Insert_At - First),
    {Head, Tail} = lists:split(Split, Tokens),
    Parser1 = Parser#yamerl_parser{
      tokens     = Head ++ [Token] ++ Tail,
      tks_queued = Queued + 1
    },
    emit_tokens(Parser1).

%%
%% Emit tokens which are ready using the callback function.
%% At this point, all tokens coming in are in the final order. We may
%% only do some checks, add empty nodes and set nodes' tag property.
%%

emit_tokens(
  #yamerl_parser{tokens = Tokens, tks_first_idx = First} = Parser) ->
    Max = max_token_idx_ready(Parser),
    emit_tokens2(Parser, lists:reverse(Tokens), First, Max).

emit_tokens2(
  #yamerl_parser{last_tag = undefined,
    tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yamerl_tag{} = Tag | Rest], Idx, Max)
  when Idx =< Max ->
    %% Keep the tag outside of the token queue. It'll be attached to a
    %% following node.
    Parser1 = Parser#yamerl_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_tag      = Tag
    },
    emit_tokens2(Parser1, Rest, Idx + 1, Max);
emit_tokens2(
  #yamerl_parser{tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yamerl_tag{line = Line, column = Col} = Tag | Rest], Idx, Max)
  when Idx =< Max ->
    %% Error: several tags for the same node.
    Error = #yamerl_parsing_error{
      name   = multiple_tag_properties,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple tag properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yamerl_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_tag      = Tag
    },
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(#yamerl_parser{last_anchor = undefined,
    tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yamerl_anchor{} = Anchor | Rest], Idx, Max)
  when Idx =< Max ->
    %% Keep the anchor outside of the token queue. It'll be emitted with
    %% its attached node. We also use this to check if multiple anchors
    %% are attached to the same node.
    Parser1 = Parser#yamerl_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_anchor   = Anchor
    },
    emit_tokens2(Parser1, Rest, Idx + 1, Max);
emit_tokens2(
  #yamerl_parser{tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yamerl_anchor{line = Line, column = Col} = Anchor | Rest], Idx, Max)
  when Idx =< Max ->
    %% Error: several tags for the same node.
    Error = #yamerl_parsing_error{
      name   = multiple_anchor_properties,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple anchor properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yamerl_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_anchor   = Anchor
    },
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(#yamerl_parser{last_token = Last} = Parser,
  [Token | Rest], Idx , Max)
  when Idx =< Max ->
    %% Run some checks:
    %%   o  Can "Last" and "Token" be in a row?
    %%   o  Do we need to insert an empty scalar?
    Parser1 = check_tokens_in_a_row(Parser, Last, Token),
    %% Handle properties and execute the specified callback function (or
    %% queue the token).
    Parser2 = handle_anchor_property(Parser1, Token),
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(Parser, Tokens, _, _) ->
    Tokens1 = lists:reverse(Tokens),
    Parser#yamerl_parser{
      tokens = Tokens1
    }.

%% Check if a token can follow another and insert empty node if
%% necessary.
check_tokens_in_a_row(
  #yamerl_parser{last_tag = Tag, last_anchor = Anchor,
    tks_queued = Queued} = Parser,
  Token1, Token2) when
  %% Empty sequence entry.
  (is_record(Token1, yamerl_sequence_entry) andalso
   (?IN_BLOCK_CTX(Parser) orelse
    Tag /= undefined orelse
    Anchor /= undefined) andalso
   (is_record(Token2, yamerl_sequence_entry) orelse
    is_record(Token2, yamerl_collection_end))) orelse
  %% Empty mapping key.
  (is_record(Token1, yamerl_mapping_key) andalso
   (is_record(Token2, yamerl_mapping_value) orelse
    is_record(Token2, yamerl_collection_end))) orelse
  %% Empty mapping value.
  (is_record(Token1, yamerl_mapping_value) andalso
   (is_record(Token2, yamerl_mapping_key) orelse
    is_record(Token2, yamerl_collection_end))) orelse
  %% Empty mapping value.
  (is_record(Token1, yamerl_mapping_value) andalso
   (is_record(Token2, yamerl_mapping_key) orelse
    is_record(Token2, yamerl_collection_end))) orelse
  %% Empty document.
  (is_record(Token1, yamerl_doc_start) andalso
   is_record(Token2, yamerl_doc_end)) orelse
  %% Anchor alone.
  (is_record(Token1, yamerl_anchor) andalso
   (is_record(Token2, yamerl_mapping_value) orelse
    is_record(Token2, yamerl_collection_end) orelse
    is_record(Token2, yamerl_doc_end))) ->
    %% Token1 is followed by an empty scalar.
    {Line, Col} = case Tag of
        #yamerl_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        undefined ->
            case Token1 of
                #yamerl_doc_start{} ->
                    {?TOKEN_LINE(Token2), ?TOKEN_COLUMN(Token2)};
                _ ->
                    {?TOKEN_LINE(Token1), ?TOKEN_COLUMN(Token1)}
            end
    end,
    Empty = empty_scalar(Line, Col),
    Parser1 = Parser#yamerl_parser{
      tks_queued = Queued + 1
    },
    handle_anchor_property(Parser1, Empty);
check_tokens_in_a_row(Parser, Token1, Token2) when
  (is_record(Token1, yamerl_scalar) orelse
   is_record(Token1, yamerl_collection_end)) andalso
  (is_record(Token2, yamerl_scalar) orelse
   is_record(Token2, yamerl_collection_start)) ->
    %% Token2 can't follow Token1.
    Error = #yamerl_parsing_error{
      name   = unexpected_token,
      token  = Token2,
      line   = ?TOKEN_LINE(Token2),
      column = ?TOKEN_COLUMN(Token2)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected \"~s\" token following a \"~s\" token",
      [?TOKEN_NAME(Token2), ?TOKEN_NAME(Token1)]),
    do_return(Parser1);
check_tokens_in_a_row(Parser, _, _) ->
    Parser.

handle_anchor_property(
  #yamerl_parser{last_anchor = undefined} = Parser, Token) ->
    handle_tag_property(Parser, Token);
handle_anchor_property(
  #yamerl_parser{last_anchor = Anchor} = Parser, Token) when
  (is_record(Token, yamerl_collection_start) andalso
   Anchor#yamerl_anchor.line < Token#yamerl_collection_start.line) orelse
  (is_record(Token, yamerl_scalar) andalso
   (Anchor#yamerl_anchor.line < Token#yamerl_scalar.line orelse
    (Anchor#yamerl_anchor.line == Token#yamerl_scalar.line andalso
     Anchor#yamerl_anchor.column =< Token#yamerl_scalar.column))) ->
    Parser1 = do_emit_token(Parser, Anchor),
    Parser2 = Parser1#yamerl_parser{
      last_anchor = undefined
    },
    handle_tag_property(Parser2, Token);
handle_anchor_property(Parser, Token) ->
    handle_tag_property(Parser, Token).

handle_tag_property(
  #yamerl_parser{last_tag = undefined} = Parser, Token) ->
    do_emit_token(Parser, Token);
handle_tag_property(
  #yamerl_parser{last_tag = Tag} = Parser, Token) when
  (is_record(Token, yamerl_collection_start) andalso
   Tag#yamerl_tag.line < Token#yamerl_collection_start.line) orelse
  (is_record(Token, yamerl_collection_start) andalso
   Token#yamerl_collection_start.style =:= flow andalso
   (Tag#yamerl_tag.line < Token#yamerl_collection_start.line orelse
    (Tag#yamerl_tag.line == Token#yamerl_collection_start.line andalso
     Tag#yamerl_tag.column =< Token#yamerl_collection_start.column))) orelse
  (is_record(Token, yamerl_scalar) andalso
   (Tag#yamerl_tag.line < Token#yamerl_scalar.line orelse
    (Tag#yamerl_tag.line == Token#yamerl_scalar.line andalso
     Tag#yamerl_tag.column =< Token#yamerl_scalar.column))) ->
    %% The tag property is attached to this token.
    Token1 = case Token of
        #yamerl_scalar{} ->
            Token#yamerl_scalar{
              tag = Tag
            };
        #yamerl_collection_start{} ->
            Token#yamerl_collection_start{
              tag = Tag
            }
    end,
    %% Clear the pending tag property.
    Parser1 = Parser#yamerl_parser{
      last_tag = undefined
    },
    do_emit_token(Parser1, Token1);
handle_tag_property(Parser, Token) ->
    do_emit_token(Parser, Token).

do_emit_token(
  #yamerl_parser{token_fun = Not_Fun,
    tks_queued = Queued, tks_first_idx = First,
    tks_emitted = Emitted, tks_ready = Ready} = Parser,
  Token) when Not_Fun == acc orelse Not_Fun == drop ->
    %% The anchor was already counted when first removed from the queue.
    {Queued1, First1} = case ?TOKEN_NAME(Token) of
        yamerl_anchor -> {Queued,     First};
        _           -> {Queued - 1, First + 1}
    end,
    Ready1 = case Not_Fun of
        acc  -> [Token | Ready];
        drop -> Ready
    end,
    Parser#yamerl_parser{
      tks_queued    = Queued1,
      tks_first_idx = First1,
      tks_emitted   = Emitted + 1,
      last_token    = Token,
      tks_ready     = Ready1
    };
do_emit_token(
  #yamerl_parser{token_fun = Fun,
    tks_queued = Queued, tks_first_idx = First, tks_emitted = Emitted} = Parser,
  Token) ->
    %% The anchor was already counted when first removed from the queue.
    {Queued1, First1} = case ?TOKEN_NAME(Token) of
        yamerl_anchor -> {Queued,     First};
        _           -> {Queued - 1, First + 1}
    end,
    try
        Fun1 = case Fun(Token) of
            ok       -> Fun;
            {ok, F1} -> F1
        end,
        Parser#yamerl_parser{
          token_fun     = Fun1,
          tks_queued    = Queued1,
          tks_first_idx = First1,
          tks_emitted   = Emitted + 1,
          last_token    = Token
        }
    catch
        throw:Error when is_record(Error, yamerl_parsing_error) ->
            Parser1 = add_error(Parser, Error),
            Parser2 = Parser1#yamerl_parser{
              tks_queued    = Queued1,
              tks_first_idx = First1,
              tks_emitted   = Emitted + 1,
              last_token    = Token
            },
            if
                Error#yamerl_parsing_error.type == error -> do_return(Parser2);
                true                                     -> Parser2
            end;
        throw:{Fun2, Error} when is_record(Error, yamerl_parsing_error) ->
            Parser1 = add_error(Parser, Error),
            Parser2 = Parser1#yamerl_parser{
              token_fun     = Fun2,
              tks_queued    = Queued1,
              tks_first_idx = First1,
              tks_emitted   = Emitted + 1,
              last_token    = Token
            },
            if
                Error#yamerl_parsing_error.type == error -> do_return(Parser2);
                true                                     -> Parser2
            end
    end.

next_insert_at(tail, _)      -> tail;
next_insert_at(Insert_At, N) -> Insert_At + N.

max_token_idx_ready(#yamerl_parser{ik_stack = Stack,
  tks_first_idx = First, tks_queued = Queued}) ->
    max_token_idx_ready2(First + Queued - 1, lists:reverse(Stack)).

max_token_idx_ready2(_, [#impl_key{possible = true, token_idx = Idx} | _]) ->
    Idx - 1;
max_token_idx_ready2(All, [_ | Rest]) ->
    max_token_idx_ready2(All, Rest);
max_token_idx_ready2(All, []) ->
    All.

%% A token was inserted before the potential implicit key: move the
%% key's index.
update_impl_key_index(#yamerl_parser{ik_stack = Stack} = Parser, Line, Col) ->
    update_impl_key_index2(Parser, Stack, Line, Col, []).

update_impl_key_index2(Parser,
  [#impl_key{token_idx = Index, line = Key_L, col = Key_C} = Impl_Key | Rest],
  Line, Col, Result)
  when is_integer(Key_L) andalso is_integer(Key_C) andalso
  (Line < Key_L orelse (Line == Key_L andalso Col =< Key_C)) ->
    Impl_Key1 = Impl_Key#impl_key{
      token_idx = Index + 1
    },
    Result1 = [Impl_Key1 | Result],
    update_impl_key_index2(Parser, Rest, Line, Col, Result1);
update_impl_key_index2(Parser,
  [#impl_key{line = Key_L, col = Key_C} | _] = Rest,
  Line, Col, Result)
  when is_integer(Key_L) andalso is_integer(Key_C) andalso
  (Line > Key_L orelse (Line == Key_L andalso Col > Key_C)) ->
    Parser#yamerl_parser{
      ik_stack = lists:reverse(Result) ++ Rest
    };
update_impl_key_index2(Parser,
  [Impl_Key | Rest], Line, Col, Result) ->
    Result1 = [Impl_Key | Result],
    update_impl_key_index2(Parser, Rest, Line, Col, Result1);
update_impl_key_index2(Parser, [], _, _, Result) ->
    Parser#yamerl_parser{
      ik_stack = lists:reverse(Result)
    }.

%% -------------------------------------------------------------------
%% Tag resolution.
%% -------------------------------------------------------------------

setup_default_tags(#yamerl_parser{options = Options} = Parser) ->
    Tags  = dict:new(),
    %% By default, "!" is resolved as "!" and the tag is considered
    %% local.
    Tags1 = dict:store({default, "!"},  "!", Tags),
    %% By default, "!!" is resolved as "tag:yaml.org,2002:" and is used
    %% by the YAML tags repository.
    Tags2 = dict:store({default, "!!"}, "tag:yaml.org,2002:", Tags1),
    %% Non-specific tags are associated to nodes which don't have an
    %% explicit tag or to those with the "!" explicit non-specific tag.
    %% The non-specific tags are resolved using a schema.
    Tags3 = dict:store({default, {non_specific, "!"}}, "tag:yaml.org,2002:",
      Tags2),
    Tags4 = dict:store({default, {non_specific, "?"}}, "tag:yaml.org,2002:",
      Tags3),
    Tags5 = case proplists:get_value(default_tags, Options) of
        undefined ->
            Tags4;
        List ->
            Fun = fun({Prefix, Value}, T) ->
                dict:store({default, Prefix}, Value, T)
            end,
            lists:foldl(Fun, Tags4, List)
    end,
    Parser#yamerl_parser{
      tags = Tags5
    }.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

%% @private

option_names() ->
    [
      default_tags,
      doc_version,
      io_blocksize,
      token_fun
    ].

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({default_tags, List}) when is_list(List) ->
    %% This fun() returns true for any invalid entries, to keep only
    %% those.
    Fun = fun
        ({{non_specific, A}, B}) ->
            not (io_lib:char_list(A) andalso io_lib:char_list(B));
        ({A, B}) ->
            not (io_lib:char_list(A) andalso io_lib:char_list(B));
        (_) ->
            true
    end,
    case lists:filter(Fun, List) of
        [] -> true;
        _  -> false
    end;
is_option_valid({doc_version, {Major, Minor}}) when
  is_integer(Major) andalso Major >= 0 andalso
  is_integer(Minor) andalso Minor >= 0 ->
    true;
is_option_valid({io_blocksize, BS})
  when is_integer(BS) andalso BS >= 1 ->
    true;
is_option_valid({token_fun, acc}) ->
    true;
is_option_valid({token_fun, drop}) ->
    true;
is_option_valid({token_fun, Fun})
  when is_function(Fun, 1) ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yamerl_invalid_option{
      option = Option
    },
    Error1 = case Option of
        {default_tags, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"default_tags\": "
              "it must be a list of {Prefix, Prefix_Value}"
            };
        {doc_version, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"doc_version\": "
              "it must be a tuple of the form {Major, Minor} "
              "where Major and Minor are positive integers"
            };
        {io_blocksize, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"io_blocksize\": "
              "it must be a positive integer, expressed in bytes"
            };
        {token_fun, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"token_fun\": "
              "it must be a function taking the next token as "
              "its sole argument, or the atom 'acc' or 'drop'"
            };
        _ ->
            yamerl_errors:format(Error, "Unknown option \"~w\"", [Option])
    end,
    yamerl_errors:throw(Error1).

empty_scalar(Line, Col) ->
    #yamerl_scalar{
      style    = flow,
      substyle = plain,
      text     = "",
      line     = Line,
      column   = Col,
      tag      = ?PLAIN_SCALAR_DEFAULT_TAG(Line, Col)
    }.

check_for_closed_block_collections([C | _] = Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #bcoll{kind = sequence, indent = At_Col},
  parent_colls = [#bcoll{kind = mapping,  indent = At_Col} = Parent_Coll |
  Colls]} = Parser, At_Col) when C /= $- ->
    %% The sequence has the same indentation level than its parent
    %% mapping. The next token has this same indentation but is not a
    %% sequence entry (denoted by the '-' character). Let's close it but
    %% not the parent mapping.
    Token    = #yamerl_collection_end{
      style  = block,
      kind   = sequence,
      line   = Parser#yamerl_parser.last_token_endline,
      column = Parser#yamerl_parser.last_token_endcol
    },
    Parser1 = queue_token(Parser, Token),
    %% Remove its indentation from the stack.
    Parser2 = Parser1#yamerl_parser{
      cur_coll     = Parent_Coll,
      parent_colls = Colls
    },
    check_for_closed_block_collections(Chars, Line, Col, Delta, Parser2,
      At_Col);
check_for_closed_block_collections(Chars, Line, Col, Delta,
  #yamerl_parser{cur_coll = #bcoll{kind = Kind, indent = Indent},
  parent_colls = [Parent_Coll | Colls]} = Parser, At_Col)
  when At_Col < Indent ->
    Parser1 = finish_incomplete_block_entries(Line, Col, Parser),
    %% Emit a token to signal the end of the block collection.
    Token    = #yamerl_collection_end{
      style  = block,
      kind   = Kind,
      line   = Parser1#yamerl_parser.last_token_endline,
      column = Parser1#yamerl_parser.last_token_endcol
    },
    Parser2 = queue_token(Parser1, Token),
    %% Remove its indentation from the stack.
    Parser3 = Parser2#yamerl_parser{
      cur_coll     = Parent_Coll,
      parent_colls = Colls
    },
    check_for_closed_block_collections(Chars, Line, Col, Delta, Parser3,
      At_Col);
check_for_closed_block_collections(_, _, _, _, Parser, _) ->
    Parser.

is_uri_valid(Parser, #yamerl_tag{uri = {non_specific, _}}) ->
    Parser;
is_uri_valid(Parser, #yamerl_tag{uri = [$! | _]}) ->
    Parser;
is_uri_valid(Parser, #yamerl_tag{uri = URI} = Tag) ->
    is_uri_scheme_valid1(Parser, Tag, URI);
is_uri_valid(Parser, #yamerl_tag_directive{prefix = [$! | _]}) ->
    Parser;
is_uri_valid(Parser, #yamerl_tag_directive{prefix = URI} = Directive) ->
    is_uri_scheme_valid1(Parser, Directive, URI).

is_uri_scheme_valid1(Parser, Token, [C | Rest]) when
  (C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) ->
    is_uri_scheme_valid2(Parser, Token, Rest);
is_uri_scheme_valid1(Parser, Token, [_ | _]) ->
    Error = #yamerl_parsing_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []);
is_uri_scheme_valid1(Parser, Token, []) ->
    Error = #yamerl_parsing_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Unexpected end of URI", []).

is_uri_scheme_valid2(Parser, Token, [C | Rest]) when
  (C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) orelse
  (C >= $0 andalso C =< $9) orelse
  C == $+ orelse C == $. orelse C == $- ->
    is_uri_scheme_valid2(Parser, Token, Rest);
is_uri_scheme_valid2(Parser, Token, [$: | Rest]) ->
    is_uri_hier_part_valid(Parser, Token, Rest);
is_uri_scheme_valid2(Parser, Token, [_ | _]) ->
    Error = #yamerl_parsing_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []);
is_uri_scheme_valid2(Parser, Token, []) ->
    Error = #yamerl_parsing_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Unexpected end of URI", []).

is_uri_hier_part_valid(Parser, Token, [C | Rest]) when ?IS_URI_CHAR(C) ->
    is_uri_hier_part_valid(Parser, Token, Rest);
is_uri_hier_part_valid(Parser, _, []) ->
    Parser;
is_uri_hier_part_valid(Parser, Token, [_ | _]) ->
    Error = #yamerl_parsing_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []).

add_error(Parser, Error, Format, Args) ->
    %% Format error message.
    Error1 = yamerl_errors:format(Error, Format, Args),
    add_error(Parser, Error1).

add_error(
  #yamerl_parser{has_errors = Has_Errors, errors = Errors} = Parser, Error) ->
    %% Update has_errors flag.
    Has_Errors1 = if
        Has_Errors -> Has_Errors;
        true       -> Error#yamerl_parsing_error.type == error
    end,
    Parser#yamerl_parser{
      has_errors = Has_Errors1,
      errors     = [Error | Errors]
    }.

-spec suspend_parsing(Chars, Line, Col, Delta, Parser, Fun) ->
        Ret | no_return() when
        Chars      :: unicode_string(),
        Line       :: position(),
        Col        :: position(),
        Delta      :: non_neg_integer(),
        Parser     :: yamerl_parser(),
        Fun        :: stream_state_fun(),
        Ret        :: {continue, New_Parser} | Parser,
        New_Parser :: yamerl_parser().

suspend_parsing(Chars, Line, Col, Delta, Parser, Fun) ->
    Parser1 = ?FLUSH_TO_PARSER(Chars, Line, Col, Delta, Parser),
    Parser2 = Parser1#yamerl_parser{
      stream_state = Fun
    },
    do_return(Parser2).

suspend_parsing(Chars, Line, Col, Delta, Parser, Fun, Ctx) ->
    Fun1 = fun(Ch, Li, Co, De, P) -> Fun(Ch, Li, Co, De, P, Ctx) end,
    suspend_parsing(Chars, Line, Col, Delta, Parser, Fun1).

return(Chars, Line, Col, Delta, Parser) ->
    Parser1 = ?FLUSH_TO_PARSER(Chars, Line, Col, Delta, Parser),
    do_return(Parser1).

-spec do_return(Parser) -> {continue, Parser} | Parser | no_return() when
        Parser :: yamerl_parser().

do_return(#yamerl_parser{has_errors = true, errors = Errors}) ->
    yamerl_errors:throw(Errors);
do_return(#yamerl_parser{raw_eos = true, chars_len = 0} = Parser) ->
    Parser;
do_return(Parser) ->
    {continue, Parser}.

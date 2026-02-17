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

-ifndef(yamerl_tokens_hrl).
-define(yamerl_tokens_hrl, true).

-include("yamerl_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Supported versions range.
%% -------------------------------------------------------------------

-define(MIN_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MIN_YAML_MINOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MINOR_VERSION_SUPPORTED, 2).
-define(IMPLICIT_YAML_VERSION, {
    ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
    ?MAX_YAML_MINOR_VERSION_SUPPORTED
  }).

%% -------------------------------------------------------------------
%% Stream tokens.
%% -------------------------------------------------------------------

%% Stream start: emitted at the beginning of a stream. It's the very
%% first emitted token.
-record(yamerl_stream_start, {
    line     = 1                     :: position(),
    column   = 1                     :: position(),
    encoding = utf8                  :: encoding()
  }).
-type yamerl_stream_start()          :: #yamerl_stream_start{}.

%% Stream end: emitted at the end of a stream. It's the last emitted
%% token.
-record(yamerl_stream_end, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yamerl_stream_end()            :: #yamerl_stream_end{}.

%% -------------------------------------------------------------------
%% Document tokens.
%% -------------------------------------------------------------------

%% Document start: emitted at the beginning of a document: either when
%% an explicit "directives-end" marker is found or before the first
%% token that is not directive or a comment token.
-record(yamerl_doc_start, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    version = ?IMPLICIT_YAML_VERSION :: document_version(),
    tags    = dict:new()             :: tags_table()
  }).
-type yamerl_doc_start()             :: #yamerl_doc_start{}.

%% Document end: emitted at the end of a document: either when an
%% explicit "document-end" or "directives-end" marker is found or before
%% a stream end.
-record(yamerl_doc_end, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yamerl_doc_end()               :: #yamerl_doc_end{}.

%% YAML directive: emitted when a YAML directive is parsed.
-record(yamerl_yaml_directive, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    version = ?IMPLICIT_YAML_VERSION :: document_version() | undefined
  }).
-type yamerl_yaml_directive()        :: #yamerl_yaml_directive{
                                          version :: document_version()
                                        }.

%% TAG directive: emitted when a TAG directive is parsed.
-record(yamerl_tag_directive, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    handle  = "!"                    :: tag_handle() | [] | undefined,
    prefix  = "!"                    :: tag_prefix() | [] | undefined
  }).
-type yamerl_tag_directive()         :: #yamerl_tag_directive{
                                          handle :: tag_handle(),
                                          prefix :: tag_prefix()
                                        }.

%% Reserved directive: emitted when an unknown directive is found.
-record(yamerl_reserved_directive, {
    line       = 1                   :: position(),
    column     = 1                   :: position(),
    name       = "RESERVED"          :: nonempty_string(),
    args       = []                  :: [nonempty_string()],
    args_count = 0                   :: non_neg_integer()
  }).
-type yamerl_reserved_directive()    :: #yamerl_reserved_directive{}.

%% -------------------------------------------------------------------
%% Node properties.
%% -------------------------------------------------------------------

-record(yamerl_anchor, {
    line   = 1                       :: position(),
    column = 1                       :: position(),
    name   = "anchor"                :: nonempty_string()
  }).
-type yamerl_anchor()                :: #yamerl_anchor{}.

-record(yamerl_alias, {
    line   = 1                       :: position(),
    column = 1                       :: position(),
    name   = "alias"                 :: nonempty_string()
  }).
-type yamerl_alias()                 :: #yamerl_alias{}.

-record(yamerl_tag, {
    line   = 1                       :: position(),
    column = 1                       :: position(),
    uri    = "!"                     :: tag_uri() | []
  }).
-type yamerl_tag()                   :: #yamerl_tag{uri :: tag_uri()}.

%% -------------------------------------------------------------------
%% Node tokens.
%% -------------------------------------------------------------------

%% Scalar: emitted when a scalar, no matter its style, is found.
-record(yamerl_scalar, {
    line     = 1                     :: position(),
    column   = 1                     :: position(),
    tag      = #yamerl_tag{}         :: yamerl_tag(),
    style    = flow                  :: style(),
    substyle = plain                 :: scalar_substyle(),
    text     = ""                    :: string()
  }).
-type yamerl_scalar()                :: #yamerl_scalar{
                                          tag :: yamerl_tag()
                                        }.

%% Collection start: emitted when a sequence or a mapping is opened.
-record(yamerl_collection_start, {
    line   = 1                       :: position(),
    column = 1                       :: position(),
    tag    = #yamerl_tag{}           :: yamerl_tag(),
    style  = block                   :: style(),
    kind   = sequence                :: collection_kind()
  }).
-type yamerl_collection_start()      :: #yamerl_collection_start{}.

%% Collection end: emitted when a sequence or a mapping is closed.
-record(yamerl_collection_end, {
    line   = 1                       :: position(),
    column = 1                       :: position(),
    style  = block                   :: style(),
    kind   = sequence                :: collection_kind()
  }).
-type yamerl_collection_end()        :: #yamerl_collection_end{}.

%% -------------------------------------------------------------------
%% Collection entries.
%% -------------------------------------------------------------------

%% Sequence entry: emitted before each sequence entry.
-record(yamerl_sequence_entry, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yamerl_sequence_entry()        :: #yamerl_sequence_entry{}.

%% Mapping key: emitted before each mapping key.
-record(yamerl_mapping_key, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yamerl_mapping_key()           :: #yamerl_mapping_key{}.

%% Mapping value: emitted before each mapping value.
-record(yamerl_mapping_value, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yamerl_mapping_value()         :: #yamerl_mapping_value{}.

%% -------------------------------------------------------------------
%% Final data type specifications.
%% -------------------------------------------------------------------

-type yamerl_token() ::
        yamerl_stream_start()
      | yamerl_stream_end()
      | yamerl_doc_start()
      | yamerl_doc_end()
      | yamerl_yaml_directive()
      | yamerl_tag_directive()
      | yamerl_reserved_directive()
      | yamerl_scalar()
      | yamerl_collection_start()
      | yamerl_collection_end()
      | yamerl_sequence_entry()
      | yamerl_mapping_key()
      | yamerl_mapping_value()
      | yamerl_tag()
      | yamerl_anchor()
      | yamerl_alias().

%% A partial token appears in a #yamerl_parser_error{}.
-type yamerl_partial_token() ::
        #yamerl_stream_start{}
      | #yamerl_stream_end{}
      | #yamerl_doc_start{}
      | #yamerl_doc_end{}
      | #yamerl_yaml_directive{}
      | #yamerl_tag_directive{}
      | #yamerl_reserved_directive{}
      | #yamerl_scalar{}
      | #yamerl_collection_start{}
      | #yamerl_collection_end{}
      | #yamerl_sequence_entry{}
      | #yamerl_mapping_key{}
      | #yamerl_mapping_value{}
      | #yamerl_tag{}
      | #yamerl_anchor{}
      | #yamerl_alias{}.

%% -------------------------------------------------------------------
%% Macros to access common members of the token records.
%% -------------------------------------------------------------------

-define(TOKEN_NAME(T),   element(1, T)).
-define(TOKEN_LINE(T),   element(#yamerl_scalar.line, T)).
-define(TOKEN_COLUMN(T), element(#yamerl_scalar.column, T)).

-endif.

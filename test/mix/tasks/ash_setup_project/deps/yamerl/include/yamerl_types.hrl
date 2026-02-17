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

-ifndef(yamerl_types_hrl).
-define(yamerl_types_hrl, true).

%% -------------------------------------------------------------------
%% Data types specifications.
%% -------------------------------------------------------------------

%% YAML version which a document conforms to.
%% The tuple has the form {Major, Minor}
-type document_version() :: {non_neg_integer(), non_neg_integer()}.

%% Stream encoding.
%% Only Unicode encodings are accepted.
-type encoding()         :: utf8
                          | {utf16, little | big}
                          | {utf32, little | big}.
-type unicode_char()     :: integer().
-type unicode_string()   :: [unicode_char()].
%% unicode:external_unicode_binary() (UTF16/32) not exported.
-type unicode_binary()   :: unicode:unicode_binary().
-type unicode_data()     :: unicode_string()
                          | unicode_binary().

%% Tag declaration and usage.
%% A tag handle is used in TAG directives and in front of a node. A tag
%% prefix is used in a TAG directive. The final tag is used on nodes
%% after tag resolution. The tags table stores all tags declaration.
-type tag_handle()       :: nonempty_string().
-type tag_prefix()       :: nonempty_string().
-type tag_uri()          :: nonempty_string()
                          | {non_specific, [33 | 63]}. %% "!" | "?"
-type tags_table()       :: dict:dict(term(), term()).

%% Node styles, substyles and kinds.
-type style()            :: block
                          | flow.
-type scalar_substyle()  :: literal
                          | folded
                          | double_quoted
                          | single_quoted
                          | plain.
-type collection_kind()  :: sequence
                          | mapping.

%% Position of a token (line and column).
-type position()         :: pos_integer().

-endif.

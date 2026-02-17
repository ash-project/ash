defmodule YamlElixir.Records do
  require Record

  Record.defrecord(
    :yamerl_constr,
    Record.extract(:yamerl_constr, from_lib: "yamerl/include/internal/yamerl_constr.hrl")
  )

  # Unable to extract here since the record defined in Erlang uses an Erlang macro
  # which breaks the extraction.
  Record.defrecord(
    :unfinished_node,
    module: nil,
    path: nil,
    pres: nil,
    priv: nil
  )

  Record.defrecord(
    :yamerl_map,
    Record.extract(:yamerl_map, from_lib: "yamerl/include/yamerl_nodes.hrl")
  )

  Record.defrecord(
    :yamerl_collection_start,
    Record.extract(:yamerl_collection_start, from_lib: "yamerl/include/yamerl_tokens.hrl")
  )

  Record.defrecord(
    :yamerl_collection_end,
    Record.extract(:yamerl_collection_end, from_lib: "yamerl/include/yamerl_tokens.hrl")
  )

  Record.defrecord(
    :yamerl_mapping_key,
    Record.extract(:yamerl_mapping_key, from_lib: "yamerl/include/yamerl_tokens.hrl")
  )

  Record.defrecord(
    :yamerl_mapping_value,
    Record.extract(:yamerl_mapping_value, from_lib: "yamerl/include/yamerl_tokens.hrl")
  )

  Record.defrecord(
    :yamerl_tag,
    Record.extract(:yamerl_tag, from_lib: "yamerl/include/yamerl_tokens.hrl")
  )

  Record.defrecord(
    :yamerl_parsing_error,
    Record.extract(:yamerl_parsing_error, from_lib: "yamerl/include/yamerl_errors.hrl")
  )

  Record.defrecord(
    :yaml_elixir_keyword_list,
    module: __MODULE__,
    tag: "tag:yaml_elixir,2019:keyword_list",
    pres: [],
    pairs: []
  )
end

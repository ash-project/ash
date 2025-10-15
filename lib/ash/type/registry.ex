# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Registry do
  @moduledoc false

  @builtin_short_names [
    map: Ash.Type.Map,
    keyword: Ash.Type.Keyword,
    term: Ash.Type.Term,
    atom: Ash.Type.Atom,
    tuple: Ash.Type.Tuple,
    string: Ash.Type.String,
    integer: Ash.Type.Integer,
    file: Ash.Type.File,
    float: Ash.Type.Float,
    duration_name: Ash.Type.DurationName,
    function: Ash.Type.Function,
    boolean: Ash.Type.Boolean,
    struct: Ash.Type.Struct,
    uuid: Ash.Type.UUID,
    uuid_v7: Ash.Type.UUIDv7,
    binary: Ash.Type.Binary,
    date: Ash.Type.Date,
    time: Ash.Type.Time,
    time_usec: Ash.Type.TimeUsec,
    decimal: Ash.Type.Decimal,
    ci_string: Ash.Type.CiString,
    naive_datetime: Ash.Type.NaiveDatetime,
    utc_datetime: Ash.Type.UtcDatetime,
    utc_datetime_usec: Ash.Type.UtcDatetimeUsec,
    datetime: Ash.Type.DateTime,
    duration: Ash.Type.Duration,
    url_encoded_binary: Ash.Type.UrlEncodedBinary,
    union: Ash.Type.Union,
    module: Ash.Type.Module,
    vector: Ash.Type.Vector
  ]

  @builtin_types Keyword.values(@builtin_short_names)

  @custom_short_names Application.compile_env(:ash, :custom_types, [])
  @short_names @custom_short_names ++ @builtin_short_names

  @doc false
  def builtin_short_names, do: @builtin_short_names

  @doc false
  def custom_short_names, do: @custom_short_names

  @doc false
  def short_names, do: @short_names

  @doc false
  def builtin_types, do: @builtin_types
end

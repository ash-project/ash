# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Page.Keyset do
  @moduledoc """
  A page of results from `keyset` based pagination.

  The results are generated with a `keyset` metadata,
  which can be used to fetch the next/previous pages.
  """

  # Upper bound on the size (in bytes) of the term a client-supplied cursor is
  # allowed to deserialize into. Cursors legitimately encode only the sort
  # values of a single record, so this is generous; it exists purely to prevent
  # a hostile cursor (e.g. a compressed term that inflates to tens of MB) from
  # exhausting memory. Configurable via `config :ash, max_keyset_byte_size: ...`.
  @default_max_keyset_byte_size 10_240

  @derive {Inspect, only: [:results, :count, :before, :after, :more?]}
  defstruct [:results, :count, :before, :after, :limit, :rerun, :more?]

  @type t :: %__MODULE__{
          results: [Ash.Resource.Record.t()],
          count: non_neg_integer(),
          before: binary() | nil,
          after: binary() | nil,
          limit: pos_integer(),
          more?: boolean(),
          rerun: {Ash.Query.t(), Keyword.t()}
        }

  @type page_opts_type :: :non_neg_integer | :pos_integer | :any | :boolean
  @type page_opts_opts :: [type: page_opts_type(), doc: String.t()]
  @type page_opts :: [
          before: page_opts_opts(),
          after: page_opts_opts(),
          limit: page_opts_opts(),
          filter: page_opts_opts(),
          count: page_opts_opts()
        ]

  @page_opts [
    before: [
      type: :string,
      doc: "Get records that appear before the provided keyset (mutually exclusive with `after`)"
    ],
    after: [
      type: :string,
      doc: "Get records that appear after the provided keyset (mutually exclusive with `before`)"
    ],
    limit: [
      type: :pos_integer,
      doc: "How many records to include in the page"
    ],
    filter: [
      type: :any,
      doc: "See the `filter` option for offset pagination, this behaves the same."
    ],
    count: [
      type: :boolean,
      doc: "Whether or not to return the page with a full count of all records"
    ]
  ]

  page_opts = @page_opts

  defmodule Opts do
    @moduledoc false

    use Spark.Options.Validator, schema: page_opts
  end

  @doc false
  @spec page_opts() :: page_opts()
  def page_opts do
    @page_opts
  end

  @doc """
  Creates a new `Ash.Page.Keyset.t`.
  """
  @spec new(
          [Ash.Resource.Record.t()],
          non_neg_integer(),
          term(),
          Ash.Query.t(),
          boolean(),
          Keyword.t()
        ) :: t()
  def new(results, count, _sort, original_query, more?, opts) do
    %__MODULE__{
      results: results,
      count: count,
      before: original_query.page[:before],
      after: original_query.page[:after],
      limit: original_query.page[:limit],
      more?: more?,
      rerun: {original_query, opts}
    }
  end

  @doc """
  Appends keyset info to results.
  """
  @spec data_with_keyset([Ash.Resource.Record.t()], term(), term()) :: [Ash.Resource.Record.t()]
  def data_with_keyset(results, _resource, sort) when is_list(results) do
    Enum.map(results, fn result ->
      Map.update!(
        result,
        :__metadata__,
        &Map.put(&1, :keyset, keyset(result, sort))
      )
    end)
  end

  @doc """
  Creates filters on the query using the query for the Keyset.
  """
  @spec filter(Ash.Query.t(), [term()], term(), :after | :before) ::
          {:ok, Keyword.t()} | {:error, term()}
  def filter(%{resource: resource} = query, values, sort, after_or_before)
      when after_or_before in [:after, :before] do
    with {:ok, decoded} <- decode_values(values, after_or_before),
         {:ok, zipped} <- zip_fields(sort, decoded, values),
         {:ok, zipped} <- cast_values(zipped, resource, values) do
      {:ok, filters(Enum.with_index(zipped), resource, query, after_or_before)}
    else
      {:error, %Ash.Error.Page.InvalidKeyset{} = error} ->
        {:error, maybe_redact(error, resource, sort)}

      {:error, error} ->
        {:error, error}
    end
  end

  # a keyset is `term_to_binary` + Base64 over the sort values of the record it
  # was built from, so it exposes those values to anyone holding it
  defp maybe_redact(error, resource, sort) do
    if Application.get_env(:ash, :redact_sensitive_values_in_errors?, false) and
         sensitive_sort?(resource, sort) do
      %{error | value: Ash.Helpers.redact(error.value)}
    else
      error
    end
  end

  defp sensitive_sort?(resource, sort) do
    Enum.any?(sort, fn
      {%{sensitive?: sensitive?}, _} ->
        sensitive?

      {field, _} ->
        match?(%{sensitive?: true}, Ash.Resource.Info.field(resource, field))
    end)
  end

  defp decode_values(values, key) do
    max_byte_size =
      Application.get_env(:ash, :max_keyset_byte_size, @default_max_keyset_byte_size)

    with {:ok, decoded} <- Base.decode64(values),
         :ok <- check_keyset_size(decoded, max_byte_size),
         term <- non_executable_binary_to_term(decoded, [:safe]),
         :ok <- check_no_expression(term) do
      {:ok, term}
    else
      _ ->
        {:error, Ash.Error.Page.InvalidKeyset.exception(value: values, key: key)}
    end
  rescue
    _e ->
      {:error, Ash.Error.Page.InvalidKeyset.exception(value: values, key: key)}
  end

  # Ash only ever encodes cursors with uncompressed `:erlang.term_to_binary/1`,
  # so a compressed payload (external term format tag `80`) is never a legitimate
  # keyset. Rejecting it outright removes the decompression-bomb vector entirely,
  # without relying on the header's self-declared uncompressed size. Uncompressed
  # payloads are then bounded directly by their own byte size.
  defp check_keyset_size(<<131, 80, _::binary>>, _max), do: :error
  defp check_keyset_size(binary, max) when byte_size(binary) > max, do: :error
  defp check_keyset_size(_binary, _max), do: :ok

  # A legitimate cursor only ever contains scalar sort values. A decoded term
  # that is or contains an Ash expression (e.g. `%Ash.Query.Call{}`) is a forged
  # cursor attempting to inject a filter expression — which would be spliced into
  # the query as a value and evaluated (SQL injection / RCE depending on the data
  # layer). Reject any such term outright.
  defp check_no_expression(term) do
    if Ash.Expr.expr?(term) do
      :error
    else
      :ok
    end
  end

  defp filters(keyset, resource, query, after_or_before) do
    [or: do_filters(keyset, resource, query, after_or_before)]
  end

  defp do_filters([], _, _, _), do: []

  defp do_filters([{{field, direction, value}, index} | rest], resource, query, after_or_before) do
    {operator, nils_first?} = operator(after_or_before, direction)

    allow_nil? = allow_nil?(resource, field)

    # keyset pagination is generally done like so
    # (x > a) OR
    # (x = a AND y > b) OR
    # (x = a AND y = b AND z > c) OR

    field =
      case field do
        %{__struct__: field_struct} = calc
        when field_struct in [Ash.Query.Calculation, Ash.Query.Aggregate] ->
          calc

        field ->
          Ash.Resource.Info.field(resource, field)
      end

    field =
      if index in query.sort_input_indices do
        %Ash.Query.Ref{attribute: field, relationship_path: [], resource: resource, input?: true}
      else
        %Ash.Query.Ref{attribute: field, relationship_path: [], resource: resource}
      end

    operator_check =
      if is_nil(value) do
        if nils_first? do
          {field, [is_nil: false]}
        else
          {field, [is_nil: true]}
        end
      else
        if nils_first? do
          {field, [{operator, value}]}
        else
          if allow_nil? do
            [or: [{field, [{operator, value}]}, {field, [is_nil: true]}]]
          else
            {field, [{operator, value}]}
          end
        end
      end

    check = [[operator_check]]

    stacked_check =
      if is_nil(value) do
        [[{field, [{:is_nil, true}]}]]
      else
        if nils_first? do
          [[{field, [{:eq, value}]}]]
        else
          if allow_nil? do
            [[[or: [{field, [{:eq, value}]}, {field, [is_nil: true]}]]]]
          else
            [[{field, [{:eq, value}]}]]
          end
        end
      end

    if is_nil(value) and not nils_first? do
      Enum.map(do_filters(rest, resource, query, after_or_before), fn nested ->
        stacked_check ++ nested
      end)
    else
      check ++
        Enum.map(do_filters(rest, resource, query, after_or_before), fn nested ->
          stacked_check ++ nested
        end)
    end
  end

  defp allow_nil?(resource, field) when is_atom(field) do
    case Ash.Resource.Info.field(resource, field) do
      %Ash.Resource.Attribute{allow_nil?: allow_nil?} -> allow_nil?
      %Ash.Resource.Calculation{allow_nil?: allow_nil?} -> allow_nil?
      _ -> true
    end
  end

  defp allow_nil?(_, _), do: true

  defp operator(:after, :asc), do: {:gt, false}
  defp operator(:after, :asc_nils_first), do: {:gt, true}
  defp operator(:after, :asc_nils_last), do: {:gt, false}
  defp operator(:after, :desc), do: {:lt, true}
  defp operator(:after, :desc_nils_first), do: {:lt, true}
  defp operator(:after, :desc_nils_last), do: {:lt, false}
  defp operator(:before, :asc), do: {:lt, true}
  defp operator(:before, :asc_nils_first), do: {:lt, false}
  defp operator(:before, :asc_nils_last), do: {:lt, true}
  defp operator(:before, :desc), do: {:gt, false}
  defp operator(:before, :desc_nils_first), do: {:gt, false}
  defp operator(:before, :desc_nils_last), do: {:gt, true}

  defp zip_fields(pkey, values, full_value, acc \\ [])
  defp zip_fields([], [], _full_value, acc), do: {:ok, Enum.reverse(acc)}

  defp zip_fields([{pkey, direction} | rest_pkey], [value | rest_values], full_value, acc) do
    zip_fields(rest_pkey, rest_values, full_value, [{pkey, direction, value} | acc])
  end

  defp zip_fields(_, _, full_value, _),
    do: {:error, Ash.Error.Page.InvalidKeyset.exception(value: full_value)}

  # A keyset only ever encodes scalar sort values. On the way back in we cast each
  # decoded value against the type of the field it sorts on, so a forged cursor
  # whose value can't be a legitimate value of that field (e.g. an injected
  # `%Ash.Query.Call{}` in place of an integer) is rejected here rather than being
  # spliced into the filter and hydrated as an expression. The `expr?` guard in
  # `decode_values/2` covers permissive types (e.g. `:any`) whose `cast_input`
  # would otherwise pass an expression through unchanged.
  defp cast_values(zipped, resource, full_value) do
    zipped
    |> Enum.reduce_while({:ok, []}, fn {field, direction, value}, {:ok, acc} ->
      case cast_value(resource, field, value) do
        {:ok, value} ->
          {:cont, {:ok, [{field, direction, value} | acc]}}

        :error ->
          {:halt, {:error, Ash.Error.Page.InvalidKeyset.exception(value: full_value)}}
      end
    end)
    |> case do
      {:ok, casted} -> {:ok, Enum.reverse(casted)}
      {:error, error} -> {:error, error}
    end
  end

  # `nil` is inert and is a legitimate keyset value for a nullable sort field.
  defp cast_value(_resource, _field, nil), do: {:ok, nil}

  defp cast_value(resource, field, value) do
    case field_type(resource, field) do
      {type, constraints} ->
        case Ash.Type.cast_input(type, value, constraints) do
          {:ok, casted} -> {:ok, casted}
          _ -> :error
        end

      :error ->
        # Field type couldn't be determined; the `expr?` guard has already
        # rejected expressions, so pass the (scalar) value through unchanged.
        {:ok, value}
    end
  end

  defp field_type(resource, field) do
    field
    |> resolve_field(resource)
    |> case do
      # Attributes and calculations carry a resolved type directly. Aggregates
      # may too (query aggregates), so prefer it when present.
      %{type: type, constraints: constraints} when not is_nil(type) ->
        {type, constraints}

      # Resource aggregates generally leave `type`/`constraints` nil — their type
      # is derived from the aggregate kind and the field being aggregated, via the
      # same resolver used everywhere else.
      %struct{} = aggregate when struct in [Ash.Query.Aggregate, Ash.Resource.Aggregate] ->
        case Ash.Query.Aggregate.aggregate_type(resource, aggregate) do
          {:ok, type, constraints} -> {type, constraints}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp resolve_field(field, resource) when is_atom(field) or is_binary(field),
    do: Ash.Resource.Info.field(resource, field)

  defp resolve_field(field, _resource), do: field

  defp keyset(record, fields) do
    record
    |> field_values(fields)
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp field_values(record, sort) do
    sort
    |> Enum.with_index()
    |> Enum.map(fn
      {{%{__struct__: Ash.Query.Calculation, load: load, name: name}, _}, index} ->
        if load do
          Map.get(record, load)
        else
          # anonymous sort calculations are renamed to `{:__ash_runtime_sort__, index}`
          # when they are computed by `Ash.Actions.Sort.runtime_sort/3`
          case Map.fetch(record.calculations, name) do
            {:ok, value} -> value
            :error -> Map.get(record.calculations, {:__ash_runtime_sort__, index})
          end
        end

      {{%{__struct__: Ash.Query.Aggregate, load: load, name: name}, _}, index} ->
        if load do
          Map.get(record, load)
        else
          case Map.fetch(record.aggregates, name) do
            {:ok, value} -> value
            :error -> Map.get(record.aggregates, {:__ash_runtime_sort__, index})
          end
        end

      {{field, _}, _index} ->
        Map.get(record, field)
    end)
  end

  @doc """
  A restricted version of `:erlang.binary_to_term/2` that forbids
  *executable* terms, such as anonymous functions.
  The `opts` are given to the underlying `:erlang.binary_to_term/2`
  call, with an empty list as a default.
  By default this function does not restrict atoms, as an atom
  interned in one node may not yet have been interned on another
  (except for releases, which preload all code).
  If you want to avoid atoms from being created, then you can pass
  `[:safe]` as options, as that will also enable the safety mechanisms
  from `:erlang.binary_to_term/2` itself.
  Ripped from https://github.com/elixir-plug/plug_crypto/blob/v1.2.0/lib/plug/crypto.ex
  """
  defdelegate non_executable_binary_to_term(binary, opts), to: Ash.Helpers
end

defmodule Ash.Page.Keyset do
  @moduledoc """
  A page of results from `keyset` based pagination.

  The results are generated with a `keyset` metadata,
  which can be used to fetch the next/previous pages.
  """
  defstruct [:results, :count, :before, :after, :limit, :rerun, :more?]

  @type t :: %__MODULE__{}

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
  def page_opts do
    @page_opts
  end

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

  def data_with_keyset(results, _resource, sort) when is_list(results) do
    Enum.map(results, fn result ->
      Map.update!(
        result,
        :__metadata__,
        &Map.put(&1, :keyset, keyset(result, sort))
      )
    end)
  end

  def filter(%{resource: resource} = query, values, sort, after_or_before)
      when after_or_before in [:after, :before] do
    with {:ok, decoded} <- decode_values(values, after_or_before),
         {:ok, zipped} <- zip_fields(sort, decoded, values) do
      {:ok, filters(Enum.with_index(zipped), resource, query, after_or_before)}
    end
  end

  defp decode_values(values, key) do
    {:ok,
     values
     |> Base.decode64!()
     |> non_executable_binary_to_term([:safe])}
  rescue
    _e ->
      {:error, Ash.Error.Page.InvalidKeyset.exception(value: values, key: key)}
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

  defp keyset(record, fields) do
    record
    |> field_values(fields)
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp field_values(record, sort) do
    Enum.map(sort, fn
      {%{__struct__: Ash.Query.Calculation, load: load, name: name}, _} ->
        if load do
          Map.get(record, load)
        else
          Map.get(record.calculations, name)
        end

      {%{__struct__: Ash.Query.Aggregate, load: load, name: name}, _} ->
        if load do
          Map.get(record, load)
        else
          Map.get(record.aggregates, name)
        end

      {field, _} ->
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

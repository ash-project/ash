defmodule Ash.DataLayer.Mnesia do
  @moduledoc """
  An Mnesia backed Ash Datalayer.

  In your application intialization, you will need to call `Mnesia.create_schema([node()])`.

  Additionally, you will want to create your mnesia tables there.

  This data layer is *extremely unoptimized*, fetching all records from a table and filtering them
  in place. This is primarily used for testing the behavior of data layers in Ash. If it was improved,
  it could be a viable data layer.
  """
  @behaviour Ash.DataLayer

  @mnesia %Ash.Dsl.Section{
    name: :mnesia,
    describe: """
    A section for configuring the mnesia data layer
    """,
    examples: [
      """
      mnesia do
        table :custom_table
      end
      """
    ],
    schema: [
      table: [
        type: :atom,
        doc: "The table name to use, defaults to the name of the resource"
      ]
    ]
  }

  use Ash.Dsl.Extension, sections: [@mnesia]

  alias Ash.Actions.Sort
  alias Ash.Dsl.Extension
  alias :mnesia, as: Mnesia

  def start(api) do
    Mnesia.create_schema([node()])
    Mnesia.start()

    Code.ensure_compiled(api)

    api
    |> Ash.Api.resources()
    |> Enum.each(fn resource ->
      attributes =
        resource |> Ash.Resource.Info.attributes() |> Enum.map(& &1.name) |> Enum.sort()

      case Ash.Resource.Info.primary_key(resource) do
        [] ->
          resource
          |> table()
          |> Mnesia.create_table(attributes: attributes)

        _ ->
          resource
          |> table()
          |> Mnesia.create_table(attributes: [:_pkey | attributes])
      end
    end)
  end

  def table(resource) do
    Extension.get_opt(resource, [:ets], :private?, resource, true)
  end

  defmodule Query do
    @moduledoc false
    defstruct [:api, :resource, :filter, :limit, :sort, relationships: %{}, offset: 0]
  end

  @impl true
  def can?(_, :async_engine), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :upsert), do: true
  def can?(_, :create), do: true
  def can?(_, :read), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :filter), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :transact), do: true

  def can?(_, {:join, resource}) do
    # This is to ensure that these can't join, which is necessary for testing
    # if someone needs to use these both and *actually* needs real joins for private
    # ets resources then we can talk about making this only happen in ash tests
    not (Ash.DataLayer.data_layer(resource) == Ash.DataLayer.Ets &&
           Ash.DataLayer.Ets.private?(resource))
  end

  def can?(_, {:filter_expr, _}), do: true
  def can?(_, :nested_expressions), do: true
  def can?(_, {:sort, _}), do: true

  def can?(_, _), do: false

  @impl true
  def resource_to_query(resource, api) do
    %Query{
      resource: resource,
      api: api
    }
  end

  @impl true
  def in_transaction?(_), do: Mnesia.is_transaction()

  @impl true
  def limit(query, offset, _), do: {:ok, %{query | limit: offset}}

  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

  @impl true
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @impl true
  def run_query(
        %Query{
          api: api,
          resource: resource,
          filter: filter,
          offset: offset,
          limit: limit,
          sort: sort
        },
        _resource
      ) do
    records =
      Mnesia.transaction(fn ->
        Mnesia.select(table(resource), [{:_, [], [:"$_"]}])
      end)

    case records do
      {:aborted, reason} ->
        {:error, reason}

      {:atomic, records} ->
        attributes =
          resource |> Ash.Resource.Info.attributes() |> Enum.map(& &1.name) |> Enum.sort()

        elements_to_drop =
          case Ash.Resource.Info.primary_key(resource) do
            [] ->
              1

            _ ->
              2
          end

        structified_records =
          Enum.map(records, fn record ->
            struct(
              resource,
              Enum.zip(attributes, Enum.drop(Tuple.to_list(record), elements_to_drop))
            )
          end)

        api
        |> Ash.Filter.Runtime.filter_matches(structified_records, filter)
        |> case do
          {:ok, filtered} ->
            offset_records =
              filtered
              |> Sort.runtime_sort(sort)
              |> Enum.drop(offset || 0)

            limited_records =
              if limit do
                Enum.take(offset_records, limit)
              else
                offset_records
              end

            {:ok, limited_records}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  @impl true
  def create(resource, changeset) do
    {:ok, record} = Ash.Changeset.apply_attributes(changeset)

    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.map(fn attr ->
        Map.get(record, attr)
      end)

    values =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.sort_by(& &1.name)
      |> Enum.map(&Map.get(record, &1.name))

    values_with_primary_key =
      case pkey do
        [] ->
          List.to_tuple([table(resource) | values])

        pkey_values ->
          List.to_tuple([table(resource) | [pkey_values | values]])
      end

    result =
      Mnesia.transaction(fn ->
        Mnesia.write(values_with_primary_key)
      end)

    case result do
      {:atomic, _} -> {:ok, record}
      {:aborted, error} -> {:error, error}
    end
  end

  @impl true
  def destroy(resource, %{data: record}) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.map(&Map.get(record, &1))

    result =
      Mnesia.transaction(fn ->
        Mnesia.delete({table(resource), pkey})
      end)

    case result do
      {:atomic, _} -> :ok
      {:aborted, error} -> {:error, error}
    end
  end

  @impl true
  def update(resource, changeset) do
    create(resource, changeset)
  end

  @impl true
  def upsert(resource, changeset, _) do
    create(resource, changeset)
  end

  @impl true
  def transaction(_, func) do
    case Mnesia.transaction(func) do
      {:atomic, result} -> {:ok, result}
      {:aborted, reason} -> {:error, reason}
    end
  end

  @impl true
  @spec rollback(term, term) :: no_return
  def rollback(_, value) do
    Mnesia.abort(value)
  end
end

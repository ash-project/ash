defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS backed Ash Datalayer. Should only be used for testing, or for
  unimportant/small datasets.
  """

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @data_layer Ash.DataLayer.Ets
      @mix_ins Ash.DataLayer.Ets

      @ets_private? Keyword.get(opts, :private?, false)

      def ets_private?() do
        @ets_private?
      end
    end
  end

  def before_compile_hook(_env) do
    quote do
      struct_fields =
        @attributes
        |> Enum.map(fn attr ->
          {attr.name, nil}
        end)
        |> Enum.concat(Enum.map(@relationships, fn rel -> {rel.name, :not_loaded} end))

      defstruct struct_fields
    end
  end

  def private?(resource) do
    resource.ets_private?()
  end

  defmodule Query do
    defstruct [:resource, :limit, match_spec: {:_, :"$2"}, offset: 0]
  end

  def resource_to_query(resource), do: %Query{resource: resource}
  def limit(query, limit, _), do: {:ok, %Query{query | limit: limit}}
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}
  def can_query_async?(_), do: false

  def filter(query, :id, id, _resource) do
    {:ok, %{query | match_spec: {id, :"$2"}}}
  end

  def filter(_query, _field, _value, _resource) do
    {:error, "filter not supported for anything other than id"}
  end

  def run_query(
        %Query{resource: resource, match_spec: match_spec, offset: offset, limit: limit},
        _
      ) do
    with {:ok, table} <- wrap_or_create_table(resource),
         {:ok, {results, _}} <- match_limit(table, match_spec, limit, offset) do
      ret = results |> Enum.drop(offset) |> Enum.map(&List.first/1)

      {:ok, ret}
    end
  end

  def create(_resource, _attributes, relationships) when relationships != %{} do
    {:error, "#{inspect(__MODULE__)} does not support creating with relationships"}
  end

  def create(resource, attributes, _relationships) do
    with {:ok, table} <- wrap_or_create_table(resource),
         attrs <- Map.put_new_lazy(attributes, :id, &Ash.UUID.generate/0),
         record <- struct(resource, attrs),
         {:ok, _} <- Ets.Set.put(table, {attrs.id, record}) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp match_limit(table, match_spec, limit, offset) do
    # TODO: Fix this
    # This is a hack :(
    # Either implement cursor based pagination
    # or find a way to skip in ETS
    if limit do
      Ets.Set.match(table, match_spec, limit + offset)
    else
      Ets.Set.match(table, match_spec)
    end
  end

  defp wrap_or_create_table(resource) do
    case Ets.Set.wrap_existing(resource) do
      {:error, :table_not_found} ->
        protection =
          if private?(resource) do
            :private
          else
            :public
          end

        Ets.Set.new(name: resource, protection: protection, ordered: true, read_concurrency: true)

      {:ok, table} ->
        {:ok, table}

      {:error, other} ->
        {:error, other}
    end
  end
end

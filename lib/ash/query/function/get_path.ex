defmodule Ash.Query.Function.GetPath do
  @moduledoc """
  Gets the value at the provided path in the value, which must be a map or embed.

  If you are using a datalayer that provides a `type` function (like AshPostgres), it is a good idea to
  wrap your call in that function, e.g `type(author[:bio][:title], :string)`, since data layers that depend
  on knowing types may not be able to infer the type from the path. Ash may eventually be able to figure out
  the type, in the case that the path consists of only embedded attributes.

  If an atom key is provided, access is *indiscriminate* of atoms vs strings. The atom key is checked first.
  If a string key is provided, that is the only thing that is checked. If the value will or may be a struct, be sure to use atoms.

  The data layer may handle this differently, for example, AshPostgres only checks
  strings at the data layer (because thats all it can be in the database anyway).

  Available in query expressions using bracket syntax, e.g `foo[:bar][:baz]`.
  """
  use Ash.Query.Function, name: :get_path, no_inspect?: true

  def args,
    do: [
      [:map, {:array, :any}],
      [:map, :any]
    ]

  def returns, do: [:any, :any]

  def new([%__MODULE__{arguments: [inner_left, inner_right]} = get_path, right])
      when is_list(inner_right) and is_list(right) do
    {:ok, %{get_path | arguments: [inner_left, inner_right ++ right]}}
  end

  def new([_, right]) when not (is_list(right) or is_atom(right) or is_binary(right)) do
    {:error, "#{inspect(right)} is not a valid path to get"}
  end

  def new([left, right]) when not is_list(right) do
    new([left, [right]])
  end

  def new([left, right]) do
    super([left, right])
  end

  def evaluate(%{arguments: [%{} = obj, path]}) when is_list(path) do
    Enum.reduce_while(path, {:known, obj}, fn key, {:known, obj} ->
      if is_map(obj) do
        value =
          if is_atom(key) do
            Map.get(obj, key, Map.get(obj, to_string(key)))
          else
            case Enum.find(obj, fn {map_key, _val} ->
                   is_atom(map_key) && to_string(map_key) == key
                 end) do
              {_, val} ->
                val

              nil ->
                Map.get(obj, key)
            end
          end

        case value do
          nil ->
            {:halt, {:known, nil}}

          value ->
            {:cont, {:known, value}}
        end
      else
        {:halt, :unknown}
      end
    end)
  end

  def evaluate(_), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra
    import Ash.Query.InspectHelpers

    def inspect(%{arguments: [value, path]}, opts) do
      opts = put_container_type(opts, :get_path)

      path_items =
        path
        |> Enum.map(fn item ->
          concat(["[", to_doc(item, opts), "]"])
        end)
        |> concat()

      value
      |> to_doc(opts)
      |> concat(path_items)
    end
  end
end

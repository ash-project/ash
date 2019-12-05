defmodule Ash.Type do
  @moduledoc """
  This behaviour is a superset of the Ecto.Type behavior, that also contains
  API level information, like what kinds of filters are allowed. Eventually,
  this may be used for composite types or serialization.

  Much better to `use Ash.Type` than to say `@behaviour Ash.Type` and define
  everything yourself.
  """
  @callback supported_filter_types(Ash.data_layer()) :: list(Ash.DataLayer.Filter.filter_type())
  @callback storage_type() :: Ecto.Type.t()
  @callback ecto_type() :: Ecto.Type.t()
  @callback cast_input(term) :: {:ok, term} | {:error, keyword()} | :error
  @callback cast_stored(term) :: {:ok, term} | :error
  @callback dump_to_native(term) :: {:ok, term} | :error
  @callback equal?(term, term) :: boolean
  @callback describe() :: String.t()

  @builtins [
    string: [ecto_type: :string, filters: [:equal]],
    uuid: [ecto_type: :binary_id, filters: [:equal]],
    utc_datetime: [ecto_type: :utc_datetime, filters: [:equal]]
  ]

  @builtin_names Keyword.keys(@builtins)

  @type t :: module | atom

  @doc """
  Returns a list of filter types supported by this type. By default, a type supports only the `:equal` filter
  """
  @spec supported_filter_types(t, Ash.data_layer()) ::
          list(Ash.DataLayer.Filter.filter_type())
  def supported_filter_types(type, _data_layer) when type in @builtin_names do
    @builtins[type][:filters]
  end

  def supported_filter_types(type, data_layer), do: type.supported_filter_types(data_layer)

  @doc """
  Returns the *underlying* storage type (the underlying type of the *ecto type* of the *ash type*)
  """
  @spec storage_type(t()) :: Ecto.Type.t()
  def storage_type(type), do: type.storage_type()

  @doc """
  Returns the ecto compatible type for an Ash.Type.

  If you `use Ash.Type`, this is created for you. For builtin types
  this may return a corresponding ecto builtin type (atom)
  """
  @spec ecto_type(t) :: Ecto.Type.t()
  for {name, builtin} <- @builtins do
    def ecto_type(unquote(name)), do: unquote(builtin[:ecto_type])
  end

  def ecto_type(type) do
    type.ecto_type()
  end

  @doc """
  Casts input (e.g. unknown) data to an instance of the type, or errors

  Maps to `Ecto.Type.cast/2`
  """
  @spec cast_input(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def cast_input(type, term) when type in @builtin_names do
    Ecto.Type.cast(@builtins[term][:ecto_type], term)
  end

  def cast_input(type, term) do
    type.cast_input(term)
  end

  @doc """
  Casts a value from the data store to an instance of the type, or errors

  Maps to `Ecto.Type.load/2`
  """
  @spec cast_stored(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def cast_stored(type, term) when type in @builtin_names do
    Ecto.Type.load(@builtins[type][:ecto_type], term)
  end

  def cast_stored(type, term) do
    type.cast_stored(term)
  end

  @doc """
  Casts a value from the Elixir type to a value that the data store can persist

  Maps to `Ecto.Type.dump/2`
  """
  @spec dump_to_native(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def dump_to_native(type, term) when type in @builtin_names do
    Ecto.Type.dump(@builtins[type][:ecto_type], term)
  end

  def dump_to_native(type, term) do
    type.dump_to_native(term)
  end

  @doc """
  Determines if two values of a given type are equal.

  Maps to `Ecto.Type.equal?/3`
  """
  @spec equal?(t(), term, term) :: boolean
  def equal?(type, left, right) when type in @builtin_names do
    Ecto.Type.equal?(@builtins[type][:ecto_type], left, right)
  end

  def equal?(type, left, right) do
    type.equal?(left, right)
  end

  # @callback equal?(term, term) :: boolean

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Type

      parent = __MODULE__

      defmodule EctoType do
        @behaviour Ecto.Type

        @parent parent

        @impl true
        def type do
          @parent.storage_type()
        end

        @impl true
        def cast(term) do
          @parent.cast_input(term)
        end

        @impl true
        def load(term) do
          @parent.cast_stored(term)
        end

        @impl true
        def dump(term) do
          @parent.dump_to_native(term)
        end

        @impl true
        def equal?(left, right) do
          @parent.equal?(left, right)
        end

        @impl true
        def embed_as(_), do: :self
      end

      def ecto_type(), do: EctoType

      def supported_filter_types(_data_layer), do: [:equal]
      def equal?(left, right), do: left == right

      defoverridable supported_filter_types: 1, equal?: 2
    end
  end

  @doc "A list of the built in type names"
  def builtins(), do: @builtin_names

  @doc "Returns true if the value is a builtin type or adopts the `Ash.Type` behaviour"
  def ash_type?(atom) when atom in @builtin_names, do: true

  def ash_type?(module) do
    :erlang.function_exported(module, :module_info, 0) and ash_type_module?(module)
  end

  defp ash_type_module?(module) do
    :attributes
    |> module.module_info()
    |> Keyword.get(:behaviour, [])
    |> Enum.any?(&(&1 == __MODULE__))
  end
end

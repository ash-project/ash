defmodule Ash.Type do
  @moduledoc """
  This behaviour is a superset of the Ecto.Type behavior, that also contains
  api level information, like what kinds of filters are allowed. Eventually,
  this may be used for composite types or serialization.

  Much better to `use Ash.Type` than to say `@behaviour Ash.Type` and define
  everything yourself.
  """
  @type constraints :: Keyword.t()
  @callback storage_type() :: Ecto.Type.t()
  @callback ecto_type() :: Ecto.Type.t()
  @callback cast_input(term) :: {:ok, term} | {:error, keyword()} | :error
  @callback cast_stored(term) :: {:ok, term} | :error
  @callback dump_to_native(term) :: {:ok, term} | :error
  @callback constraints() :: constraints()
  @callback apply_constraints(term, constraints()) :: :ok | {:error, String.t() | [String.t()]}
  @callback equal?(term, term) :: boolean

  @short_names [
    atom: Ash.Type.Atom,
    term: Ash.Type.Term,
    string: Ash.Type.String,
    integer: Ash.Type.Integer,
    boolean: Ash.Type.Boolean,
    uuid: Ash.Type.UUID,
    date: Ash.Type.Date,
    utc_datetime: Ash.Type.UtcDatetime
  ]

  @type t :: module | atom

  @spec get_type(atom | module) :: atom | module
  def get_type(value) when is_atom(value) do
    case Keyword.fetch(@short_names, value) do
      {:ok, mod} -> mod
      :error -> value
    end
  end

  def get_type(value) do
    value
  end

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
  for {name, mod} <- @short_names do
    def ecto_type(unquote(name)), do: ecto_type(unquote(mod))
  end

  def ecto_type(type) do
    type.ecto_type()
  end

  @spec ash_type?(term) :: boolean
  @doc "Returns true if the value is a builtin type or adopts the `Ash.Type` behaviour"
  def ash_type?(module) when is_atom(module) do
    case Code.ensure_compiled(module) do
      {:module, _} -> ash_type_module?(module)
      _ -> false
    end
  end

  def ash_type?(_), do: false

  @doc """
  Casts input (e.g. unknown) data to an instance of the type, or errors

  Maps to `Ecto.Type.cast/2`
  """
  @spec cast_input(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def cast_input(type, term) do
    type.cast_input(term)
  end

  @doc """
  Casts a value from the data store to an instance of the type, or errors

  Maps to `Ecto.Type.load/2`
  """
  @spec cast_stored(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def cast_stored(type, term) do
    type.cast_stored(term)
  end

  @doc """
  Confirms if a casted value matches the provided constraints.
  """
  @spec apply_constraints(t(), term, constraints()) :: :ok | {:error, String.t()}
  def apply_constraints(type, term, constraints) do
    type.apply_constraints(term, constraints)
  end

  @spec constraints(t()) :: constraints()
  def constraints(type) do
    type.constraints()
  end

  @doc """
  Casts a value from the Elixir type to a value that the data store can persist

  Maps to `Ecto.Type.dump/2`
  """
  @spec dump_to_native(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def dump_to_native(type, term) do
    type.dump_to_native(term)
  end

  @doc """
  Determines if two values of a given type are equal.

  Maps to `Ecto.Type.equal?/3`
  """
  @spec equal?(t(), term, term) :: boolean
  def equal?(type, left, right) do
    type.equal?(left, right)
  end

  # @callback equal?(term, term) :: boolean

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Ash.Type

      parent = __MODULE__

      defmodule EctoType do
        @moduledoc false
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

      @impl true
      def ecto_type, do: EctoType

      @impl true
      def equal?(left, right), do: left == right

      @impl true
      def constraints, do: []

      @impl true
      def apply_constraints(_, _), do: :ok

      defoverridable equal?: 2, constraints: 0, apply_constraints: 2
    end
  end

  defp ash_type_module?(module) do
    Ash.implements_behaviour?(module, __MODULE__)
  end
end

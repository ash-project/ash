defmodule Ash.Type do
  @list_constraints [
    min_length: [
      type: :non_neg_integer,
      doc: "A minimum length for the items"
    ],
    max_length: [
      type: :non_neg_integer,
      doc: "A maximum length for the items"
    ],
    nil_items?: [
      type: :boolean,
      doc: "Whether or not the list can contain nil items",
      default: true
    ]
  ]

  @short_names [
    map: Ash.Type.Map,
    term: Ash.Type.Term,
    atom: Ash.Type.Atom,
    string: Ash.Type.String,
    integer: Ash.Type.Integer,
    boolean: Ash.Type.Boolean,
    uuid: Ash.Type.UUID,
    date: Ash.Type.Date,
    utc_datetime: Ash.Type.UtcDatetime
  ]

  @builtin_types Keyword.values(@short_names)

  def builtin?(type) when type in @builtin_types, do: true
  def builtin?(_), do: false

  @doc_list_constraints Keyword.put(@list_constraints, :items,
                          type: :any,
                          doc:
                            "Constraints for the elements of the list. See the contained type's docs for more."
                        )
  @moduledoc """
  This behaviour is a superset of the Ecto.Type behavior, that also contains
  api level information, like what kinds of filters are allowed. Eventually,
  this may be used for composite types or serialization.

  Much better to `use Ash.Type` than to say `@behaviour Ash.Type` and define
  everything yourself.

  ## Built in types

  #{
    Enum.map_join(@short_names, fn {key, module} ->
      "* `#{inspect(key)}` - `#{inspect(module)}`\n"
    end)
  }

  ### Composite Types

  Currently, the only composite type supported is a list type, specified via:
  `{:array, Type}`. The constraints available are:

  #{Ash.OptionsHelpers.docs(@doc_list_constraints)}
  """
  @type constraints :: Keyword.t()
  @callback storage_type() :: Ecto.Type.t()
  @callback ecto_type() :: Ecto.Type.t()
  @callback cast_input(term) :: {:ok, term} | {:error, Keyword.t()} | :error
  @callback cast_stored(term) :: {:ok, term} | :error
  @callback dump_to_native(term) :: {:ok, term} | :error
  @callback constraints() :: constraints()
  @callback apply_constraints(term, constraints()) ::
              :ok | {:error, constraint_error() | list(constraint_error)}
  @callback describe(constraints()) :: String.t() | nil
  @callback equal?(term, term) :: boolean

  @type constraint_error :: String.t() | {String.t(), Keyword.t()}
  @type t :: atom | {:array, atom}

  def describe(type, constraints) do
    case get_type(type) do
      {:array, type} ->
        type.describe(constraints)

      type ->
        type.describe(constraints)
    end
  end

  @doc false
  def list_constraints, do: @list_constraints

  @spec get_type(atom | module) :: atom | module | {:array, atom | module}
  def get_type({:array, value}) do
    {:array, get_type(value)}
  end

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
  def storage_type({:array, type}), do: {:array, type.storage_type()}
  def storage_type(type), do: type.storage_type()

  @doc """
  Returns the ecto compatible type for an Ash.Type.

  If you `use Ash.Type`, this is created for you. For builtin types
  this may return a corresponding ecto builtin type (atom)
  """
  @spec ecto_type(t) :: Ecto.Type.t()
  def ecto_type({:array, type}), do: {:array, ecto_type(type)}

  for {name, mod} <- @short_names do
    def ecto_type(unquote(name)), do: ecto_type(unquote(mod))
  end

  def ecto_type(type) do
    type.ecto_type()
  end

  @spec ash_type?(term) :: boolean
  @doc "Returns true if the value is a builtin type or adopts the `Ash.Type` behaviour"
  def ash_type?({:array, value}), do: ash_type?(value)

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
  @spec cast_input(t(), term) :: {:ok, term} | {:error, Keyword.t()} | :error
  def cast_input({:array, _type}, term) when not is_list(term) do
    {:error, message: "must be a list"}
  end

  def cast_input({:array, type}, term) do
    term
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
      case cast_input(type, item) do
        :error ->
          {:halt, {:error, index: index}}

        {:error, keyword} ->
          {:halt, {:error, Keyword.put(keyword, :index, index)}}

        {:ok, value} ->
          {:cont, {:ok, [value | casted]}}
      end
    end)
  end

  def cast_input(type, term) do
    case type.cast_input(term) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        {:error, "is invalid"}

      {:error, other} ->
        {:error, other}
    end
  end

  @doc """
  Casts a value from the data store to an instance of the type, or errors

  Maps to `Ecto.Type.load/2`
  """
  @spec cast_stored(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def cast_stored({:array, type}, term) when is_list(term) do
    term
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
      case cast_stored(type, item) do
        :error ->
          {:halt, {:error, index: index}}

        {:error, keyword} ->
          {:halt, {:error, Keyword.put(keyword, :index, index)}}

        {:ok, value} ->
          {:cont, {:ok, [value | casted]}}
      end
    end)
  end

  def cast_stored(type, term) do
    type.cast_stored(term)
  end

  @doc """
  Confirms if a casted value matches the provided constraints.
  """
  @spec apply_constraints(t(), term, constraints()) :: :ok | {:error, String.t()}
  def apply_constraints({:array, type}, term, constraints) when is_list(term) do
    list_constraint_errors = list_constraint_errors(term, constraints)

    case list_constraint_errors do
      [] ->
        nil_items? = Keyword.get(constraints, :nil_items?, true)
        item_constraints = constraints[:items] || []

        if item_constraints != [] || !nil_items? do
          errors =
            term
            |> Enum.with_index()
            |> Enum.reduce([], fn {item, index}, errors ->
              errors =
                if is_nil(item) && not nil_items? do
                  [{"no nil/null values at index %{index}", index: index} | errors]
                else
                  errors
                end

              case apply_constraints(type, item, item_constraints) do
                :ok ->
                  errors

                {:error, new_errors} ->
                  new_errors =
                    new_errors
                    |> List.wrap()
                    |> Enum.map(fn
                      {template, replacements} ->
                        {template <> " at index %{index}",
                         Keyword.put(replacements, :index, index)}

                      string ->
                        {string <> " at index %{index}", index: index}
                    end)

                  List.wrap(new_errors) ++ errors
              end
            end)

          if errors == [] do
            :ok
          else
            {:error, errors}
          end
        else
          :ok
        end

      errors ->
        {:error, errors}
    end
  end

  def apply_constraints({:array, _}, _, _) do
    {:error, ["must be a list"]}
  end

  def apply_constraints(type, term, constraints) do
    type.apply_constraints(term, constraints)
  end

  defp list_constraint_errors(term, constraints) do
    length =
      if Keyword.has_key?(constraints, :max_length) || Keyword.has_key?(constraints, :min_length) do
        length(term)
      else
        0
      end

    constraints
    |> Enum.reduce([], fn
      {:min_length, min_length}, errors ->
        if length < min_length do
          [{"must have more than %{min} items", min: min_length} | errors]
        else
          errors
        end

      {:max_length, max_length}, errors ->
        if length > max_length do
          [{"must have fewer than %{max} items", max: max_length} | errors]
        else
          errors
        end

      _, errors ->
        errors
    end)
  end

  @spec constraints(t()) :: constraints()
  def constraints({:array, _type}) do
    @list_constraints
  end

  def constraints(type) do
    type.constraints()
  end

  @doc """
  Casts a value from the Elixir type to a value that the data store can persist

  Maps to `Ecto.Type.dump/2`
  """
  @spec dump_to_native(t(), term) :: {:ok, term} | {:error, keyword()} | :error
  def dump_to_native({:array, type}, term) do
    term
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, []}, fn item, {:ok, dumped} ->
      case dump_to_native(type, item) do
        :error ->
          {:halt, :error}

        {:ok, value} ->
          {:cont, {:ok, [value | dumped]}}
      end
    end)
  end

  def dump_to_native(type, term) do
    type.dump_to_native(term)
  end

  @doc """
  Determines if two values of a given type are equal.

  Maps to `Ecto.Type.equal?/3`
  """
  @spec equal?(t(), term, term) :: boolean
  def equal?({:array, type}, [nil | xs], [nil | ys]), do: equal?({:array, type}, xs, ys)

  def equal?({:array, type}, [x | xs], [y | ys]),
    do: equal?(type, x, y) && equal?({:array, type}, xs, ys)

  def equal?({:array, _}, [], []), do: true
  def equal?({:array, _}, _, _), do: false

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
          storage_type = @parent.storage_type()

          if Ash.Type.ash_type?(storage_type) do
            Ash.Type.storage_type(@parent.storage_type())
          else
            storage_type
          end
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
      def describe([]), do: String.trim_leading(inspect(__MODULE__), "Ash.Type.")

      def describe(constraints) do
        "#{String.trim_leading(inspect(__MODULE__), "Ash.Type.")} | #{inspect(constraints)}"
      end

      @impl true
      def apply_constraints(_, _), do: :ok

      defoverridable equal?: 2, constraints: 0, apply_constraints: 2
    end
  end

  defp ash_type_module?(module) do
    Ash.implements_behaviour?(module, __MODULE__)
  end
end

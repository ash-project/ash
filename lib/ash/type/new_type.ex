# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.NewType do
  @moduledoc """
  Allows defining a new type that is the combination of an existing type and custom constraints

  A subtle difference between this type and its supertype (one that will almost certainly not matter
  in any case) is that we use the `apply_constraints` logic of the underlying type in the same step
  as `cast_input`. We do this because new types like these are, generally speaking, considering the constraint
  application as part of the core type. Other types, if you simply do `Ash.Type.cast_input/3` you will not be
  also applying their constraints.

  ## Options

  - `:subtype_of` - The type that this new type is a subtype of.
  - `:constraints` - The constraints that this new type uses for the underlying type.
  - `:lazy_init?` - If true, the `init/1` function will be called at runtime instead of compile time.
    Allows for recursive types.

  For Example:

  ```elixir
  defmodule MyApp.Types.SSN do
    use Ash.Type.NewType, subtype_of: :string, constraints: [match: ~r/regex for ssn/]
  end

  defmodule MyApp.Types.Metadata do
    use Ash.Type.NewType, subtype_of: :union, constraints: [types: [
      foo: [...],
      bar: [...]
    ]]
  end
  ```
  """

  @doc "Returns the type that the NewType is a subtype of."
  @callback subtype_of() :: module | atom
  @doc "Whether or not the type is lazy initialized (so needs to be initialized when fetching constraints)"
  @callback lazy_init?() :: boolean()
  @doc "Returns the underlying subtype constraints"
  @callback subtype_constraints() :: Keyword.t()
  @doc "Returns the modified NewType constraints"
  @callback type_constraints(constraints :: Keyword.t(), subtype_constraints :: Keyword.t()) ::
              Keyword.t()

  @type t :: module | atom | {:array, module | atom}

  @doc "Returns true if the corresponding type is an Ash.Type.NewType"
  @spec new_type?(Ash.Type.t()) :: boolean
  def new_type?({:array, type}), do: new_type?(type)
  def new_type?(type), do: Spark.implements_behaviour?(type, __MODULE__)

  @doc "Returns the type that the given newtype is a subtype of"
  @spec subtype_of(t()) :: Ash.Type.t()
  def subtype_of({:array, type}), do: {:array, subtype_of(type)}

  def subtype_of(type) do
    if new_type?(type) do
      subtype_of = type.subtype_of()

      if new_type?(subtype_of) do
        subtype_of(subtype_of)
      else
        subtype_of
      end
    else
      type
    end
  end

  @doc "Returns the constraints schema."
  @spec constraints(Ash.Type.t(), Keyword.t()) :: Keyword.t()
  def constraints({:array, type}, constraints) do
    if new_type?(type) do
      if type.lazy_init?() do
        item_constraints = constraints(type, constraints)

        Keyword.update(
          constraints,
          :items,
          item_constraints,
          &Keyword.merge(&1, item_constraints)
        )
      else
        constraints
      end
    else
      constraints
    end
  end

  def constraints(type, constraints) do
    if new_type?(type) do
      if type.lazy_init?() do
        case type.do_init(constraints) do
          {:ok, constraints} ->
            constraints

          {:error, error} ->
            raise Ash.Error.to_ash_error(error)
        end
      else
        constraints
      end
    else
      constraints
    end
  end

  defmacro __using__(opts) do
    case Keyword.keys(opts) -- [:subtype_of, :constraints, :lazy_init?] do
      [] ->
        []

      keys ->
        raise ArgumentError, "Unknown options given to `use Ash.Type.NewType`: #{inspect(keys)}"
    end

    subtype_of =
      Ash.Type.get_type(opts[:subtype_of])

    quote bind_quoted: [
            subtype_of: subtype_of,
            subtype_constraints: Macro.escape(opts[:constraints] || []),
            lazy_init?: Keyword.get(opts, :lazy_init?, false),
            mod: __MODULE__
          ],
          generated: true do
      if !lazy_init? do
        Code.ensure_compiled!(subtype_of)
      end

      @compile {:inline, get_constraints: 1}

      if is_nil(subtype_of) do
        raise "Must supply `:subtype_of` option when using #{mod}"
      end

      use Ash.Type

      @behaviour Ash.Type.NewType

      @impl Ash.Type.NewType
      def subtype_constraints, do: unquote(subtype_constraints)

      @impl Ash.Type.NewType
      def subtype_of do
        unquote(subtype_of)
      end

      @impl Ash.Type
      def load(values, load, constraints, context) do
        constraints = get_constraints(constraints)

        apply(unquote(subtype_of), :load, [
          values,
          load,
          constraints,
          context
        ])
      end

      @impl Ash.Type
      def can_load?(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).can_load?(constraints)
      end

      @impl Ash.Type
      def merge_load(left, right, constraints, context) do
        constraints = get_constraints(constraints)
        Ash.Type.merge_load(unquote(subtype_of), left, right, constraints, context)
      end

      @impl Ash.Type
      def get_rewrites(merged_load, calculation, path, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.get_rewrites(unquote(subtype_of), merged_load, calculation, path, constraints)
      end

      @impl Ash.Type
      def rewrite(value, rewrites, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.rewrite(unquote(subtype_of), value, rewrites, constraints)
      end

      @impl Ash.Type
      def matches_type?(type, constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).matches_type?(type, constraints)
      end

      @impl Ash.Type
      def cast_input(value, constraints) do
        constraints = get_constraints(constraints)

        with {:ok, value} <- unquote(subtype_of).cast_input(value, constraints) do
          Ash.Type.apply_constraints(unquote(subtype_of), value, constraints)
        end
      end

      @impl Ash.Type
      def coerce(value, constraints) do
        constraints = get_constraints(constraints)

        unquote(subtype_of).coerce(value, constraints)
      end

      @impl Ash.Type
      def cast_input_array(value, constraints) do
        constraints = get_constraints(constraints)

        with {:ok, value} <- unquote(subtype_of).cast_input_array(value, constraints) do
          Ash.Type.apply_constraints({:array, unquote(subtype_of)}, value, items: constraints)
        end
      end

      @impl Ash.Type.NewType
      def lazy_init?, do: unquote(lazy_init?)

      @impl Ash.Type
      if lazy_init? do
        def init(constraints) do
          {:ok, constraints}
        end
      else
        def init(constraints) do
          do_init(constraints)
        end
      end

      @doc false
      def do_init(constraints) do
        case validate_constraints(unquote(subtype_of), constraints) do
          :ok ->
            type_constraints =
              type_constraints(constraints, unquote(subtype_constraints))

            Ash.Type.init(unquote(subtype_of), type_constraints)

          {:error, error} ->
            {:error, error}
        end
      end

      @impl Ash.Type
      def cast_stored(value, constraints) do
        constraints = get_constraints(constraints)

        unquote(subtype_of).cast_stored(
          value,
          constraints
        )
      end

      @impl Ash.Type
      def cast_stored_array(value, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.cast_stored({:array, unquote(subtype_of)}, value, items: constraints)
      end

      @impl Ash.Type
      def include_source(constraints, source) do
        constraints = get_constraints(constraints)
        Ash.Type.include_source(unquote(subtype_of), source, constraints)
      end

      @impl Ash.Type
      def dump_to_embedded(value, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.dump_to_embedded(unquote(subtype_of), value, constraints)
      end

      @impl Ash.Type
      def composite?(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).composite?(constraints)
      end

      @impl Ash.Type
      def composite_types(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).composite_types(constraints)
      end

      @impl Ash.Type
      def dump_to_embedded_array(value, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.dump_to_embedded({:array, unquote(subtype_of)}, value, items: constraints)
      end

      @impl Ash.Type
      def dump_to_native(value, constraints) do
        constraints = get_constraints(constraints)

        unquote(subtype_of).dump_to_native(
          value,
          constraints
        )
      end

      @impl Ash.Type
      def dump_to_native_array(value, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.dump_to_native({:array, unquote(subtype_of)}, value, items: constraints)
      end

      @impl Ash.Type
      def equal?(left, right) do
        unquote(subtype_of).equal?(
          left,
          right
        )
      end

      @impl Ash.Type
      def generator(constraints) do
        Ash.Type.generator(unquote(subtype_of), constraints)
      end

      @impl Ash.Type
      def handle_change(old_term, new_term, constraints) do
        constraints = get_constraints(constraints)

        Ash.Type.handle_change(unquote(subtype_of), old_term, new_term, constraints)
      end

      @impl Ash.Type
      def handle_change_array(old_term, new_term, constraints) do
        constraints = get_constraints(constraints)

        Ash.Type.handle_change(
          {:array, unquote(subtype_of)},
          old_term,
          new_term,
          items: constraints
        )
      end

      @impl Ash.Type
      def prepare_change(old_term, new_term, constraints) do
        constraints = get_constraints(constraints)

        Ash.Type.prepare_change(
          unquote(subtype_of),
          old_term,
          new_term,
          constraints
        )
      end

      @impl Ash.Type
      def prepare_change_array(old_term, new_term, constraints) do
        constraints = get_constraints(constraints)

        Ash.Type.prepare_change(
          {:array, unquote(subtype_of)},
          old_term,
          new_term,
          items: constraints
        )
      end

      @impl Ash.Type
      def simple_equality? do
        unquote(subtype_of).simple_equality?()
      end

      @impl Ash.Type
      def storage_type(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).storage_type(constraints)
      end

      @impl Ash.Type
      def constraints do
        unquote(subtype_of).constraints()
      end

      @impl Ash.Type
      def embedded? do
        unquote(subtype_of).embedded?()
      end

      @impl Ash.Type
      def cast_atomic(value, constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).cast_atomic(value, constraints)
      end

      @impl Ash.Type
      def may_support_atomic_update?(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).may_support_atomic_update?(constraints)
      end

      @impl Ash.Type
      def cast_atomic_array(value, constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).cast_atomic_array(value, constraints)
      end

      @impl Ash.Type
      def apply_atomic_constraints(value, constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).apply_atomic_constraints(value, constraints)
      end

      @impl Ash.Type
      def apply_atomic_constraints_array(value, constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).apply_atomic_constraints_array(value, constraints)
      end

      @impl Ash.Type
      def apply_constraints(value, _constraints) do
        {:ok, value}
      end

      @impl Ash.Type
      def apply_constraints_array(value, constraints) do
        constraints = get_constraints(constraints)
        Ash.Type.apply_constraints({:array, unquote(subtype_of)}, value, items: constraints)
      end

      @impl Ash.Type
      def cast_in_query?(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).cast_in_query?(constraints)
      end

      @impl Ash.Type
      def describe(constraints) do
        constraints = get_constraints(constraints)
        unquote(subtype_of).describe(constraints)
      end

      @impl Ash.Type.NewType
      def type_constraints(constraints, subtype_constraints) do
        constraints = get_constraints(constraints)
        Keyword.merge(constraints || [], subtype_constraints || [])
      end

      defp get_constraints(constraints) do
        constraints
      end

      defp validate_constraints(type, constraints) do
        constraint_keys = constraints |> List.wrap() |> Keyword.keys()
        valid_constraint_keys = type |> Ash.Type.constraints() |> Keyword.keys()

        case constraint_keys -- valid_constraint_keys do
          [] ->
            case constraints[:fields] do
              nil ->
                :ok

              fields ->
                fields
                |> Enum.reduce_while(:ok, fn
                  {key, field}, :ok ->
                    field_keys = field |> List.wrap() |> Keyword.keys()

                    case field_keys -- [:type, :constraints, :allow_nil?, :description] do
                      [] ->
                        {:cont, validate_constraints(field[:type], field[:constraints])}

                      keys ->
                        {:halt, {:error, "Unknown options given to #{key}: #{inspect(keys)}"}}
                    end

                  _, acc ->
                    {:halt, acc}
                end)
            end

          keys ->
            {:error, "Unknown options given to `#{type}`: #{inspect(keys)}"}
        end
      end

      defoverridable storage_type: 1,
                     cast_input: 2,
                     prepare_change: 3,
                     dump_to_embedded: 2,
                     handle_change: 3,
                     dump_to_embedded_array: 2,
                     include_source: 2,
                     apply_constraints_array: 2,
                     handle_change_array: 3,
                     cast_stored_array: 2,
                     prepare_change_array: 3,
                     cast_stored: 2,
                     generator: 1,
                     dump_to_native: 2,
                     dump_to_native_array: 2,
                     type_constraints: 2
    end
  end
end

defmodule Ash.Type.NewType do
  @moduledoc """
  Allows defining a new type that is the combination of an existing type and custom constraints

  A subtle difference between this type and its supertype (one that will almost certainly not matter
  in any case) is that we use the `apply_constraints` logic of the underlying type in the same step
  as `cast_input`. We do this because new types like these are, generally speaking, considering the constraint
  application as part of the core type. Other types, if you simply do `Ash.Type.cast_input/3` you will not be
  also applying their constraints.

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
    item_constraints = constraints(type, constraints)
    Keyword.update(constraints, :items, item_constraints, &Keyword.merge(&1, item_constraints))
  end

  def constraints(type, constraints) do
    if new_type?(type) do
      constraints = Keyword.merge(constraints, type.subtype_constraints())

      if new_type?(type.subtype_of()) do
        constraints(type.subtype_of(), constraints)
      else
        constraints
      end
    else
      constraints
    end
  end

  defmacro __using__(opts) do
    case Keyword.keys(opts) -- [:subtype_of, :constraints] do
      [] ->
        []

      keys ->
        raise ArgumentError, "Unknown options given to `use Ash.Type.NewType`: #{inspect(keys)}"
    end

    quote bind_quoted: [
            subtype_of: Ash.Type.get_type(opts[:subtype_of]),
            subtype_constraints: Macro.escape(opts[:constraints] || []),
            mod: __MODULE__
          ],
          generated: true do
      Code.ensure_compiled!(subtype_of)

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
        apply(unquote(subtype_of), :load, [
          values,
          load,
          constraints,
          context
        ])
      end

      @impl Ash.Type
      def can_load?(constraints) do
        unquote(subtype_of).can_load?(constraints)
      end

      if function_exported?(subtype_of, :merge_load, 4) do
        @impl Ash.Type
        def merge_load(left, right, constraints, context) do
          unquote(subtype_of).merge_load(left, right, constraints, context)
        end
      end

      if function_exported?(subtype_of, :get_rewrites, 4) do
        @impl Ash.Type
        def get_rewrites(merged_load, calculation, path, constraints) do
          unquote(subtype_of).get_rewrites(merged_load, calculation, path, constraints)
        end
      end

      if function_exported?(subtype_of, :rewrite, 3) do
        @impl Ash.Type
        def rewrite(value, rewrites, constraints) do
          unquote(subtype_of).rewrite(value, rewrites, constraints)
        end
      end

      @impl Ash.Type
      def matches_type?(type, constraints) do
        unquote(subtype_of).matches_type?(type, constraints)
      end

      @impl Ash.Type
      def cast_input(value, constraints) do
        with {:ok, value} <- unquote(subtype_of).cast_input(value, constraints) do
          Ash.Type.apply_constraints(unquote(subtype_of), value, constraints)
        end
      end

      @impl Ash.Type
      def cast_input_array(value, constraints) do
        with {:ok, value} <- unquote(subtype_of).cast_input_array(value, constraints) do
          Ash.Type.apply_constraints({:array, unquote(subtype_of)}, value, items: constraints)
        end
      end

      @impl Ash.Type
      def init(constraints) do
        unquote(subtype_of).init(type_constraints(constraints, unquote(subtype_constraints)))
      end

      @impl Ash.Type
      def cast_stored(value, constraints) do
        unquote(subtype_of).cast_stored(
          value,
          constraints
        )
      end

      if function_exported?(subtype_of, :cast_stored_array, 2) do
        @impl Ash.Type
        def cast_stored_array(value, constraints) do
          unquote(subtype_of).cast_stored_array(
            value,
            constraints
          )
        end

        defoverridable cast_stored_array: 2
      end

      if function_exported?(subtype_of, :include_source, 2) do
        @impl Ash.Type
        def include_source(constraints, source) do
          unquote(subtype_of).include_source(
            constraints,
            source
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_embedded, 2) do
        @impl Ash.Type
        def dump_to_embedded(value, constraints) do
          unquote(subtype_of).dump_to_embedded(
            value,
            constraints
          )
        end
      end

      @impl Ash.Type
      def composite?(constraints) do
        unquote(subtype_of).composite?(constraints)
      end

      @impl Ash.Type
      def composite_types(constraints) do
        unquote(subtype_of).composite_types(constraints)
      end

      if function_exported?(subtype_of, :dump_to_embedded_array, 2) do
        @impl Ash.Type
        def dump_to_embedded_array(value, constraints) do
          unquote(subtype_of).dump_to_embedded_array(
            value,
            constraints
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_native, 2) do
        @impl Ash.Type
        def dump_to_native(value, constraints) do
          unquote(subtype_of).dump_to_native(
            value,
            constraints
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_native_array, 2) do
        @impl Ash.Type
        def dump_to_native_array(value, constraints) do
          unquote(subtype_of).dump_to_native_array(
            value,
            constraints
          )
        end

        defoverridable dump_to_native_array: 2
      end

      @impl Ash.Type
      def equal?(left, right) do
        unquote(subtype_of).equal?(
          left,
          right
        )
      end

      if function_exported?(subtype_of, :generator, 1) do
        @impl Ash.Type
        def generator(constraints) do
          unquote(subtype_of).generator(constraints)
        end
      end

      if function_exported?(subtype_of, :handle_change, 3) do
        @impl Ash.Type
        def handle_change(old_term, new_term, constraints) do
          unquote(subtype_of).handle_change(
            old_term,
            new_term,
            constraints
          )
        end
      end

      if function_exported?(subtype_of, :handle_change_array, 3) do
        @impl Ash.Type
        def handle_change_array(old_term, new_term, constraints) do
          unquote(subtype_of).handle_change_array(
            old_term,
            new_term,
            constraints
          )
        end
      end

      if function_exported?(subtype_of, :prepare_change, 3) do
        @impl Ash.Type
        def prepare_change(old_term, new_term, constraints) do
          unquote(subtype_of).prepare_change(
            old_term,
            new_term,
            constraints
          )
        end
      end

      if function_exported?(subtype_of, :prepare_change_array, 3) do
        @impl Ash.Type
        def prepare_change_array(old_term, new_term, constraints) do
          unquote(subtype_of).prepare_change_array(
            old_term,
            new_term,
            constraints
          )
        end
      end

      @impl Ash.Type
      def simple_equality? do
        unquote(subtype_of).simple_equality?()
      end

      @impl Ash.Type
      def storage_type(constraints) do
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
        unquote(subtype_of).cast_atomic(value, constraints)
      end

      @impl Ash.Type
      def cast_atomic_array(value, constraints) do
        unquote(subtype_of).cast_atomic_array(value, constraints)
      end

      @impl Ash.Type
      def apply_atomic_constraints(value, constraints) do
        unquote(subtype_of).apply_atomic_constraints(value, constraints)
      end

      @impl Ash.Type
      def apply_atomic_constraints_array(value, constraints) do
        unquote(subtype_of).apply_atomic_constraints_array(value, constraints)
      end

      @impl Ash.Type
      def apply_constraints(value, _constraints) do
        {:ok, value}
      end

      if function_exported?(subtype_of, :apply_constraints_array, 3) do
        @impl Ash.Type
        def apply_constraints_array(value, constraints) do
          unquote(subtype_of).apply_constraints_array(value, constraints)
        end
      end

      @impl Ash.Type
      def cast_in_query?(constraints) do
        unquote(subtype_of).cast_in_query?(constraints)
      end

      @impl Ash.Type
      def describe(constraints) do
        unquote(subtype_of).describe(constraints)
      end

      @impl Ash.Type.NewType
      def type_constraints(constraints, subtype_constraints) do
        Keyword.merge(constraints || [], subtype_constraints || [])
      end

      defoverridable storage_type: 1,
                     cast_input: 2,
                     cast_stored: 2,
                     dump_to_native: 2,
                     type_constraints: 2
    end
  end
end

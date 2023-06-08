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

  @callback subtype_of() :: module | atom
  @callback subtype_constraints() :: Keyword.t()
  @callback type_constraints(constraints :: Keyword.t(), subtype_constraints :: Keyword.t()) ::
              Keyword.t()

  @type t :: module | atom | {:array, module | atom}

  @spec new_type?(Ash.Type.t()) :: boolean
  def new_type?({:array, type}), do: new_type?(type)
  def new_type?(type), do: Spark.implements_behaviour?(type, __MODULE__)

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
    quote bind_quoted: [
            subtype_of: Ash.Type.get_type(opts[:subtype_of]),
            subtype_constraints: Macro.escape(opts[:constraints] || []),
            mod: __MODULE__
          ] do
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

      if Ash.Type.can_load?(subtype_of) do
        @impl Ash.Type
        def load(values, load, constraints, context) do
          unquote(subtype_of).load(
            values,
            load,
            type_constraints(constraints, unquote(subtype_constraints)),
            context
          )
        end
      end

      @impl Ash.Type
      def cast_input(value, constraints) do
        case unquote(subtype_of).cast_input(
               value,
               type_constraints(constraints, unquote(subtype_constraints))
             ) do
          {:ok, value} ->
            apply_constraints(value, constraints)

          other ->
            other
        end
      end

      if function_exported?(subtype_of, :cast_input_array, 2) do
        @impl Ash.Type
        def cast_input_array(value, constraints) do
          unquote(subtype_of).cast_input_array(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      @impl Ash.Type
      def cast_stored(value, constraints) do
        unquote(subtype_of).cast_stored(
          value,
          type_constraints(constraints, unquote(subtype_constraints))
        )
      end

      if function_exported?(subtype_of, :cast_stored_array, 3) do
        @impl Ash.Type
        def cast_stored_array(value, constraints) do
          unquote(subtype_of).cast_stored_array(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_embedded, 2) do
        @impl Ash.Type
        def dump_to_embedded(value, constraints) do
          unquote(subtype_of).dump_to_embedded(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_embedded_array, 2) do
        @impl Ash.Type
        def dump_to_embedded_array(value, constraints) do
          unquote(subtype_of).dump_to_embedded_array(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_native, 2) do
        @impl Ash.Type
        def dump_to_native(value, constraints) do
          unquote(subtype_of).dump_to_native(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :dump_to_native_array, 2) do
        @impl Ash.Type
        def dump_to_native_array(value, constraints) do
          unquote(subtype_of).dump_to_native_array(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
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
          unquote(subtype_of).generator(
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :handle_change, 3) do
        @impl Ash.Type
        def handle_change(old_term, new_term, constraints) do
          unquote(subtype_of).handle_change(
            old_term,
            new_term,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :handle_change_array, 3) do
        @impl Ash.Type
        def handle_change_array(old_term, new_term, constraints) do
          unquote(subtype_of).handle_change_array(
            old_term,
            new_term,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :prepare_change, 3) do
        @impl Ash.Type
        def prepare_change(old_term, new_term, constraints) do
          unquote(subtype_of).prepare_change(
            old_term,
            new_term,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      if function_exported?(subtype_of, :prepare_change_array, 3) do
        @impl Ash.Type
        def prepare_change_array(old_term, new_term, constraints) do
          unquote(subtype_of).prepare_change_array(
            old_term,
            new_term,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      @impl Ash.Type
      def simple_equality? do
        unquote(subtype_of).simple_equality?()
      end

      @impl Ash.Type
      def storage_type do
        unquote(subtype_of).storage_type()
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
      def apply_constraints(value, constraints) do
        unquote(subtype_of).apply_constraints(
          value,
          type_constraints(constraints, unquote(subtype_constraints))
        )
      end

      if function_exported?(subtype_of, :apply_constraints_array, 3) do
        @impl Ash.Type
        def apply_constraints_array(value, constraints) do
          unquote(subtype_of).apply_constraints_array(
            value,
            type_constraints(constraints, unquote(subtype_constraints))
          )
        end
      end

      @impl Ash.Type
      def cast_in_query?(constraints) do
        unquote(subtype_of).cast_in_query?(
          type_constraints(constraints, unquote(subtype_constraints))
        )
      end

      @impl Ash.Type
      def describe(constraints) do
        unquote(subtype_of).describe(type_constraints(constraints, unquote(subtype_constraints)))
      end

      @impl Ash.Type.NewType
      def type_constraints(constraints, subtype_constraints) do
        Keyword.merge(constraints || [], subtype_constraints)
      end

      defoverridable type_constraints: 2
    end
  end
end

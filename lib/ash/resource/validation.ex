defmodule Ash.Resource.Validation do
  @moduledoc """
  Represents a validation in Ash.

  See `Ash.Resource.Validation.Builtins` for a list of builtin validations.

  To write your own validation, define a module that implements the `c:init/1` callback
  to validate options at compile time, and `c:validate/2` callback to do the validation.

  Then, in a resource, you can say:

  ```
  validations do
    validate {MyValidation, [foo: :bar]}
  end
  ```
  """
  defstruct [
    :validation,
    :module,
    :opts,
    :only_when_valid?,
    :description,
    :message,
    :before_action?,
    :where,
    :always_atomic?,
    on: []
  ]

  @type t :: %__MODULE__{
          validation: {atom(), list(atom())},
          module: atom(),
          opts: list(atom()),
          only_when_valid?: boolean(),
          description: String.t() | nil,
          where: list({atom(), list(atom())}),
          on: list(atom())
        }

  @type path :: [atom | integer]
  @type ref :: {module(), Keyword.t()} | module()

  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
  @callback validate(changeset :: Ash.Changeset.t(), opts :: Keyword.t()) :: :ok | {:error, term}
  @callback describe(opts :: Keyword.t()) ::
              String.t() | [{:message, String.t()} | {:vars, Keyword.t()}]
  @callback atomic?() :: boolean
  @callback atomic(Ash.Changeset.t(), Keyword.t()) ::
              :ok
              | {:atomic, Ash.Expr.t()}
              | {:error, term()}

  @optional_callbacks describe: 1, validate: 2, atomic: 2

  @validation_type {:spark_function_behaviour, Ash.Resource.Validation,
                    Ash.Resource.Validation.Builtins, {Ash.Resource.Validation.Function, 1}}

  @schema [
    validation: [
      type: @validation_type,
      required: true,
      doc:
        "The module (or module and opts) that implements the `Ash.Resource.Validation` behaviour. Also accepts a one argument function that takes the changeset."
    ],
    where: [
      type: {:wrap_list, @validation_type},
      required: false,
      default: [],
      doc: """
      Validations that should pass in order for this validation to apply. Any of these validations failing will result in this validation being ignored.
      """
    ],
    on: [
      type: {:wrap_list, {:in, [:create, :update, :destroy]}},
      default: [:create, :update],
      doc: """
      The action types the validation should run on. Many validations don't make sense in the context of deletion, so by default it is not included.
      """
    ],
    only_when_valid?: [
      type: :boolean,
      default: false,
      doc:
        "If the validation should only run on valid changes. Useful for expensive validations or validations that depend on valid data."
    ],
    message: [
      type: :string,
      doc: "If provided, overrides any message set by the validation error"
    ],
    description: [
      type: :string,
      doc: "An optional description for the validation"
    ],
    before_action?: [
      type: :boolean,
      default: false,
      doc: "If set to `true`, the validation will be run in a before_action hook"
    ],
    always_atomic?: [
      type: :boolean,
      default: false,
      doc:
        "By default, validations are only run atomically if all changes will be run atomically or if there is no `validate/2` callback defined. Set this to `true` to run it atomically always."
    ]
  ]

  @action_schema Keyword.delete(@schema, :on)

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Validation
      @before_compile Ash.Resource.Validation

      @impl Ash.Resource.Validation
      def init(opts), do: {:ok, opts}

      defp with_description(keyword, opts) do
        if Kernel.function_exported?(__MODULE__, :describe, 1) do
          keyword ++ apply(__MODULE__, :describe, [opts])
        else
          keyword
        end
      end

      defoverridable init: 1
    end
  end

  @doc false
  def transform(%{validation: {module, opts}} = validation) do
    {:ok,
     %{
       validation
       | validation: {module, opts},
         module: module,
         opts: opts
     }}
  end

  def opt_schema, do: @schema
  def action_schema, do: @action_schema
  def validation_type, do: @validation_type

  defmacro __before_compile__(_env) do
    quote generated: true do
      unless Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
        @impl Ash.Resource.Validation
        if Module.defines?(__MODULE__, {:atomic, 2}, :def) do
          def atomic?, do: true
        else
          def atomic?, do: false
        end
      end
    end
  end
end

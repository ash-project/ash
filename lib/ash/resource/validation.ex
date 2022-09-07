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

  To make it more readable, you can define a function in the module that returns that tuple,
  and import it into your resource.


  ```
  defmodule MyValidation do
    def my_validation(value) do
      {__MODULE__, foo: value}
    end
  end
  ```

  ```
  defmodule MyResource do
    ...

    import MyValidation

    validations do
      validate my_validation(:foo)
    end
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

  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
  @callback validate(Ash.Changeset.t(), Keyword.t()) :: :ok | {:error, term}

  @schema [
    validation: [
      type: {:spark_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins},
      required: true,
      doc: "The module/opts pair of the validation",
      links: []
    ],
    where: [
      type:
        {:list, {:spark_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins}},
      required: false,
      default: [],
      links: [
        modules: [
          "ash:module:Ash.Resource.Validation.Builtins"
        ]
      ],
      doc: """
      Validations that should pass in order for this validation to apply.
      These validations failing will not invalidate the changes, but will instead result in this validation being ignored.
      """
    ],
    on: [
      type: {:custom, __MODULE__, :on, []},
      default: [:create, :update],
      links: [],
      doc: """
      The action types the validation should run on.

      Many validations don't make sense in the context of deletion, so by default it is left out of the list.
      """
    ],
    only_when_valid?: [
      type: :boolean,
      default: false,
      links: [],
      doc:
        "If the validation should only run on valid changes. Useful for expensive validations or validations that depend on valid data."
    ],
    message: [
      type: :string,
      doc: "If provided, overrides any message set by the validation error",
      links: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the validation",
      links: []
    ],
    before_action?: [
      type: :boolean,
      default: false,
      links: [],
      doc: "If set to `true`, the validation will be run in a before_action hook"
    ]
  ]

  @action_schema Keyword.delete(@schema, :on)

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Validation

      def init(opts), do: {:ok, opts}

      defoverridable init: 1
    end
  end

  @doc false
  def transform(%{validation: {module, opts}} = validation) do
    case module.init(opts) do
      {:ok, opts} -> {:ok, %{validation | validation: {module, opts}, module: module, opts: opts}}
      {:error, error} -> {:error, error}
    end
  end

  def opt_schema, do: @schema
  def action_schema, do: @action_schema

  def on(list) do
    list
    |> List.wrap()
    |> Enum.all?(&(&1 in [:create, :update, :destroy]))
    |> case do
      true ->
        {:ok, List.wrap(list)}

      false ->
        {:error, "Expected items of [:create, :update, :destroy], got: #{inspect(list)}"}
    end
  end
end

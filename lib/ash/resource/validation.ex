defmodule Ash.Resource.Validation do
  @moduledoc """
  Represents a validation in Ash.

  See `Ash.Resource.Validation.Builtin` for a list of builtin validations.

  To write your own validation, define a module that implements the `c:init/1` callback
  to validate options at compile time, and `c:validate/2` callback to do the validation.

  Then, in a resource, you can say:

  ```
  validations do
    validation {MyValidation, [foo: :bar]}
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
  defstruct [:validation, :module, :opts, :expensive?, :description, on: []]

  @type t :: %__MODULE__{
          validation: {atom(), list(atom())},
          module: atom(),
          opts: list(atom()),
          expensive?: boolean(),
          description: String.t() | nil,
          on: list(atom())
        }

  @type path :: [atom | integer]
  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
  @callback validate(Ash.changeset(), Keyword.t()) :: :ok | {:error, Ash.error()}

  @schema [
    validation: [
      type: {:custom, __MODULE__, :validation, []},
      required: true,
      doc: "The module/opts pair of the validation"
    ],
    on: [
      type: {:custom, __MODULE__, :on, []},
      default: [:create, :update],
      doc: """
      The action types the validation should run on.

      Many validations don't make sense in the context of deletion, so by default it is left out of the list.
      """
    ],
    expensive?: [
      type: :boolean,
      default: false,
      doc:
        "If a validation is expensive, it won't be run on invalid changes. All inexpensive validations are always run, to provide informative errors."
    ],
    description: [
      type: :string,
      doc: "An optional description for the validation"
    ]
  ]

  def transform(%__MODULE__{validation: {module, opts}} = validation) do
    {:ok, %{validation | module: module, opts: opts}}
  end

  def opt_schema, do: @schema

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

  def validation({module, opts}) when is_atom(module) do
    if Keyword.keyword?(opts) do
      case module.init(opts) do
        {:ok, opts} ->
          {:ok, {module, opts}}

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, "Validation must be a `{module, opts}` tuple, got: #{inspect({module, opts})}"}
    end
  end

  def validation(module) when is_atom(module) do
    validation({module, []})
  end

  def validation(other) do
    {:error, "Validation must be a `{module, opts}` tuple, got: #{inspect(other)}"}
  end
end

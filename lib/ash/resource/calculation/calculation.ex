defmodule Ash.Resource.Calculation do
  @moduledoc "Represents a named calculation on a resource"
  defstruct [:name, :calculation, :arguments, :description]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The field name to use for the calculation value"
    ],
    calculation: [
      type: {:custom, __MODULE__, :calculation, []},
      required: true,
      doc: "The module or {module, opts} to use for the calculation"
    ],
    description: [
      type: :string,
      doc: "An optional description for the calculation"
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          calculation: {:ok, {atom(), any()}} | {:error, String.t()},
          arguments: list(any()),
          description: String.t() | nil
        }

  defmodule Argument do
    @moduledoc "An argument to a calculation"
    defstruct [:name, :type, :default, :allow_nil?, :constraints]

    @schema [
      name: [
        type: :atom,
        required: true,
        doc: "The name to use for the argument"
      ],
      type: [
        type: {:custom, Ash.OptionsHelpers, :ash_type, []},
        required: true,
        doc: "The type of the argument"
      ],
      default: [
        type: {:custom, Ash.OptionsHelpers, :default, []},
        required: false,
        doc: "A default value to use for the argument if not provided"
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: "Whether or not the argument value may be nil"
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc:
          "Constraints to provide to the type when casting the value. See the type's documentation for more information."
      ]
    ]

    def schema, do: @schema

    def transform(%{constraints: []} = argument), do: {:ok, argument}

    def transform(%{constraints: constraints, type: type} = argument) do
      case type do
        {:array, type} ->
          with {:ok, new_constraints} <-
                 NimbleOptions.validate(
                   Keyword.delete(constraints, :items),
                   Ash.Type.list_constraints()
                 ),
               {:ok, item_constraints} <- validate_item_constraints(type, constraints) do
            {:ok,
             %{argument | constraints: Keyword.put(new_constraints, :items, item_constraints)}}
          end

        type ->
          schema = Ash.Type.constraints(type)

          case NimbleOptions.validate(constraints, schema) do
            {:ok, constraints} ->
              {:ok, %{argument | constraints: constraints}}

            {:error, error} ->
              {:error, error}
          end
      end
    end

    defp validate_item_constraints(type, constraints) do
      if Keyword.has_key?(constraints, :items) do
        schema = Ash.Type.constraints(type)

        case NimbleOptions.validate(constraints[:items], schema) do
          {:ok, item_constraints} ->
            {:ok, item_constraints}

          {:error, error} ->
            {:error, error}
        end
      else
        {:ok, constraints}
      end
    end
  end

  def schema, do: @schema

  def calculation({module, opts}) when is_atom(module) and is_list(opts),
    do: {:ok, {module, opts}}

  def calculation(module) when is_atom(module), do: {:ok, {module, []}}

  def calculation(other) do
    {:error, "Expected a module or {module, opts}, got: #{inspect(other)}"}
  end
end

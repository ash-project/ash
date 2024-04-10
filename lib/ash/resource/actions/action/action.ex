defmodule Ash.Resource.Actions.Action do
  @moduledoc "Represents a custom action on a resource."

  defstruct [
    :name,
    :description,
    :returns,
    :run,
    constraints: [],
    touches_resources: [],
    arguments: [],
    allow_nil?: false,
    transaction?: false,
    primary?: false,
    type: :action
  ]

  @type t :: %__MODULE__{
          type: :action,
          name: atom,
          description: String.t() | nil,
          arguments: [Ash.Resource.Actions.Argument.t()],
          allow_nil?: boolean,
          touches_resources: [Ash.Resource.t()],
          constraints: Keyword.t(),
          run: {module, Keyword.t()},
          returns: Ash.Type.t() | nil,
          primary?: boolean,
          transaction?: boolean
        }

  import Ash.Resource.Actions.SharedOptions

  def transform(%{returns: nil} = action) do
    {:ok, action}
  end

  def transform(%{returns: original_type, constraints: constraints} = thing) do
    type = Ash.Type.get_type(original_type)

    ash_type? =
      try do
        Ash.Type.ash_type?(type)
      rescue
        _ ->
          false
      end

    unless ash_type? do
      raise """
      #{inspect(original_type)} is not a valid type.

      Valid types include any custom types, or the following short codes (alongside the types they map to):

      #{Enum.map_join(Ash.Type.short_names(), "\n", fn {name, type} -> "  #{inspect(name)} -> #{inspect(type)}" end)}

      """
    end

    case Ash.Type.validate_constraints(type, constraints) do
      {:ok, constraints} ->
        {:ok, %{thing | returns: type, constraints: constraints}}

      {:error, error} ->
        {:error, error}
    end
  end

  @global_opts shared_options()
  @opt_schema [
                returns: [
                  type: Ash.OptionsHelpers.ash_type(),
                  doc: "The return type of the action. See `Ash.Type` for more."
                ],
                constraints: [
                  type: :keyword_list,
                  doc: """
                  Constraints for the return type. See `Ash.Type` for more.
                  """
                ],
                allow_nil?: [
                  type: :boolean,
                  default: false,
                  doc: """
                  Whether or not the action can return nil. Unlike attributes & arguments, this defaults to `false`.
                  """
                ],
                run: [
                  type:
                    {:or,
                     [
                       {:spark_function_behaviour, Ash.Resource.Actions.Implementation,
                        {Ash.Resource.Action.ImplementationFunction, 2}},
                       {:spark, Reactor}
                     ]}
                ]
              ]
              |> Spark.Options.merge(
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end

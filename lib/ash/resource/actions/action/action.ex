# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.Action do
  @moduledoc "Represents a custom action on a resource."

  defstruct [
    :name,
    :description,
    :returns,
    :run,
    constraints: [],
    touches_resources: [],
    skip_unknown_inputs: [],
    arguments: [],
    preparations: [],
    allow_nil?: false,
    transaction?: false,
    primary?: false,
    skip_global_validations?: false,
    type: :action,
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{
          type: :action,
          name: atom,
          description: String.t() | nil,
          arguments: [Ash.Resource.Actions.Argument.t()],
          skip_unknown_inputs: list(atom() | String.t()),
          allow_nil?: boolean,
          touches_resources: [Ash.Resource.t()],
          constraints: Keyword.t(),
          run: {module, Keyword.t()},
          returns: Ash.Type.t() | nil,
          primary?: boolean,
          transaction?: boolean,
          preparations: [Ash.Resource.Preparation.t()],
          skip_global_validations?: boolean,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  import Ash.Resource.Actions.SharedOptions

  def transform(%{returns: nil} = action) do
    {:ok, action}
  end

  def transform(%{returns: original_type, constraints: constraints} = thing) do
    type = Ash.Type.get_type!(original_type)

    case Ash.Type.init(type, constraints) do
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
                     ]},
                  doc: """
                  Module may be an `Ash.Resource.Actions.Implementation` or `Reactor`.
                  """
                ]
              ]
              |> Spark.Options.merge(
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end

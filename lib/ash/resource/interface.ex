# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface

  See the functions defined in this module for specifications of the options
  that each type of code interface function supports in its options list.
  """
  defstruct [
    :name,
    :action,
    :get?,
    :get_by,
    :get_by_identity,
    :not_found_error?,
    args: [],
    custom_inputs: [],
    exclude_inputs: [],
    default_options: [],
    require_reference?: true,
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{__spark_metadata__: Spark.Dsl.Entity.spark_meta()}

  defp set_get?(definition) do
    if definition.get_by || definition.get_by_identity do
      %{definition | get?: true}
    else
      definition
    end
  end

  defp set_require_reference?(%{get?: true} = definition) do
    %{definition | require_reference?: false}
  end

  defp set_require_reference?(definition) do
    definition
  end

  defmodule CanOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: Keyword.delete(Ash.can_opts(), :actor)
  end

  defmodule CanQuestionMarkOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: Keyword.delete(Ash.can_question_mark_opts(), :actor)
  end

  defmodule CalculateOpts do
    @moduledoc false

    use Spark.Options.Validator,
      schema: Keyword.drop(Ash.calculate_opts(), [:domain, :refs, :args, :record])
  end

  defmodule CreateOpts do
    @moduledoc false

    use Spark.Options.Validator,
      schema:
        Keyword.merge(Ash.create_opts(),
          changeset: [
            type: :any,
            doc: "A changeset to seed the action with."
          ],
          bulk_options: [
            type: :keyword_list,
            doc:
              "Options passed to `Ash.bulk_create`, if a list or stream of inputs is provided.",
            keys: Keyword.drop(Ash.bulk_create_opts(), Keyword.keys(Ash.create_opts()))
          ],
          private_arguments: [
            type: :map,
            doc: "Private argument values to set before validations and changes.",
            default: %{}
          ]
        )
        |> Keyword.drop([:domain])
  end

  defmodule UpdateOpts do
    @moduledoc false
    use Spark.Options.Validator,
      schema:
        Ash.update_opts()
        |> Keyword.merge(
          bulk_options: [
            type: :keyword_list,
            doc:
              "Options passed to `Ash.bulk_update`, if a query, list, or stream of inputs is provided.",
            keys:
              Keyword.drop(Ash.bulk_update_opts(), Keyword.keys(Ash.update_opts()) ++ [:resource])
          ],
          private_arguments: [
            type: :map,
            doc: "Private argument values to set before validations and changes.",
            default: %{}
          ]
        )
        |> Keyword.drop([:domain])
  end

  defmodule DestroyOpts do
    @moduledoc false
    use Spark.Options.Validator,
      schema:
        Ash.destroy_opts()
        |> Keyword.merge(
          bulk_options: [
            type: :keyword_list,
            doc:
              "Options passed to `Ash.bulk_destroy`, if a query, list, or stream of inputs is provided.",
            keys:
              Keyword.drop(
                Ash.bulk_destroy_opts(),
                Keyword.keys(Ash.destroy_opts()) ++ [:resource]
              )
          ],
          private_arguments: [
            type: :map,
            doc: "Private argument values to set before validations and changes.",
            default: %{}
          ]
        )
        |> Keyword.drop([:domain])
  end

  defmodule GetOpts do
    @moduledoc false
    use Spark.Options.Validator,
      schema:
        Ash.read_opts()
        |> Keyword.merge(
          query: [
            type: {:or, [{:behaviour, Ash.Resource}, {:struct, Ash.Query}, :keyword_list]},
            doc: "A query to seed the action with."
          ],
          not_found_error?: [
            type: :boolean,
            doc:
              "Whether or not to return or raise a `NotFound` error or to return `nil` when a get? action/interface is called."
          ]
        )
        |> Keyword.drop([:domain])
  end

  defmodule ReadOpts do
    @moduledoc false
    use Spark.Options.Validator,
      schema:
        Ash.read_opts()
        |> Keyword.merge(
          query: [
            type: {:or, [{:behaviour, Ash.Resource}, {:struct, Ash.Query}, :keyword_list]},
            doc: "A query to seed the action with."
          ],
          not_found_error?: [
            type: :boolean,
            doc:
              "Whether or not to return or raise a `NotFound` error or to return `nil` when a get? action/interface is called."
          ]
        )
        |> Keyword.merge(
          stream?: [
            type: :boolean,
            default: false,
            doc: "If true, a stream of the results will be returned"
          ],
          stream_options: [
            type: :keyword_list,
            doc: "Options passed to `Ash.stream!`, if `stream?: true` is given",
            keys: Keyword.drop(Ash.stream_opts(), Keyword.keys(Ash.read_opts()))
          ]
        )
        |> Keyword.drop([:domain])
  end

  defmodule ActionOpts do
    @moduledoc false
    use Spark.Options.Validator,
      schema:
        Ash.run_action_opts()
        |> Keyword.merge(
          private_arguments: [
            type: :map,
            doc: "Private argument values to set before validations and changes.",
            default: %{}
          ]
        )
        |> Keyword.drop([:domain])
  end

  @doc """
  Options supported by code interfaces for create actions

  ## Options

  #{CreateOpts.docs()}
  """
  def create_opts do
    CreateOpts
  end

  @doc """
  Options supported by code interfaces for read actions with `get?` or `get_by` set.

  ## Options

  #{ReadOpts.docs()}
  """
  def get_opts do
    ReadOpts
  end

  @doc """
  Options supported by code interfaces for read actions

  ## Options

  #{ReadOpts.docs()}
  """
  def read_opts do
    ReadOpts
  end

  @doc """
  Options supported by code interfaces for update actions

  ## Options

  #{UpdateOpts.docs()}
  """
  def update_opts do
    UpdateOpts
  end

  @doc """
  Options supported by code interfaces for destroy actions

  ## Options

  #{DestroyOpts.docs()}
  """
  def destroy_opts do
    DestroyOpts
  end

  @doc """
  Options supported by code interfaces for generic actions

  ## Options

  #{ActionOpts.docs()}
  """
  def generic_action_opts do
    ActionOpts
  end

  @doc """
  Options supported by code interfaces for calculations

  ## Options

  #{CalculateOpts.docs()}
  """
  def calculate_opts do
    CalculateOpts
  end

  @doc false
  def interface_options(:calculate, _) do
    calculate_opts()
  end

  def interface_options(:create, _) do
    create_opts()
  end

  def interface_options(:update, _) do
    update_opts()
  end

  def interface_options(:destroy, _) do
    destroy_opts()
  end

  def interface_options(:read, interface) do
    if interface.get? do
      get_opts()
    else
      read_opts()
    end
  end

  def interface_options(:action, _) do
    generic_action_opts()
  end

  @schema [
    name: [
      type: :atom,
      doc: "The name of the function that will be defined",
      required: true
    ],
    action: [
      type: :atom,
      doc:
        "The name of the action that will be called. Defaults to the same name as the function."
    ],
    args: [
      type: {:list, {:or, [:atom, {:tagged_tuple, :optional, :atom}]}},
      doc: """
      Map specific arguments to named inputs. Can provide any argument/attributes that the action allows.
      """
    ],
    not_found_error?: [
      type: :boolean,
      default: true,
      doc:
        "If the action or interface is configured with `get?: true`, this determines whether or not an error is raised or `nil` is returned."
    ],
    require_reference?: [
      type: :boolean,
      default: true,
      doc:
        "For update and destroy actions, require a resource or identifier to be passed in as the first argument. Not relevant for other action types."
    ],
    exclude_inputs: [
      type: {:list, :atom},
      default: [],
      doc: "A list of action inputs to not accept in the defined interface"
    ],
    get?: [
      type: :boolean,
      default: false,
      doc: """
      Expects to only receive a single result from a read action or a bulk update/destroy, and returns a single result instead of a list. Sets `require_reference?` to false automatically.
      """
    ],
    get_by: [
      type: {:wrap_list, :atom},
      doc: """
      Takes a list of fields and adds those fields as arguments, which will then be used to filter. Sets `get?` to true and `require_reference?` to false automatically. Adds filters for read, update and destroy actions, replacing the `record` first argument.
      """
    ],
    get_by_identity: [
      type: :atom,
      doc: """
      Takes an identity, gets its field list, and performs the same logic as `get_by` with those fields. Adds filters for read, update and destroy actions, replacing the `record` first argument.
      """
    ],
    default_options: [
      type: {:or, [:keyword_list, {:fun, 0}]},
      default: [],
      doc:
        "Default options to be merged with client-provided options. These can override domain or action defaults. `:load`, `:bulk_options`, and `:page` options will be deep merged. Can be a keyword list or a zero-arity function that returns a keyword list."
    ]
  ]

  @doc false
  def schema, do: @schema

  @doc false
  def transform(definition) do
    {:ok,
     definition
     |> set_get?()
     |> set_require_reference?()}
  end
end

defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface
  """
  defstruct [
    :name,
    :action,
    :args,
    :get?,
    :get_by,
    :get_by_identity,
    :not_found_error?,
    default_options: [],
    require_reference?: true
  ]

  @type t :: %__MODULE__{}

  def transform(definition) do
    {:ok,
     definition
     |> set_get?()
     |> set_require_reference?()}
  end

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
              "Options passed to `Ash.bulk_create`, if a query, list, or stream of inputs is provided.",
            keys:
              Keyword.drop(Ash.bulk_update_opts(), Keyword.keys(Ash.update_opts()) ++ [:resource])
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
              "Options passed to `Ash.bulk_create`, if a query, list, or stream of inputs is provided.",
            keys:
              Keyword.drop(
                Ash.bulk_destroy_opts(),
                Keyword.keys(Ash.destroy_opts()) ++ [:resource]
              )
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
        |> Keyword.drop([:domain])
  end

  def interface_options(:calculate, _) do
    CalculateOpts
  end

  def interface_options(:create, _) do
    CreateOpts
  end

  def interface_options(:update, _) do
    UpdateOpts
  end

  def interface_options(:destroy, _) do
    DestroyOpts
  end

  def interface_options(:read, interface) do
    if interface.get? do
      GetOpts
    else
      ReadOpts
    end
  end

  def interface_options(:action, _) do
    ActionOpts
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
      type: :keyword_list,
      default: [],
      doc:
        "Default options to be merged with client-provided options. These can override domain or action defaults."
    ]
  ]

  def schema, do: @schema
end

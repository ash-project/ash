defmodule Ash.Resource.Actions.Read do
  @moduledoc "Represents a read action on a resource."

  defstruct arguments: [],
            description: nil,
            filter: nil,
            filters: [],
            get_by: nil,
            get?: nil,
            manual: nil,
            metadata: [],
            skip_unknown_inputs: [],
            modify_query: nil,
            multitenancy: nil,
            name: nil,
            pagination: nil,
            preparations: [],
            primary?: nil,
            touches_resources: [],
            timeout: nil,
            transaction?: false,
            type: :read

  @type t :: %__MODULE__{
          arguments: [Ash.Resource.Actions.Argument.t()],
          description: String.t() | nil,
          filter: any,
          get_by: nil | atom | [atom],
          get?: nil | boolean,
          filters: [any],
          manual: atom | {atom, Keyword.t()} | nil,
          metadata: [Ash.Resource.Actions.Metadata.t()],
          skip_unknown_inputs: list(atom | String.t()),
          modify_query: nil | mfa,
          multitenancy: atom,
          name: atom,
          pagination: any,
          primary?: boolean,
          touches_resources: [atom],
          timeout: pos_integer() | nil,
          transaction?: boolean,
          type: :read
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()

  @opt_schema Spark.Options.merge(
                [
                  manual: [
                    type:
                      {:spark_function_behaviour, Ash.Resource.ManualRead,
                       {Ash.Resource.ManualRead.Function, 3}},
                    doc: """
                    Delegates running of the query to the provided module. Accepts a module or module and opts, or a function that takes the ash query, the data layer query, and context. See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                    """
                  ],
                  get?: [
                    type: :boolean,
                    default: false,
                    doc: """
                    Expresses that this action innately only returns a single result. Used by extensions to validate and/or modify behavior. Causes code interfaces to return a single value instead of a list. See the [code interface guide](/documentation/topics/resources/code-interfaces.md) for more.
                    """
                  ],
                  modify_query: [
                    type: {:or, [:mfa, {:fun, 2}]},
                    doc: """
                    Allows direct manipulation of the data layer query via an MFA. The ash query and the data layer query will be provided as additional arguments. The result must be `{:ok, new_data_layer_query} | {:error, error}`.
                    """
                  ],
                  get_by: [
                    type: {:wrap_list, :atom},
                    default: nil,
                    doc: """
                    A helper to automatically generate a "get by X" action. Sets `get?` to true, add args for each of the specified fields, and adds a filter for each of the arguments.
                    """
                  ],
                  timeout: [
                    type: :pos_integer,
                    doc: """
                    The maximum amount of time, in milliseconds, that the action is allowed to run for. Ignored if the data layer doesn't support transactions *and* async is disabled.
                    """
                  ],
                  multitenancy: [
                    type: {:in, [:enforce, :allow_global, :bypass]},
                    default: :enforce,
                    doc: """
                    This setting defines how this action handles multitenancy. `:enforce` requires a tenant to be set (the default behavior), `:allow_global` allows using this action both with and without a tenant, `:bypass` completely ignores the tenant even if it's set. This is useful to change the behaviour of selected read action without the need of marking the whole resource with `global? true`.
                    """
                  ]
                ],
                @global_opts,
                "Action Options"
              )

  @pagination_schema [
    keyset?: [
      type: :boolean,
      doc: "Whether or not keyset based pagination is supported",
      default: false
    ],
    offset?: [
      type: :boolean,
      doc: "Whether or not offset based pagination is supported",
      default: false
    ],
    default_limit: [
      type: :pos_integer,
      doc: "The default page size to apply, if one is not supplied"
    ],
    # Change this default in 4.0
    countable: [
      type: {:in, [true, false, :by_default]},
      doc:
        "Whether not a returned page will have a full count of all records. Use `:by_default` to do it automatically.",
      default: true
    ],
    max_page_size: [
      type: :pos_integer,
      doc: "The maximum amount of records that can be requested in a single page",
      default: 250
    ],
    required?: [
      type: :boolean,
      doc:
        "Whether or not pagination can be disabled (by passing `page: false` to `Ash.Api.read!/2`, or by having `required?: false, default_limit: nil` set). Only relevant if some pagination configuration is supplied.",
      default: true
    ]
  ]

  defmodule Pagination do
    @moduledoc "Represents the pagination configuration of a read action"
    defstruct [
      :default_limit,
      :max_page_size,
      countable: false,
      required?: false,
      keyset?: false,
      offset?: false
    ]

    @type t :: %__MODULE__{}

    def transform(pagination) do
      if pagination.keyset? or pagination.offset? do
        {:ok, pagination}
      else
        {:error, "Must enable `keyset?` or `offset?`"}
      end
    end
  end

  def transform(read) do
    {:ok,
     read
     |> transform_pagination()
     |> concat_filters()}
  end

  def concat_filters(%{filters: [filter]} = read) do
    %{read | filter: filter.filter}
  end

  def concat_filters(%{filters: [first | rest]} = read) do
    filter =
      Enum.reduce(rest, first.filter, fn filter, acc ->
        Ash.Query.BooleanExpression.new(:and, filter.filter, acc)
      end)

    %{read | filter: filter}
  end

  @doc false
  def concat_filters(read) do
    read
  end

  defp transform_pagination(read) do
    if read.pagination do
      if is_list(read.pagination) do
        %{read | pagination: List.last(read.pagination) || false}
      else
        %{read | pagination: read.pagination}
      end
    else
      %{read | pagination: false}
    end
  end

  @doc false
  def opt_schema, do: @opt_schema
  def pagination_schema, do: @pagination_schema
end

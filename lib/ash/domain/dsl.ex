defmodule Ash.Domain.Dsl do
  @domain %Spark.Dsl.Section{
    name: :domain,
    describe: "General domain configuration",
    examples: [
      """
      domain do
        description \"\"\"
        Resources related to the flux capacitor.
        \"\"\"
      end
      """
    ],
    schema: [
      description: [
        type: :string,
        doc: """
        A description for the domain.
        """
      ]
    ]
  }

  @execution %Spark.Dsl.Section{
    name: :execution,
    describe: "Options for how requests are executed using this domain",
    examples: [
      """
      execution do
        timeout :timer.seconds(30)
      end
      """
    ],
    schema: [
      timeout: [
        type: :timeout,
        doc: """
        The default timeout in milliseconds to use for requests using this domain. See the [timeouts guide](/documentation/topics/timeouts.md) for more.
        """,
        default: :infinity
      ],
      trace_name: [
        type: :string,
        doc: """
        The name to use in traces. Defaults to the last part of the module. See the [monitoring guide](/documentation/topics/monitoring.md) for more
        """
      ]
    ]
  }

  @authorization %Spark.Dsl.Section{
    name: :authorization,
    describe: """
    Options for how requests are authorized using this domain. See the [Sensitive Data guide](/documentation/topics/security/sensitive-data.md) for more.
    """,
    examples: [
      """
      authorization do
        authorize :always
      end
      """
    ],
    schema: [
      require_actor?: [
        type: :boolean,
        default: false,
        doc: "Requires that an actor has been supplied."
      ],
      authorize: [
        type: {:in, [:always, :by_default, :when_requested]},
        default: :by_default,
        doc: """
        When to run authorization for a given request.
        """
      ]
    ]
  }

  defmodule ResourceReference do
    @moduledoc "A resource reference in a domain"
    defstruct [:resource, definitions: []]

    @type t :: %__MODULE__{
            resource: module(),
            definitions: list(Ash.Resource.Interface.t() | Ash.Resource.CalculationInterface.t())
          }
  end

  @define %Spark.Dsl.Entity{
    name: :define,
    describe: """
    Defines a function with the corresponding name and arguments. See the [code interface guide](/documentation/topics/resources/code-interfaces.md) for more.
    """,
    examples: [
      "define :get_user_by_id, action: :get_by_id, args: [:id], get?: true"
    ],
    target: Ash.Resource.Interface,
    schema: Ash.Resource.Interface.schema(),
    transform: {Ash.Resource.Interface, :transform, []},
    args: [:name]
  }

  @define_calculation %Spark.Dsl.Entity{
    name: :define_calculation,
    describe: """
    Defines a function with the corresponding name and arguments, that evaluates a calculation. Use `:_record` to take an instance of a record. See the [code interface guide](/documentation/topics/resources/code-interfaces.md) for more.
    """,
    examples: [
      "define_calculation :referral_link, User, args: [:id]",
      "define_calculation :referral_link, User, args: [{:arg, :id}, {:ref, :id}]"
    ],
    target: Ash.Resource.CalculationInterface,
    schema: Ash.Resource.CalculationInterface.schema(),
    transform: {Ash.Resource.CalculationInterface, :transform, []},
    args: [:name]
  }

  @resource %Spark.Dsl.Entity{
    name: :resource,
    describe: "A resource present in the domain",
    examples: [
      "resource Foo"
    ],
    target: ResourceReference,
    entities: [
      definitions: [@define, @define_calculation]
    ],
    args: [:resource],
    modules: [:resource],
    schema: [
      resource: [
        type: {:spark, Ash.Resource},
        required: true
      ]
    ]
  }

  @resources %Spark.Dsl.Section{
    name: :resources,
    describe: "List the resources of this domain",
    examples: [
      """
      resources do
        resource MyApp.Tweet
        resource MyApp.Comment
      end
      """
    ],
    entities: [
      @resource
    ],
    schema: [
      allow: [
        type: :mfa,
        doc: """
        Support a dynamic resource list by providing a callback that checks whether or not the resource should be allowed.
        """
      ],
      allow_unregistered?: [
        type: :boolean,
        default: false,
        doc: """
        Whether the domain will support only registered entries or not.
        """
      ]
    ]
  }

  @sections [@domain, @resources, @execution, @authorization]

  @verifiers [
    Ash.Domain.Verifiers.EnsureNoEmbeds,
    Ash.Domain.Verifiers.ValidateRelatedResourceInclusion,
    Ash.Domain.Verifiers.ValidateArgumentsToCodeInterface
  ]

  @moduledoc false

  use Spark.Dsl.Extension, sections: @sections, verifiers: @verifiers
end

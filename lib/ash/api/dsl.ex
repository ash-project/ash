defmodule Ash.Api.Dsl do
  @api %Spark.Dsl.Section{
    name: :api,
    describe: "General Api configuration",
    examples: [
      """
      api do
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
        A description for the api.
        """
      ]
    ]
  }

  @execution %Spark.Dsl.Section{
    name: :execution,
    describe: "Options for how requests are executed using this Api",
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
        The default timeout to use for requests using this API. See the [timeouts guide](/documentation/topics/timeouts.md) for more.
        """,
        default: 30_000
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
    Options for how requests are authorized using this Api. See the [security guide](/documentation/topics/security.md) for more.
    """,
    examples: [
      """
      authorization do
        authorize :when_requested
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
    @moduledoc "A resource reference in an api"
    defstruct [:resource]
  end

  @resource %Spark.Dsl.Entity{
    name: :resource,
    describe: "A resource present in the API",
    examples: [
      "resource Foo"
    ],
    target: ResourceReference,
    args: [:resource],
    no_depend_modules: [:resource],
    schema: [
      resource: [
        type: {:spark, Ash.Resource},
        required: true
      ]
    ]
  }

  @resources %Spark.Dsl.Section{
    name: :resources,
    describe: "List the resources present in this API",
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
        Whether the Api will support only registered entries or not.
        """
      ],
      registry: [
        type: {:behaviour, Ash.Registry},
        doc: """
        Configure a registry that contains the resources. This option is generally not necessary anymore, and remains for backwards compatibility. Instead, configure resources in this block directly.
        """
      ]
    ],
    no_depend_modules: [:registry]
  }

  @sections [@api, @resources, @execution, @authorization]

  @verifiers [
    Ash.Api.Verifiers.EnsureNoEmbeds,
    Ash.Api.Verifiers.EnsureResourcesCompiled,
    Ash.Api.Verifiers.ValidateRelatedResourceInclusion
  ]

  @moduledoc false

  use Spark.Dsl.Extension, sections: @sections, verifiers: @verifiers
end

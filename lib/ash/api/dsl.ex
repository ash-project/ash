defmodule Ash.Api.Dsl do
  @execution %Spark.Dsl.Section{
    name: :execution,
    describe: "Options for how requests are executed using this Api",
    links: [],
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
        doc: "The default timeout to use for requests using this API.",
        default: 30_000,
        links: [
          guides: [
            "ash:guide:Timeouts"
          ]
        ]
      ],
      trace_name: [
        type: :string,
        doc: "The name to use in traces. Defaults to the last part of the module",
        links: [
          guides: [
            "ash:guide:Instrumentation"
          ]
        ]
      ]
    ]
  }

  @authorization %Spark.Dsl.Section{
    name: :authorization,
    describe: "Options for how requests are authorized using this Api",
    examples: [
      """
      authorization do
        authorize :by_default
        require_actor? true
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Security"
      ]
    ],
    schema: [
      require_actor?: [
        type: :boolean,
        default: false,
        doc: "Requires that an actor has been supplied.",
        links: [
          guides: ["ash:guide:Security"]
        ]
      ],
      authorize: [
        type: {:in, [:always, :by_default, :when_requested]},
        default: :when_requested,
        doc: """
        When to run authorization for a given request.
        """,
        links: [
          guides: ["ash:guide:Security"]
        ]
      ]
    ]
  }

  @resources %Spark.Dsl.Section{
    name: :resources,
    describe: "List the resources present in this API",
    examples: [
      """
      resources do
        registry MyApp.Registry
      en
      """
    ],
    links: [],
    schema: [
      allow: [
        type: :mfa,
        doc: """
        Support a dynamic resource list by providing a callback that checks whether or not the resource should be allowed.
        """,
        links: []
      ],
      allow_unregistered?: [
        type: :boolean,
        default: false,
        links: [],
        doc: """
        Whether the Api will support only registered entries or not.
        """
      ],
      registry: [
        type: {:behaviour, Ash.Registry},
        links: [
          guides: [
            "ash:guide:Quick Start"
          ]
        ],
        doc: """
        Configure the registry that contains the resources. It is recommended to use application config for this, to help with compile times. See the quick start guide for more.
        """
      ]
    ],
    no_depend_modules: [:registry]
  }

  @sections [@resources, @execution, @authorization]

  @moduledoc """
  A small DSL for declaring APIs

  Apis are the entrypoints for working with your resources.

  Apis may optionally include a list of resources, in which case they can be
  used as an `Ash.Registry` in various places. This is for backwards compatibility,
  but if at all possible you should define an `Ash.Registry` if you are using an extension
  that requires a list of resources. For example, most extensions look for two application
  environment variables called `:ash_apis` and `:ash_registries` to find any potential registries

  <!--- ash-hq-hide-start --> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop --> <!--- -->
  """

  use Spark.Dsl.Extension, sections: @sections
end

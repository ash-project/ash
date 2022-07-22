defmodule Ash.Api.Dsl do
  @execution %Ash.Dsl.Section{
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
        doc: "The default timeout to use for requests using this API.",
        default: 30_000
      ]
    ]
  }

  @authorization %Ash.Dsl.Section{
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
    schema: [
      require_actor?: [
        type: :boolean,
        default: false,
        doc: """
        Requires that an actor has been supplied.
        Important: `nil` is still a valid actor, so this won't prevent providing `actor: nil`.

        This doesn't necessarily enforce validation, because `authorize :by_default` still supports passing
        `authorize?: false` when making requests.
        """
      ],
      authorize: [
        type: {:in, [:always, :by_default, :when_requested]},
        default: :when_requested,
        doc: """
        `:always` forces `authorize?: true` on all requests to the Api. `:by_default` sets `authorize?: true` if the `authorize?` option was not set (so it can be set to `false`).
        `:when_requested` sets `authorize?: true` whenever an actor is set or `authorize?: true` is explicitly passed.
        """
      ]
    ]
  }

  @resources %Ash.Dsl.Section{
    name: :resources,
    describe: "List the resources present in this API",
    examples: [
      """
      resources do
        registry MyApp.Registry
      end
      """
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
        By default, an api will only work with resources that are explicitly included in the provided registry. In order to separate your
        application into multiple domains, you may wish to "mix and match" your resources across contexts. Specifying this option allows you
        to refer to resources in different apis in your resources, and allows providing any resource to api actions (to facilitate that requirement).

        Be sure to remove the Ash.Registry.ResourceValidations extension from your registry as well.
        """
      ],
      registry: [
        type: {:behaviour, Ash.Registry},
        doc: """
        Allows declaring that only the modules in a certain registry should be allowed to work with this Api.

        This option is ignored if any explicit resources are included in the api, so everything is either in the registry
        or in the api. See the docs on `Ash.Registry` for what the registry is used for.

        To optimize for compile times, you can place the connection from the api to the registry in application configuration.

        To accomplish this:
        1. Configure an `otp_app` when using your api, e.g `use Ash.Api, otp_app: :my_app`
        2. Add application config to set up the connection

        ```elixir
        config :my_app, MyApp.Api,
          resources: [
            registry: MyApp.Api.Registry
          ]
        ```
        """
      ]
    ],
    modules: [:registry]
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

  <!--- ash-hq-hide-start -->

  ## DSL Documentation

  ### Index

  #{Ash.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Ash.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop -->
  """

  use Ash.Dsl.Extension, sections: @sections
end

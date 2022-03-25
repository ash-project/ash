defmodule Ash.Api.Dsl do
  @resource %Ash.Dsl.Entity{
    name: :resource,
    describe: "A reference to a resource",
    target: Ash.Api.ResourceReference,
    args: [:resource],
    examples: [
      "resource MyApp.User"
    ],
    schema: [
      resource: [
        type: :atom,
        required: true,
        doc: "The module of the resource"
      ]
    ]
  }

  @execution %Ash.Dsl.Section{
    name: :execution,
    describe: "Options for how requests are executed using this Api",
    examples: [
      """
      execution do
        timeout 30_000
      end
      """
    ],
    schema: [
      timeout: [
        type: :timeout,
        doc: "The default timeout to use for requests using this API.",
        default: :infinity
      ]
    ]
  }

  @resources %Ash.Dsl.Section{
    name: :resources,
    describe: "List the resources present in this API",
    examples: [
      """
      resources do
        resource MyApp.User
        resource MyApp.Post
        resource MyApp.Comment
      end
      """
    ],
    schema: [
      allow_unregistered?: [
        type: :boolean,
        default: false,
        doc: """
        This is still experimental, but will be supported if you run into any issues.

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
    modules: [:registry],
    deprecations: [
      resource: """
      Please define your resources in an `Ash.Registry`. For example:

      # my_app/my_api/registry.ex
      defmodule MyApp.MyApi.Registry do
        use Ash.Registry,
          extensions: [Ash.Registry.ResourceValidations]

        entries do
          entry MyApp.Post
          entry MyApp.Comment
        end
      end

      # In your api module
      resources do
        registry MyApp.MyApi.Registry
      end
      """
    ],
    entities: [
      @resource
    ]
  }

  @sections [@resources, @execution]

  @moduledoc """
  A small DSL for declaring APIs

  Apis are the entrypoints for working with your resources.

  Apis may optionally include a list of resources, in which case they can be
  used as an `Ash.Registry` in various places. This is for backwards compatibility,
  but if at all possible you should define an `Ash.Registry` if you are using an extension
  that requires a list of resources. For example, most extensions look for two application
  environment variables called `:ash_apis` and `:ash_registries` to find any potential registries

  # Table of Contents
  #{Ash.Dsl.Extension.doc_index(@sections)}

  #{Ash.Dsl.Extension.doc(@sections)}
  """

  use Ash.Dsl.Extension, sections: @sections
end

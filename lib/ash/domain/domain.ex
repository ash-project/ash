defmodule Ash.Domain do
  @moduledoc """
  A domain allows you to interact with your resources, and holds domain-wide configuration.

  For example, the json domain extension adds an domain extension that lets you toggle authorization on/off
  for all resources in a given domain. You include resources in your domain like so:

  ```elixir
  defmodule MyApp.MyDomain do
    use Ash.Domain

    resources do
      resource OneResource
      resource SecondResource
    end
  end
  ```
  """

  use Spark.Dsl,
    default_extensions: [extensions: [Ash.Domain.Dsl]],
    opt_schema: [
      validate_config_inclusion?: [
        type: :boolean,
        default: true
      ]
    ]

  @type t() :: module

  @impl Spark.Dsl
  def explain(dsl_state, _opts) do
    Ash.Domain.Info.description(dsl_state)
  end

  @impl true
  def verify(module, opts) do
    if Application.get_env(:ash, :validate_domain_config_inclusion?, true) &&
         Keyword.get(opts, :validate_config_inclusion?, true) && Code.ensure_loaded?(Mix.Project) do
      otp_app = Mix.Project.config()[:app]

      domains =
        Application.get_env(otp_app, :ash_domains, [])

      if module not in domains do
        IO.warn("""
        Domain #{inspect(module)} is not present in

            config :#{otp_app}, ash_domains: #{inspect(domains)}.


        To resolve this warning, do one of the following.

        1. Add the domain to your configured domain modules. The following snippet can be used.

            config :#{otp_app}, ash_domains: #{inspect(domains ++ [module])}

        2. Add the option `validate_config_inclusion?: false` to `use Ash.Domain`

        3. Configure all domains not to warn, with `config :ash, :validate_domain_config_inclusion?, false`
        """)
      end
    end
  end

  @doc false
  # sobelow_skip ["DOS.StringToAtom"]
  @impl Spark.Dsl
  def handle_before_compile(_) do
    quote do
      use Ash.Domain.Interface

      @default_short_name __MODULE__
                          |> Module.split()
                          |> List.last()
                          |> Macro.underscore()
                          |> String.to_atom()

      def default_short_name do
        @default_short_name
      end
    end
  end
end

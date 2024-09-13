defmodule Mix.Tasks.Ash.Gen.Domain do
  @example "mix ash.gen.domain MyApp.Accounts"
  @moduledoc """
  Generates an Ash.Domain

  ## Example

  ```bash
  #{@example}
  ```
  """

  @shortdoc "Generates an Ash.Domain"
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def info(_argv, _parent) do
    %Igniter.Mix.Task.Info{
      positional: [:domain],
      example: @example
    }
  end

  @impl Igniter.Mix.Task
  def igniter(igniter, argv) do
    {%{domain: domain}, argv} = positional_args!(argv)
    domain = Igniter.Code.Module.parse(domain)

    domain_file = Igniter.Project.Module.proper_location(igniter, domain)

    app_name = Igniter.Project.Application.app_name(igniter)

    if "--ignore-if-exists" in argv && Igniter.exists?(igniter, domain_file) do
      igniter
    else
      igniter
      |> Igniter.create_new_file(domain_file, """
      defmodule #{inspect(domain)} do
        use Ash.Domain

        resources do
        end
      end
      """)
      |> Igniter.Project.Config.configure(
        "config.exs",
        app_name,
        [:ash_domains],
        [domain],
        updater: fn list ->
          Igniter.Code.List.prepend_new_to_list(
            list,
            domain
          )
        end
      )
    end
  end
end

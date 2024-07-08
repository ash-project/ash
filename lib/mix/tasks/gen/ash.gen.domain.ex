defmodule Mix.Tasks.Ash.Gen.Domain do
  @moduledoc """
  Generates an Ash.Domain

  ## Example

  ```bash
  mix ash.gen.domain MyApp.Accounts
  ```
  """

  @shortdoc "Generates an Ash.Domain"
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def igniter(igniter, [domain | argv]) do
    domain = Igniter.Code.Module.parse(domain)
    domain_file = Igniter.Code.Module.proper_location(domain)

    app_name = Igniter.Project.Application.app_name()

    if "--ignore-if-exists" in argv && Igniter.exists?(igniter, domain_file) do
      igniter
    else
      igniter
      |> Igniter.create_new_elixir_file(domain_file, """
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

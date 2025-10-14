# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
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
    def igniter(igniter) do
      domain = Igniter.Project.Module.parse(igniter.args.positional.domain)

      app_name = Igniter.Project.Application.app_name(igniter)
      {exists?, igniter} = Igniter.Project.Module.module_exists(igniter, domain)

      if "--ignore-if-exists" in igniter.args.argv_flags && exists? do
        igniter
      else
        igniter
        |> Igniter.Project.Module.create_module(domain, """
        use Ash.Domain,
          otp_app: :#{app_name}

        resources do
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
else
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

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.domain' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end

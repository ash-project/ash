defmodule Mix.Tasks.Ash.Gen.Enum do
  @example "mix ash.gen.enum MyApp.Support.Ticket.Types.Status open,closed --short-name ticket_status"
  @moduledoc """
  Generates an Ash.Type.Enum

  ## Example

  ```bash
  #{@example}
  ```

  ## Options

  - `--short-name`, `-s`: Register the type under the provided shortname, so it can be referenced like `:short_name` instead of the module name.
  """

  @shortdoc "Generates an Ash.Type.Enum"
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def info(_argv, _parent) do
    %Igniter.Mix.Task.Info{
      schema: [
        short_name: :string
      ],
      example: @example,
      positional: [:module_name, :types],
      aliases: [
        s: :short_name
      ]
    }
  end

  @impl Igniter.Mix.Task
  def igniter(igniter, argv) do
    {%{module_name: module_name, types: types}, argv} = positional_args!(argv)

    enum = Igniter.Code.Module.parse(module_name)
    file_name = Igniter.Project.Module.proper_location(igniter, enum)

    opts = options!(argv)

    short_name =
      if opts[:short_name] do
        String.to_atom(opts[:short_name])
      end

    types =
      types
      |> String.split(",")
      |> Enum.map(&String.to_atom/1)

    igniter
    |> Igniter.create_new_file(file_name, """
    defmodule #{inspect(enum)} do
      use Ash.Type.Enum, values: #{inspect(types)}
    end
    """)
    |> then(fn igniter ->
      if short_name do
        Igniter.Project.Config.configure(
          igniter,
          "config.exs",
          :ash,
          [:custom_types, short_name],
          enum
        )
      else
        igniter
      end
    end)
  end
end

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Enum do
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def igniter(igniter, [module_name, types | argv]) do
      enum = Igniter.Code.Module.parse(module_name)
      file_name = Igniter.Code.Module.proper_location(enum)

      {opts, _argv} =
        OptionParser.parse!(argv, switches: [short_name: :string], aliases: [s: :short_name])

      short_name =
        if opts[:short_name] do
          String.to_atom(opts[:short_name])
        end

      types =
        types
        |> String.split(",")
        |> Enum.map(&String.to_atom/1)

      igniter
      |> Igniter.create_new_elixir_file(file_name, """
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
end

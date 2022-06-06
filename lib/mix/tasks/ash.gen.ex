defmodule Mix.Tasks.Ash.Gen do
  @shortdoc "Generates a file for the requested DSL."
  @moduledoc @shortdoc
  use Mix.Task

  @spec run(term) :: no_return
  def run([name | args]) do
    dsl = get_dsl(name)

    module =
      quote do
        defmodule ExampleModuleName do
          use unquote(dsl)
        end
      end

    Sourceror.to_string(module)
  end

  defp get_dsl(name) do
    dsls = dsls()

    Enum.find(dsls, &(&1.module_info(:attributes)[:generator_options][:name] == name)) ||
      raise """
      Could not find dsl #{inspect(name)}.

      Available options:

      #{Enum.map_join(dsls, "\n", &"* #{inspect(&1, &1.module_info(:attributes)[:generator_options][:name])}")}
      """
  end

  defp dsls() do
    for [app] <- :ets.match(:ac_tab, {{:loaded, :"$1"}, :_}),
        {:ok, modules} = :application.get_key(app, :modules),
        module <- Enum.filter(modules, &ash_dsl?/1),
        Code.ensure_compiled!(module),
        module.module_info(:attributes)[:generator_options][:name] do
      module
    end
  end

  defp ash_dsl?(module) do
    case Code.ensure_compiled(module) do
      {:module, module} ->
        module.module_info(:attributes)[:behaviour] |> List.wrap() |> Enum.any?(&(&1 == Ash.Dsl))
    end
  end
end

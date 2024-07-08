defmodule Mix.Tasks.Ash.Patch.ExtractFunctions do
  @moduledoc """
  Interactively extracts anonymous functions from your resource actions into modules.

  ## Options

  - `--domain`, `-d` - Only process resources in the given domain.
  - `--resource`, `-r` - Only process the given resource.
  - `--action`, `-a` - Only process the given action. Ignored unless `--resource` is also set.
  """
  @shortdoc "Interactively extracts anonymous functions from your resource actions into modules."
  require Igniter.Code.Common
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def info(_argv, _) do
    %Igniter.Mix.Task.Info{
      schema: [
        domain: :string,
        resource: :string,
        action: :string
      ],
      aliases: [
        d: :domain,
        r: :resource,
        a: :action
      ]
    }
  end

  @impl Igniter.Mix.Task
  def igniter(igniter, argv) do
    options = options!(argv)

    {igniter, matching_modules} =
      Ash.Resource.Igniter.list_resources(igniter)

    matching_modules =
      if options[:resource] do
        Enum.filter(matching_modules, fn module ->
          inspect(module) == options[:resource]
        end)
      else
        matching_modules
      end

    Enum.reduce(matching_modules, igniter, fn module_name, igniter ->
      Igniter.Code.Module.find_and_update_module!(igniter, module_name, fn zipper ->
        if applies?(module_name, zipper, options) do
          extract_functions(module_name, zipper, options)
        else
          {:ok, zipper}
        end
      end)
    end)
  end

  defp extract_functions(module_name, zipper, options) do
    zipper
    |> Ash.Resource.Igniter.list_extractable_functions(zipper)
    |>
  end

  defp applies?(module_name, zipper, opts) do
    if opts[:domain] do
      domain = Igniter.Code.Module.parse(opts[:domain])

      with {:ok, zipper} <- Ash.Resource.Igniter.move_to_use_resource(zipper),
           {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
           {:ok, zipper} <- Igniter.Code.Keyword.get_key(zipper, :domain),
           true <- Igniter.Code.Common.nodes_equal?(zipper, domain) do
        true
      else
        _ ->
          false
      end
    else
      true
    end
  end
end

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Resources do
    @example """
    mix ash.gen.resources "Helpdesk.Support.Ticket --attribute subject:string:required --timestamps;Helpdesk.Support.Representative --uuid-primary-key id;Helpdesk.Support.Comment --attribute body:text --relationship belongs_to:ticket:Helpdesk.Support.Ticket"
    """
    @moduledoc """
    Generate and configure multiple Ash.Resource modules from a semicolon-separated list.

    This task takes a semicolon-separated list of resource names with their individual options
    and generates each one using the `ash.gen.resource` task.

    Each resource entry in the list can have its own specific options. The format is:
    `"ResourceName --option1 value1 --option2;AnotherResource --option3 value3"`

    ## Example

    ```bash
    #{@example}
    ```

    ## Resource Options

    Each resource supports all options from `mix ash.gen.resource`:

    * `--attribute` or `-a` - An attribute or comma separated list of attributes to add, as `name:type`. Modifiers: `primary_key`, `array`, `public`, `sensitive`, and `required`. i.e `-a name:string:required`
    * `--relationship` or `-r` - A relationship or comma separated list of relationships to add, as `type:name:dest`. Modifiers: `public`. `belongs_to` only modifiers: `primary_key`, `sensitive`, and `required`. i.e `-r belongs_to:author:MyApp.Accounts.Author:required`
    * `--default-actions` - A csv list of default action types to add. The `create` and `update` actions accept the public attributes being added.
    * `--uuid-primary-key` or `-u` - Adds a UUIDv4 primary key with that name. i.e `-u id`
    * `--uuid-v7-primary-key` - Adds a UUIDv7 primary key with that name.
    * `--integer-primary-key` or `-i` - Adds an integer primary key with that name. i.e `-i id`
    * `--domain` or `-d` - The domain module to add the resource to. i.e `-d MyApp.MyDomain`. This defaults to the resource's module name, minus the last segment.
    * `--extend` or `-e` - A comma separated list of modules or builtins to extend the resource with. i.e `-e postgres,Some.Extension`
    * `--base` or `-b` - The base module to use for the resource. i.e `-b Ash.Resource`. Requires that the module is in `config :your_app, :base_resources`
    * `--timestamps` or `-t` - If set adds `inserted_at` and `updated_at` timestamps to the resource.
    * `--ignore-if-exists` - Does nothing if the resource already exists
    * `--conflicts` - How to handle conflicts when the same attribute, relationship, or action already exists. Options: `ignore` (default), `replace`
       `ignore` will ignore your addition for that attribute, relationship, or action. `replace` will remove the existing one in favor of yours.
    """

    @shortdoc "Generate and configure multiple Ash.Resource modules from a semicolon-separated list."
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [:resources],
        example: @example,
        schema: [],
        aliases: []
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      igniter.args.positional.resources
      |> String.split(";", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.reduce(igniter, fn resource_entry, acc_igniter ->
        [resource | resource_options] = String.split(resource_entry, ~r/\s+/, trim: true)
        Igniter.compose_task(acc_igniter, "ash.gen.resource", [resource] ++ resource_options)
      end)
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Resources do
    @moduledoc """
    Generate and configure multiple Ash.Resource modules from a semicolon-separated list.
    """

    @shortdoc "Generate and configure multiple Ash.Resource modules from a semicolon-separated list."

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.resources' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end

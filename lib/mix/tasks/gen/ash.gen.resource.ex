defmodule Mix.Tasks.Ash.Gen.Resource do
  @example """
  mix ash.gen.resource Helpdesk.Support.Ticket \\
    --default-actions read \\
    --uuid-primary-key id \\
    --attribute subject:string:required:public \\
    --relationship belongs_to:representative:Helpdesk.Support.Representative \\
    --timestamps \\
    --extend postgres,graphql
  """
  @moduledoc """
  Generate and configure an Ash.Resource.

  If the domain does not exist, we create it. If it does, we add the resource to it if it is not already present.

  ## Example

  ```bash
  #{@example}
  ```

  ## Options

  * `--attribute` or `-a` - An attribute or comma separated list of attributes to add, as `name:type`. Modifiers: `primary_key`, `public`, `sensitive`, and `required`. i.e `-a name:string:required`
  * `--relationship` or `-r` - A relationship or comma separated list of relationships to add, as `type:name:dest`. Modifiers: `public`. `belongs_to` only modifiers: `primary_key`, `sensitive`, and `required`. i.e `-r belongs_to:author:MyApp.Accounts.Author:required`
  * `--default-actions` or `-da` - A csv list of default action types to add, i.e `-da read,create`. The `create` and `update` actions accept the public attributes being added.
  * `--uuid-primary-key` or `-u` - Adds a UUIDv4 primary key with that name. i.e `-u id`
  * `--uuid-v7-primary-key` or `-u7` - Adds a UUIDv7 primary key with that name. i.e `-u7 id`
  * `--integer-primary-key` or `-i` - Adds an integer primary key with that name. i.e `-i id`
  * `--domain` or `-d` - The domain module to add the resource to. i.e `-d MyApp.MyDomain`. This defaults to the resource's module name, minus the last segment.
  * `--extend` or `-e` - A comma separated list of modules or builtins to extend the resource with. i.e `-e postgres,Some.Extension`
  * `--base` or `-b` - The base module to use for the resource. i.e `-b Ash.Resource`. Requires that the module is in `config :your_app, :base_resources`
  * `--timestamps` or `-t` - If set adds `inserted_at` and `updated_at` timestamps to the resource.
  """

  @shortdoc "Generate and configure an Ash.Resource."
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def info(_argv, _parent) do
    %Igniter.Mix.Task.Info{
      positional: [:resource],
      example: @example,
      schema: [
        attribute: :csv,
        relationship: :csv,
        default_actions: :csv,
        uuid_primary_key: :string,
        uuid_v7_primary_key: :string,
        integer_primary_key: :string,
        domain: :string,
        extend: :csv,
        base: :string,
        timestamps: :boolean
      ],
      aliases: [
        a: :attribute,
        r: :relationship,
        da: :default_actions,
        d: :domain,
        u7: :uuid_v7_primary_key,
        u: :uuid_primary_key,
        i: :integer_primary_key,
        e: :extend,
        b: :base,
        t: :timestamps
      ]
    }
  end

  @impl Igniter.Mix.Task
  def igniter(igniter, argv) do
    {%{resource: resource}, argv} = positional_args!(argv)
    resource = Igniter.Code.Module.parse(resource)
    app_name = Igniter.Project.Application.app_name(igniter)

    options = options!(argv)

    domain =
      case options[:domain] do
        nil ->
          resource
          |> Module.split()
          |> :lists.droplast()
          |> Module.concat()

        domain ->
          Igniter.Code.Module.parse(domain)
      end

    options =
      options
      |> Keyword.update(
        :default_actions,
        [],
        fn defaults -> Enum.sort_by(defaults, &(&1 in ["create", "update"])) end
      )
      |> Keyword.put_new(:base, "Ash.Resource")

    base =
      if options[:base] == "Ash.Resource" do
        "Ash.Resource"
      else
        base =
          Igniter.Code.Module.parse(options[:base])

        unless base in List.wrap(Application.get_env(app_name, :base_resources)) do
          raise """
          The base module #{inspect(base)} is not in the list of base resources.

          If it exists but is not in the base resource list, add it like so:

          `config #{inspect(app_name)}, base_resources: [#{inspect(base)}]`

          If it does not exist, you can generate a base resource with `mix ash.gen.base_resource #{inspect(base)}`
          """
        end

        inspect(base)
      end

    attributes = attributes(options)

    relationships =
      if !Enum.empty?(options[:relationship]) do
        """
        relationships do
        #{relationships(options)}
        end
        """
      end

    default_accept =
      Enum.flat_map(options[:attribute], fn attribute ->
        [name, _type | modifiers] = String.split(attribute, ":", trim: true)

        if "public" in modifiers do
          [String.to_atom(name)]
        else
          []
        end
      end)

    actions =
      case options[:default_actions] do
        [] ->
          ""

        defaults ->
          default_contents =
            Enum.map_join(defaults, ", ", fn
              type when type in ["read", "destroy"] ->
                ":#{type}"

              type when type in ["create", "update"] ->
                "#{type}: #{inspect(default_accept)}"

              type ->
                raise """
                Invalid default action type given to `--default-actions`: #{inspect(type)}.
                """
            end)

          """
          actions do
            defaults [#{default_contents}]
          end
          """
      end

    attributes =
      if options[:uuid_primary_key] || options[:integer_primary_key] ||
           options[:uuid_v7_primary_key] ||
           !Enum.empty?(options[:attribute]) || options[:timestamps] do
        uuid_primary_key =
          if options[:uuid_primary_key] do
            pkey_builder("uuid_primary_key", options[:uuid_primary_key])
          end

        uuid_v7_primary_key =
          if options[:uuid_v7_primary_key] do
            pkey_builder("uuid_v7_primary_key", options[:uuid_v7_primary_key])
          end

        integer_primary_key =
          if options[:integer_primary_key] do
            pkey_builder("integer_primary_key", options[:integer_primary_key])
          end

        timestamps =
          if options[:timestamps] do
            "timestamps()"
          end

        """
        attributes do
          #{uuid_primary_key}
          #{uuid_v7_primary_key}
          #{integer_primary_key}
          #{attributes}
          #{timestamps}
        end
        """
      end

    igniter
    |> Igniter.compose_task("ash.gen.domain", [inspect(domain), "--ignore-if-exists"])
    |> Ash.Domain.Igniter.add_resource_reference(
      domain,
      resource
    )
    |> Igniter.Project.Module.create_module(
      resource,
      """
      use #{base},
        otp_app: #{inspect(app_name)},
        domain: #{inspect(domain)}

      #{actions}

      #{attributes}

      #{relationships}
      """
    )
    |> extend(resource, options[:extend], argv)
  end

  defp extend(igniter, _, [], _) do
    igniter
  end

  defp extend(igniter, resource, extensions, argv) do
    Igniter.compose_task(
      igniter,
      "ash.patch.extend",
      [inspect(resource), Enum.join(extensions, ",")] ++ argv
    )
  end

  defp pkey_builder(builder, text) do
    [name | modifiers] = String.split(text, ":", trim: true)
    modifiers = modifiers -- ["primary_key"]

    if Enum.empty?(modifiers) do
      "#{builder} :#{name}"
    else
      """
      #{builder} :#{name} do
        #{attribute_modifier_string(modifiers)}
      end
      """
    end
  end

  defp attributes(options) do
    options[:attribute]
    |> List.wrap()
    |> Enum.join(",")
    |> String.split(",", trim: true)
    |> Enum.map(fn attribute ->
      case String.split(attribute, ":") do
        [name, type | modifiers] ->
          {name, type, modifiers}

        _name ->
          raise """
          Invalid attribute format: #{attribute}. Please use the format `name:type` for each attribute.
          """
      end
    end)
    |> Enum.map_join("\n", fn
      {name, type, []} ->
        type = resolve_type(type)

        "attribute :#{name}, #{inspect(type)}"

      {name, type, modifiers} ->
        modifier_string = attribute_modifier_string(modifiers)

        type = resolve_type(type)

        """
        attribute :#{name}, #{inspect(type)} do
          #{modifier_string}
        end
        """
    end)
  end

  defp attribute_modifier_string(modifiers) do
    Enum.map_join(modifiers, "\n", fn
      "primary_key" ->
        "primary_key? true"

      "public" ->
        "public? true"

      "required" ->
        "allow_nil? false"

      "sensitive" ->
        "sensitive? true"
    end)
  end

  defp relationships(options) do
    options[:relationship]
    |> List.wrap()
    |> Enum.join(",")
    |> String.split(",")
    |> Enum.map(fn relationship ->
      case String.split(relationship, ":") do
        [type, name, destination | modifiers] ->
          {type, name, destination, modifiers}

        _name ->
          raise """
          Invalid relationship format: #{relationship}. Please use the format `type:name:destination` for each attribute.
          """
      end
    end)
    |> Enum.map_join("\n", fn
      {type, name, destination, []} ->
        "#{type} :#{name}, #{destination}"

      {type, name, destination, modifiers} ->
        modifier_string =
          Enum.map_join(modifiers, "\n", fn
            "primary_key" ->
              if type == "belongs_to" do
                "primary_key? true"
              else
                raise ArgumentError,
                      "The @ modifier (for `primary_key?: true`) is only valid for belongs_to relationships, saw it in `#{type}:#{name}`"
              end

            "public" ->
              "public? true"

            "sensitive?" ->
              "sensitive? true"

            "required" ->
              if type == "belongs_to" do
                "allow_nil? false"
              else
                raise ArgumentError,
                      "The ! modifier (for `allow_nil?: false`) is only valid for belongs_to relationships, saw it in `#{type}:#{name}`"
              end
          end)

        """
        #{type} :#{name}, #{destination} do
          #{modifier_string}
        end
        """
    end)
  end

  defp resolve_type(value) do
    if String.contains?(value, ".") do
      Module.concat([value])
    else
      String.to_atom(value)
    end
  end
end

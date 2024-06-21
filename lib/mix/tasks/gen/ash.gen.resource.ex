defmodule Mix.Tasks.Ash.Gen.Resource do
  @moduledoc """
  Generate and configures an Ash.Resource.

  ## What changes take place?

  If the domain does not exist, we create it. If it does, we add the resource to it if it is not already present.

  ## Options

  * `--attribute` or `-a` - An attribute or comma separated list of attributes to add, as `name:type`. Modifiers: `primary_key`, `public`, and `required`. i.e `-a name:string:required`
  * `--relationship` or `-r` - A relationship or comma separated list of relationships to add, as `type:name:dest`. Modifiers: `public`. `belongs_to` only modifiers: `primary_key`, and `required`. i.e `-r belongs_to:author:MyApp.Accounts.Author:required`
  * `--default-actions` or `-da` - A csv list of default action types to add, i.e `-da read,create`. The `create` and `update` actions accept the public attributes being added.
  * `--uuid-primary-key` or `-u` - Adds a UUID primary key with that name. i.e `-u id`
  * `--integer-primary-key` or `-i` - Adds an integer primary key with that name. i.e `-i id`
  * `--domain` or `-d` - The domain module to add the resource to. i.e `-d MyApp.MyDomain`. This defaults to the resource's module name, minus the last segment.
  * `--extend` or `-e` - A comma separated list of modules or builtins to extend the resource with. i.e `-e postgres,Some.Extension`
  * `--base` or `-b` - The base module to use for the resource. i.e `-b Ash.Resource`. Requires that the module is in `config :your_app, :base_resources`
  """

  @shortdoc "Generate an Ash.Resource."
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def igniter(igniter, [resource | argv]) do
    resource = Igniter.Code.Module.parse(resource)
    app_name = Igniter.Project.Application.app_name()

    {options, _, _} =
      OptionParser.parse(argv,
        strict: [
          attribute: :keep,
          relationship: :keep,
          default_actions: :keep,
          uuid_primary_key: :string,
          integer_primary_key: :string,
          domain: :string,
          extend: :keep,
          base: :string
        ],
        aliases: [
          a: :attribute,
          r: :relationship,
          da: :default_actions,
          d: :domain,
          u: :uuid_primary_key,
          i: :integer_primary_key,
          e: :extend,
          b: :base
        ]
      )

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
      |> Ash.Igniter.csv_option(:default_actions, fn values ->
        Enum.sort_by(values, &(&1 in ["create", "update"]))
      end)
      |> Ash.Igniter.csv_option(:attribute)
      |> Ash.Igniter.csv_option(:relationship)
      |> Ash.Igniter.csv_option(:extend)
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
           !Enum.empty?(options[:attribute]) do
        uuid_primary_key =
          if options[:uuid_primary_key] do
            pkey_builder("uuid_primary_key", options[:uuid_primary_key])
          end

        integer_primary_key =
          if options[:integer_primary_key] do
            pkey_builder("integer_primary_key", options[:integer_primary_key])
          end

        """
        attributes do
          #{uuid_primary_key}
          #{integer_primary_key}
          #{attributes}
        end
        """
      end

    igniter
    |> Igniter.compose_task("ash.gen.domain", [inspect(domain), "--ignore-if-exists"])
    |> Ash.Domain.Igniter.add_resource_reference(
      domain,
      resource
    )
    |> Igniter.create_new_elixir_file(
      Igniter.Code.Module.proper_location(resource),
      """
      defmodule #{inspect(resource)} do
        use #{base},
          otp_app: #{inspect(app_name)},
          domain: #{inspect(domain)}

        #{actions}

        #{attributes}

        #{relationships}
      end
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
      "ash.extend",
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
    |> Enum.map(fn
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
    |> Enum.join("\n")
  end

  defp attribute_modifier_string(modifiers) do
    Enum.map_join(modifiers, "\n", fn
      "primary_key" ->
        "primary_key? true"

      "public" ->
        "public? true"

      "required" ->
        "allow_nil? false"
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
          Invalid attribute format. Please use the format `type:name:destination` for each attribute.
          """
      end
    end)
    |> Enum.map(fn
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
    |> Enum.join("\n")
  end

  defp resolve_type(value) do
    if String.contains?(value, ".") do
      Module.concat([value])
    else
      String.to_atom(value)
    end
  end
end

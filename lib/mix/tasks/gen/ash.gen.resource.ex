if Code.ensure_loaded?(Igniter) do
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
    * `--default-actions` - A csv list of default action types to add. The `create` and `update` actions accept the public attributes being added.
    * `--uuid-primary-key` or `-u` - Adds a UUIDv4 primary key with that name. i.e `-u id`
    * `--uuid-v7-primary-key` - Adds a UUIDv7 primary key with that name.
    * `--integer-primary-key` or `-i` - Adds an integer primary key with that name. i.e `-i id`
    * `--domain` or `-d` - The domain module to add the resource to. i.e `-d MyApp.MyDomain`. This defaults to the resource's module name, minus the last segment.
    * `--extend` or `-e` - A comma separated list of modules or builtins to extend the resource with. i.e `-e postgres,Some.Extension`
    * `--base` or `-b` - The base module to use for the resource. i.e `-b Ash.Resource`. Requires that the module is in `config :your_app, :base_resources`
    * `--timestamps` or `-t` - If set adds `inserted_at` and `updated_at` timestamps to the resource.
    * `--ignore-if-exists` - Does nothing if the resource already exists
    """

    @shortdoc "Generate and configure an Ash.Resource."
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(argv, _parent) do
      for {key, cmd} <- [da: "--default-actions", u7: "--uuid-v7-primary-key"] do
        if "-#{key}" in argv do
          Mix.shell().error("""
            The `-#{key}` alias has been removed as multi-char aliases are deprecated in OptionParser.
            Please use `--#{cmd}` instead.
          """)

          Mix.shell().exit({:shutdown, 1})
        end
      end

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
          timestamps: :boolean,
          da: :string,
          u7: :string,
          ignore_if_exists: :boolean
        ],
        aliases: [
          a: :attribute,
          r: :relationship,
          d: :domain,
          u: :uuid_primary_key,
          i: :integer_primary_key,
          e: :extend,
          b: :base,
          t: :timestamps
        ]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      arguments = igniter.args.positional
      options = igniter.args.options
      argv = igniter.args.argv_flags

      resource = Igniter.Project.Module.parse(arguments.resource)
      app_name = Igniter.Project.Application.app_name(igniter)

      {exists?, igniter} = Igniter.Project.Module.module_exists(igniter, resource)

      if "--ignore-if-exists" in igniter.args.argv_flags && exists? do
        igniter
      else
        domain =
          case options[:domain] do
            nil ->
              resource
              |> Module.split()
              |> :lists.droplast()
              |> Module.concat()

            domain ->
              Igniter.Project.Module.parse(domain)
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
              Igniter.Project.Module.parse(options[:base])

            if base not in List.wrap(Application.get_env(app_name, :base_resources)) do
              raise """
              The base module #{inspect(base)} is not in the list of base resources.

              If it exists but is not in the base resource list, add it like so:

              `config #{inspect(app_name)}, base_resources: [#{inspect(base)}]`

              If it does not exist, you can generate a base resource with `mix ash.gen.base_resource #{inspect(base)}`
              """
            end

            inspect(base)
          end

        default_accept =
          Enum.flat_map(options[:attribute] || [], fn attribute ->
            case String.split(attribute, ":", trim: true) do
              [name, _type | modifiers] ->
                if "public" in modifiers do
                  [String.to_atom(name)]
                else
                  []
                end

              _ ->
                raise """
                Invalid attribute format: #{attribute}. Please use the format `name:type` for each attribute.
                """
            end
          end)

        igniter
        |> Igniter.compose_task("ash.gen.domain", [inspect(domain), "--ignore-if-exists"])
        |> Ash.Domain.Igniter.add_resource_reference(
          domain,
          resource
        )
        |> ensure_resource_exists(resource, base, app_name, domain)
        |> add_actions_to_resource(resource, options, default_accept)
        |> add_attributes_to_resource(resource, options)
        |> add_relationships_to_resource(resource, options)
        |> extend(resource, options[:extend], argv)
      end
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

    defp valid_attribute_name?(name) do
      Regex.match?(~r/^[a-zA-Z][a-zA-Z0-9_]*[!?]?$/, name)
    end

    defp attribute_modifier_string(modifiers) do
      modifiers
      |> Enum.uniq()
      |> Enum.map_join("\n", fn
        "primary_key" ->
          "primary_key? true"

        "public" ->
          "public? true"

        "required" ->
          "allow_nil? false"

        "sensitive" ->
          "sensitive? true"

        unknown ->
          raise ArgumentError,
                """
                Unrecognizeable attribute modifier: `#{unknown}`.

                Known modifiers are: primary_key, public, required, sensitive.
                """
      end)
    end

    defp resolve_type(value) do
      # sadly, we can't validate the type here
      # because the type may be being generated
      # as part of the task that generates this resource
      if String.contains?(value, ".") do
        Module.concat([value])
      else
        String.to_atom(value)
      end
    end

    defp ensure_resource_exists(igniter, resource, base, app_name, domain) do
      case Igniter.Project.Module.find_module(igniter, resource) do
        {:ok, {igniter, _source, _zipper}} ->
          # Resource already exists, don't recreate it
          igniter

        {:error, igniter} ->
          # Resource doesn't exist, create it with basic structure
          Igniter.Project.Module.create_module(
            igniter,
            resource,
            """
            use #{base},
              otp_app: #{inspect(app_name)},
              domain: #{inspect(domain)}
            """
          )
      end
    end

    defp add_attributes_to_resource(igniter, resource, options) do
      igniter
      |> add_primary_key_to_resource(resource, options)
      |> add_regular_attributes_to_resource(resource, options)
      |> add_timestamps_to_resource(resource, options)
    end

    defp add_primary_key_to_resource(igniter, resource, options) do
      cond do
        options[:uuid_primary_key] ->
          add_primary_key_attribute(
            igniter,
            resource,
            "uuid_primary_key",
            options[:uuid_primary_key]
          )

        options[:uuid_v7_primary_key] ->
          add_primary_key_attribute(
            igniter,
            resource,
            "uuid_v7_primary_key",
            options[:uuid_v7_primary_key]
          )

        options[:integer_primary_key] ->
          add_primary_key_attribute(
            igniter,
            resource,
            "integer_primary_key",
            options[:integer_primary_key]
          )

        true ->
          igniter
      end
    end

    defp add_primary_key_attribute(igniter, resource, builder, text) do
      [name | modifiers] = String.split(text, ":", trim: true)
      modifiers = modifiers -- ["primary_key"]
      name_atom = String.to_atom(name)

      if !valid_attribute_name?(name) do
        raise "Invalid attribute name provided for `#{builder}`: #{name}"
      end

      if Enum.empty?(modifiers) do
        Ash.Resource.Igniter.add_new_attribute(
          igniter,
          resource,
          name_atom,
          "#{builder} :#{name}"
        )
      else
        attribute_with_modifiers = """
        #{builder} :#{name} do
          #{attribute_modifier_string(modifiers)}
        end
        """

        Ash.Resource.Igniter.add_new_attribute(
          igniter,
          resource,
          name_atom,
          attribute_with_modifiers
        )
      end
    end

    defp add_regular_attributes_to_resource(igniter, resource, options) do
      Enum.reduce(options[:attribute] || [], igniter, fn attribute, igniter ->
        case String.split(attribute, ":") do
          [name, type | modifiers] ->
            if !valid_attribute_name?(name) do
              raise "Invalid attribute name provided: #{name}"
            end

            name_atom = String.to_atom(name)
            type = resolve_type(type)

            attribute_code =
              if Enum.empty?(modifiers) do
                "attribute :#{name}, #{inspect(type)}"
              else
                modifier_string = attribute_modifier_string(modifiers)

                """
                attribute :#{name}, #{inspect(type)} do
                  #{modifier_string}
                end
                """
              end

            Ash.Resource.Igniter.add_new_attribute(igniter, resource, name_atom, attribute_code)

          _name ->
            raise """
            Invalid attribute format: #{attribute}. Please use the format `name:type` for each attribute.
            """
        end
      end)
    end

    defp add_timestamps_to_resource(igniter, resource, options) do
      if options[:timestamps] do
        # Check if timestamps() call already exists in attributes section
        case find_existing_timestamps_call(igniter, resource) do
          {:ok, igniter} ->
            # Already exists, don't add again
            igniter

          {:error, igniter} ->
            # Also check for manual inserted_at/updated_at definitions
            {igniter, has_timestamps?} =
              Ash.Resource.Igniter.defines_attribute(igniter, resource, :inserted_at)

            if has_timestamps? do
              igniter
            else
              Ash.Resource.Igniter.add_attribute(igniter, resource, "timestamps()")
            end
        end
      else
        igniter
      end
    end

    defp add_relationships_to_resource(igniter, resource, options) do
      Enum.reduce(options[:relationship] || [], igniter, fn relationship, igniter ->
        case String.split(relationship, ":") do
          [type, name, destination | modifiers] ->
            if !valid_attribute_name?(name) do
              raise "Invalid relationship name provided: #{name}"
            end

            name_atom = String.to_atom(name)

            relationship_code =
              if Enum.empty?(modifiers) do
                "#{type} :#{name}, #{destination}"
              else
                modifier_string =
                  Enum.map_join(modifiers, "\n", fn
                    "primary_key" ->
                      if type == "belongs_to" do
                        "primary_key? true"
                      else
                        raise ArgumentError,
                              "The primary_key modifier is only valid for belongs_to relationships, saw it in `#{type}:#{name}`"
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
              end

            Ash.Resource.Igniter.add_new_relationship(
              igniter,
              resource,
              name_atom,
              relationship_code
            )

          _name ->
            raise """
            Invalid relationship format: #{relationship}. Please use the format `type:name:destination` for each attribute.
            """
        end
      end)
    end

    defp add_actions_to_resource(igniter, resource, options, default_accept) do
      case options[:default_actions] do
        [] ->
          igniter

        defaults ->
          # Check if defaults call already exists
          case find_existing_defaults_call(igniter, resource) do
            {:ok, igniter} ->
              # Merge with existing defaults
              merge_defaults_with_existing(igniter, resource, defaults, default_accept)

            {:error, igniter} ->
              # Add new defaults call
              add_new_defaults_call(igniter, resource, defaults, default_accept)
          end
      end
    end

    defp find_existing_defaults_call(igniter, resource) do
      Spark.Igniter.find(igniter, resource, fn _, zipper ->
        with {:ok, zipper} <- enter_section(zipper, :actions),
             {:ok, _zipper} <-
               Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :defaults,
                 1
               ) do
          {:ok, true}
        else
          _ -> :error
        end
      end)
      |> case do
        {:ok, igniter, _module, _value} ->
          {:ok, igniter}

        {:error, igniter} ->
          {:error, igniter}
      end
    end

    defp find_existing_timestamps_call(igniter, resource) do
      Spark.Igniter.find(igniter, resource, fn _, zipper ->
        with {:ok, zipper} <- enter_section(zipper, :attributes),
             {:ok, _zipper} <-
               Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :timestamps,
                 [0, 1]
               ) do
          {:ok, true}
        else
          _ -> :error
        end
      end)
      |> case do
        {:ok, igniter, _module, _value} ->
          {:ok, igniter}

        {:error, igniter} ->
          {:error, igniter}
      end
    end

    defp merge_defaults_with_existing(igniter, resource, new_defaults, default_accept) do
      Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
        with {:ok, zipper} <- enter_section(zipper, :actions),
             {:ok, zipper} <-
               Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :defaults,
                 1
               ),
             {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 0) do
          # Add each new default to the list, avoiding duplicates
          updated_zipper =
            Enum.reduce(new_defaults, zipper, fn action_type, zipper ->
              action_atom = String.to_atom(action_type)

              case action_type do
                type when type in ["read", "destroy"] ->
                  # For simple actions, add atom if not already present
                  case Igniter.Code.List.append_new_to_list(zipper, action_atom, fn zipper ->
                         Igniter.Code.Common.nodes_equal?(zipper, action_atom)
                       end) do
                    {:ok, zipper} -> zipper
                    :error -> zipper
                  end

                type when type in ["create", "update"] ->
                  # For actions with accept lists, add or update keyword entry
                  new_item = {action_atom, default_accept}

                  # Remove existing entry if present, then add new one
                  zipper =
                    case Igniter.Code.List.remove_from_list(zipper, fn item_zipper ->
                           # Check if this is a tuple with the same action type
                           if Igniter.Code.Tuple.tuple?(item_zipper) do
                             case Igniter.Code.Tuple.tuple_elem(item_zipper, 0) do
                               {:ok, first_elem_zipper} ->
                                 Igniter.Code.Common.nodes_equal?(first_elem_zipper, action_atom)

                               _ ->
                                 false
                             end
                           else
                             false
                           end
                         end) do
                      {:ok, zipper} -> zipper
                      :error -> zipper
                    end

                  # Add the new item
                  case Igniter.Code.List.append_to_list(zipper, new_item) do
                    {:ok, zipper} -> zipper
                    :error -> zipper
                  end

                type ->
                  raise """
                  Invalid default action type given to `--default-actions`: #{inspect(type)}.
                  """
              end
            end)

          {:ok, updated_zipper}
        else
          _ ->
            {:error, "Failed to find or update defaults call"}
        end
      end)
    end

    defp add_new_defaults_call(igniter, resource, defaults, default_accept) do
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

      actions_code = "defaults [#{default_contents}]"
      Ash.Resource.Igniter.add_block(igniter, resource, :actions, actions_code)
    end

    defp enter_section(zipper, name) do
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               name,
               1
             ) do
        Igniter.Code.Common.move_to_do_block(zipper)
      end
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Resource do
    @moduledoc """
    Generate and configure an Ash.Resource.
    """

    @shortdoc "Generate and configure an Ash.Resource."

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.resource' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end

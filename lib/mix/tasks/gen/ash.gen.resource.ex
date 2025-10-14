# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

    * `--attribute` or `-a` - An attribute or comma separated list of attributes to add, as `name:type`. Modifiers: `primary_key`, `array`, `public`, `sensitive`, and `required`. i.e `-a name:string:required`
    * `--relationship` or `-r` - A relationship or comma separated list of relationships to add, as `type:name:dest`. Modifiers: `public` and `sensitive?`. `belongs_to` only modifiers: `primary_key` and `required`. i.e `-r belongs_to:author:MyApp.Accounts.Author:required`. For many_to_many relationship the through relationship is required between name and destination, i.e. `-r many_to_many:posts:MyApp.Blog.PostComment:MyApp.Blog.Comment:public`
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
          ignore_if_exists: :boolean,
          conflicts: :string
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

        # Validate conflicts option
        conflicts_strategy =
          case options[:conflicts] do
            nil ->
              "ignore"

            strategy when strategy in ["ignore", "replace"] ->
              strategy

            invalid ->
              raise """
              Invalid value for --conflicts: #{inspect(invalid)}

              Valid options are: ignore, replace
              """
          end

        options =
          options
          |> Keyword.update(
            :default_actions,
            [],
            fn defaults -> Enum.sort_by(defaults, &(&1 in ["create", "update"])) end
          )
          |> Keyword.put_new(:base, "Ash.Resource")
          |> Keyword.put(:conflicts, conflicts_strategy)

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
        |> add_attributes_to_resource(resource, options)
        |> add_relationships_to_resource(resource, options)
        |> add_actions_to_resource(resource, options, default_accept)
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

        "array" ->
          "array"

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
            options[:uuid_primary_key],
            options
          )

        options[:uuid_v7_primary_key] ->
          add_primary_key_attribute(
            igniter,
            resource,
            "uuid_v7_primary_key",
            options[:uuid_v7_primary_key],
            options
          )

        options[:integer_primary_key] ->
          add_primary_key_attribute(
            igniter,
            resource,
            "integer_primary_key",
            options[:integer_primary_key],
            options
          )

        true ->
          igniter
      end
    end

    defp add_primary_key_attribute(igniter, resource, builder, text, options) do
      [name | modifiers] = String.split(text, ":", trim: true)
      modifiers = modifiers -- ["primary_key"]
      name_atom = String.to_atom(name)

      if !valid_attribute_name?(name) do
        raise "Invalid attribute name provided for `#{builder}`: #{name}"
      end

      attribute_code =
        if Enum.empty?(modifiers) do
          "#{builder} :#{name}"
        else
          """
          #{builder} :#{name} do
            #{attribute_modifier_string(modifiers)}
          end
          """
        end

      add_attribute_with_conflicts(
        igniter,
        resource,
        name_atom,
        attribute_code,
        options[:conflicts]
      )
    end

    defp add_regular_attributes_to_resource(igniter, resource, options) do
      Enum.reduce(options[:attribute] || [], igniter, fn attribute, igniter ->
        case String.split(attribute, ":") do
          [name, type | modifiers] ->
            if !valid_attribute_name?(name) do
              raise "Invalid attribute name provided: #{name}"
            end

            name_atom = String.to_atom(name)

            {type, modifiers} =
              if Enum.any?(modifiers, &(&1 == "array")),
                do: {{:array, resolve_type(type)}, modifiers -- ["array"]},
                else: {resolve_type(type), modifiers}

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

            add_attribute_with_conflicts(
              igniter,
              resource,
              name_atom,
              attribute_code,
              options[:conflicts]
            )

          _name ->
            raise """
            Invalid attribute format: #{attribute}. Please use the format `name:type` for each attribute.
            """
        end
      end)
    end

    defp add_timestamps_to_resource(igniter, resource, options) do
      if options[:timestamps] do
        case options[:conflicts] do
          "ignore" ->
            # Only add if no existing timestamps
            case find_existing_timestamps_call(igniter, resource) do
              # Already exists, ignore
              {:ok, igniter} ->
                igniter

              {:error, igniter} ->
                {igniter, has_timestamps?} =
                  Ash.Resource.Igniter.defines_attribute(igniter, resource, :inserted_at)

                if has_timestamps? do
                  igniter
                else
                  Ash.Resource.Igniter.add_attribute(igniter, resource, "timestamps()")
                end
            end

          "replace" ->
            # Remove existing and add new
            igniter
            |> remove_existing_timestamps(resource)
            |> Ash.Resource.Igniter.add_attribute(resource, "timestamps()")
        end
      else
        igniter
      end
    end

    defp add_relationships_to_resource(igniter, resource, options) do
      Enum.reduce(options[:relationship] || [], igniter, fn relationship, igniter ->
        case String.split(relationship, ":") do
          ["many_to_many", name, through, destination | modifiers] ->
            if !valid_attribute_name?(name) do
              raise "Invalid relationship name provided: #{name}"
            end

            name_atom = String.to_atom(name)

            relationship_code =
              if Enum.empty?(modifiers) do
                "many_to_many :#{name}, #{destination} do
                  through(#{through})
                end"
              else
                modifier_string =
                  Enum.map_join(modifiers, "\n", fn
                    "public" ->
                      "public? true"

                    "sensitive?" ->
                      "sensitive? true"

                    invalid_modifier ->
                      raise ArgumentError,
                            "Invalid modifier `#{invalid_modifier}` for many_to_many relationship, valid modifiers are `public` and `sensitive`"
                  end)

                """
                many_to_many :#{name}, #{destination} do
                  through(#{through})
                  #{modifier_string}
                end
                """
              end

            add_relationship_with_conflicts(
              igniter,
              resource,
              name_atom,
              relationship_code,
              options[:conflicts]
            )

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

            add_relationship_with_conflicts(
              igniter,
              resource,
              name_atom,
              relationship_code,
              options[:conflicts]
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
          case options[:conflicts] do
            "ignore" ->
              # Only add if no existing defaults call
              case find_existing_defaults_call(igniter, resource) do
                # Ignore if exists
                {:ok, igniter} ->
                  igniter

                {:error, igniter} ->
                  add_new_defaults_call(igniter, resource, defaults, default_accept)
              end

            "replace" ->
              # Remove existing and add new
              igniter
              |> remove_existing_defaults(resource)
              |> add_new_defaults_call(resource, defaults, default_accept)
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

    defp remove_existing_defaults(igniter, resource) do
      Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
        with {:ok, zipper} <- enter_section(zipper, :actions),
             {:ok, zipper} <-
               Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :defaults,
                 1
               ) do
          {:ok, Sourceror.Zipper.remove(zipper)}
        else
          _ -> {:ok, zipper}
        end
      end)
    end

    defp remove_existing_timestamps(igniter, resource) do
      Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
        with {:ok, zipper} <- enter_section(zipper, :attributes),
             {:ok, zipper} <-
               Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :timestamps,
                 [0, 1]
               ) do
          {:ok, Sourceror.Zipper.remove(zipper)}
        else
          _ -> {:ok, zipper}
        end
      end)
    end

    # Conflict-aware attribute handling
    defp add_attribute_with_conflicts(
           igniter,
           resource,
           name_atom,
           attribute_code,
           conflicts_strategy
         ) do
      case conflicts_strategy do
        "ignore" ->
          Ash.Resource.Igniter.add_new_attribute(igniter, resource, name_atom, attribute_code)

        "replace" ->
          igniter
          |> remove_existing_attribute(resource, name_atom)
          |> Ash.Resource.Igniter.add_attribute(resource, attribute_code)
      end
    end

    defp remove_existing_attribute(igniter, resource, name_atom) do
      Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
        with {:ok, zipper} <- enter_section(zipper, :attributes),
             {:ok, zipper} <- find_and_remove_attribute(zipper, name_atom) do
          {:ok, zipper}
        else
          _ -> {:ok, zipper}
        end
      end)
    end

    defp find_and_remove_attribute(zipper, name_atom) do
      case Igniter.Code.Function.move_to_function_call_in_current_scope(
             zipper,
             :attribute,
             [2, 3],
             &Igniter.Code.Function.argument_equals?(&1, 0, name_atom)
           ) do
        {:ok, zipper} ->
          {:ok, Sourceror.Zipper.remove(zipper)}

        :error ->
          # Try primary key functions
          primary_key_functions = [:uuid_primary_key, :uuid_v7_primary_key, :integer_primary_key]

          Enum.reduce_while(primary_key_functions, :error, fn function_name, _acc ->
            case Igniter.Code.Function.move_to_function_call_in_current_scope(
                   zipper,
                   function_name,
                   [1, 2],
                   &Igniter.Code.Function.argument_equals?(&1, 0, name_atom)
                 ) do
              {:ok, zipper} ->
                {:halt, {:ok, Sourceror.Zipper.remove(zipper)}}

              :error ->
                {:cont, :error}
            end
          end)
      end
    end

    # Conflict-aware relationship handling
    defp add_relationship_with_conflicts(
           igniter,
           resource,
           name_atom,
           relationship_code,
           conflicts_strategy
         ) do
      case conflicts_strategy do
        "ignore" ->
          Ash.Resource.Igniter.add_new_relationship(
            igniter,
            resource,
            name_atom,
            relationship_code
          )

        "replace" ->
          igniter
          |> remove_existing_relationship(resource, name_atom)
          |> Ash.Resource.Igniter.add_relationship(resource, relationship_code)
      end
    end

    defp remove_existing_relationship(igniter, resource, name_atom) do
      Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
        with {:ok, zipper} <- enter_section(zipper, :relationships),
             {:ok, zipper} <- find_and_remove_relationship(zipper, name_atom) do
          {:ok, zipper}
        else
          _ -> {:ok, zipper}
        end
      end)
    end

    defp find_and_remove_relationship(zipper, name_atom) do
      relationship_functions = [:has_one, :has_many, :belongs_to, :many_to_many]

      Enum.reduce_while(relationship_functions, :error, fn function_name, _acc ->
        case Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               function_name,
               [2, 3],
               &Igniter.Code.Function.argument_equals?(&1, 0, name_atom)
             ) do
          {:ok, zipper} ->
            {:halt, {:ok, Sourceror.Zipper.remove(zipper)}}

          :error ->
            {:cont, :error}
        end
      end)
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

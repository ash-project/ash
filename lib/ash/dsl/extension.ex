defmodule Ash.Dsl.Extension do
  @moduledoc """
  An extension to the Ash DSL.

  This allows configuring custom DSL components, whose configurations
  can then be read back. This guide is still a work in progress, but should
  serve as a decent example of what is possible. Open issues on Github if you
  have any issues/something is unclear.

  The example at the bottom shows how you might build a (not very contextually
  relevant) DSL extension that would be used like so:

      defmodule MyApp.MyResource do
        use Ash.Resource,
          extensions: [MyApp.CarExtension]

        cars do
          car :mazda, "6", trim: :touring
          car :toyota, "corolla"
        end
      end

  The extension:

      defmodule MyApp.CarExtension do
        @car_schema [
          make: [
            type: :atom,
            required: true,
            doc: "The make of the car"
          ],
          model: [
            type: :atom,
            required: true,
            doc: "The model of the car"
          ],
          type: [
            type: :atom,
            required: true,
            doc: "The type of the car",
            default: :sedan
          ]
        ]

        @car %Ash.Dsl.Entity{
          name: :car,
          describe: "Adds a car",
          examples: [
            "car :mazda, \"6\""
          ],
          target: MyApp.Car,
          args: [:make, :model],
          schema: @car_schema
        }

        @cars %Ash.Dsl.Section{
          name: :cars, # The DSL constructor will be `cars`
          describe: \"\"\"
          Configure what cars are available.

          More, deeper explanation. Always have a short one liner explanation,
          an empty line, and then a longer explanation.
          \"\"\",
          entities: [
            @car # See `Ash.Dsl.Entity` docs
          ],
          schema: [
            default_manufacturer: [
              type: :atom,
              doc: "The default manufacturer"
            ]
          ]
        }

        use Ash.Dsl.Extension, sections: [@cars]
      end


  Often, we will need to do complex validation/validate based on the configuration
  of other resources. Due to the nature of building compile time DSLs, there are
  many restrictions around that process. To support these complex use cases, extensions
  can include `transformers` which can validate/transform the DSL state after all basic
  sections/entities have been created. See `Ash.Dsl.Transformer` for more information.
  Transformers are provided as an option to `use`, like so:

      use Ash.Dsl.Extension, sections: [@cars], transformers: [
        MyApp.Transformers.ValidateNoOverlappingMakesAndModels
      ]

  By default, the generated modules will have names like `__MODULE__.SectionName.EntityName`, and that could
  potentially conflict with modules you are defining, so you can specify the `module_prefix` option, which would allow
  you to prefix the modules with something like `__MODULE__.Dsl`, so that the module path generated might be something like
  `__MODULE__.Dsl.SectionName.EntityName`, and you could then have the entity struct be `__MODULE__.SectionName.EntityName`
  without conflicts.

  To expose the configuration of your DSL, define functions that use the
  helpers like `get_entities/2` and `get_opt/3`. For example:

      defmodule MyApp.Cars do
        def cars(resource) do
          Ash.Dsl.Extension.get_entities(resource, [:cars])
        end
      end

      MyApp.Cars.cars(MyResource)
      # [%MyApp.Car{...}, %MyApp.Car{...}]

  See the documentation for `Ash.Dsl.Section` and `Ash.Dsl.Entity` for more information
  """

  @callback sections() :: [Ash.Dsl.section()]
  @callback transformers() :: [module]

  defp dsl!(resource) do
    Code.ensure_compiled!(resource)
    resource.ash_dsl_config()
  rescue
    _ in [UndefinedFunctionError, ArgumentError] ->
      try do
        Module.get_attribute(resource, :ash_dsl_config) || %{}
      rescue
        ArgumentError ->
          try do
            resource.ash_dsl_config()
          rescue
            _ ->
              reraise ArgumentError,
                      """
                      No such entity #{inspect(resource)} found.
                      """,
                      __STACKTRACE__
          end
      end
  end

  @doc "Get the entities configured for a given section"
  def get_entities(resource, path) do
    dsl!(resource)[path][:entities] || []
  end

  @doc "Get a value that was persisted while transforming or compiling the resource, e.g `:primary_key`"
  def get_persisted(resource, key, default \\ nil) do
    Map.get(dsl!(resource)[:persist] || %{}, key, default)
  end

  @doc """
  Get an option value for a section at a given path.

  Checks to see if it has been overridden via configuration.
  """
  def get_opt(resource, path, value, default, configurable? \\ false) do
    path = List.wrap(path)

    if configurable? do
      case get_opt_config(resource, path, value) do
        {:ok, value} ->
          value

        _ ->
          Keyword.get(dsl!(resource)[path][:opts] || [], value, default)
      end
    else
      Keyword.get(dsl!(resource)[path][:opts] || [], value, default)
    end
  end

  @doc """
  Generate a table of contents for a list of sections
  """
  def doc_index(sections, depth \\ 0) do
    sections
    |> Enum.flat_map(fn
      {_, entities} ->
        entities

      other ->
        [other]
    end)
    |> Enum.map_join("\n", fn
      section ->
        docs =
          if depth == 0 do
            String.duplicate(" ", depth + 1) <>
              "* [#{section.name}](#module-#{section.name})"
          else
            String.duplicate(" ", depth + 1) <> "* #{section.name}"
          end

        case List.wrap(section.entities) ++ List.wrap(Map.get(section, :sections)) do
          [] ->
            docs

          sections_and_entities ->
            docs <> "\n" <> doc_index(sections_and_entities, depth + 2)
        end
    end)
  end

  @doc """
  Generate documentation for a list of sections
  """
  def doc(sections, depth \\ 1) do
    Enum.map_join(sections, "\n\n", fn section ->
      String.duplicate("#", depth + 1) <>
        " " <>
        to_string(section.name) <> "\n\n" <> doc_section(section, depth)
    end)
  end

  def doc_section(section, depth \\ 1) do
    sections_and_entities = List.wrap(section.entities) ++ List.wrap(section.sections)

    table_of_contents =
      case sections_and_entities do
        [] ->
          ""

        sections_and_entities ->
          doc_index(sections_and_entities)
      end

    options = Ash.OptionsHelpers.docs(section.schema)

    examples =
      case section.examples do
        [] ->
          ""

        examples ->
          "Examples:\n" <> Enum.map_join(examples, &doc_example/1)
      end

    entities =
      Enum.map_join(section.entities, "\n\n", fn entity ->
        String.duplicate("#", depth + 2) <>
          " " <>
          to_string(entity.name) <>
          "\n\n" <>
          doc_entity(entity, depth + 2)
      end)

    sections =
      Enum.map_join(section.sections, "\n\n", fn section ->
        String.duplicate("#", depth + 2) <>
          " " <>
          to_string(section.name) <>
          "\n\n" <>
          doc_section(section, depth + 1)
      end)

    imports =
      case section.imports do
        [] ->
          ""

        mods ->
          "Imports:\n\n" <>
            Enum.map_join(mods, "\n", fn mod ->
              "* `#{inspect(mod)}`"
            end)
      end

    """
    #{section.describe}

    #{table_of_contents}

    #{examples}

    #{imports}

    ---

    #{options}

    #{entities}

    #{sections}
    """
  end

  def doc_entity(entity, depth \\ 1) do
    options = Ash.OptionsHelpers.docs(Keyword.drop(entity.schema, entity.hide))

    examples =
      case entity.examples do
        [] ->
          ""

        examples ->
          "Examples:\n" <> Enum.map_join(examples, &doc_example/1)
      end

    entities =
      Enum.flat_map(entity.entities, fn
        {_, entities} ->
          entities

        other ->
          [other]
      end)

    entities_doc =
      Enum.map_join(entities, "\n\n", fn entity ->
        String.duplicate("#", depth + 2) <>
          " " <>
          to_string(entity.name) <>
          "\n\n" <>
          doc_entity(entity, depth + 1)
      end)

    table_of_contents =
      case entities do
        [] ->
          ""

        entities ->
          doc_index(entities)
      end

    """
    #{entity.describe}

    #{table_of_contents}

    Introspection Target:

    `#{inspect(entity.target)}`

    #{examples}

    #{options}

    #{entities_doc}
    """
  end

  def get_opt_config(resource, path, value) do
    with otp_app when not is_nil(otp_app) <- get_persisted(resource, :otp_app),
         {:ok, config} <- Application.fetch_env(otp_app, resource) do
      path
      |> List.wrap()
      |> Kernel.++([value])
      |> Enum.reduce_while({:ok, config}, fn key, {:ok, config} ->
        if Keyword.keyword?(config) do
          case Keyword.fetch(config, key) do
            {:ok, value} -> {:cont, {:ok, value}}
            :error -> {:halt, :error}
          end
        else
          {:halt, :error}
        end
      end)
    end
  end

  defp doc_example({description, example}) when is_binary(description) and is_binary(example) do
    """
    #{description}
    ```
    #{example}
    ```
    """
  end

  defp doc_example(example) when is_binary(example) do
    """
    ```
    #{example}
    ```
    """
  end

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [
            sections: opts[:sections] || [],
            transformers: opts[:transformers] || [],
            module_prefix: opts[:module_prefix]
          ] do
      alias Ash.Dsl.Extension

      @behaviour Extension
      Extension.build(__MODULE__, module_prefix, sections)
      @_sections sections
      @_transformers transformers

      @doc false
      def sections, do: set_docs(@_sections)

      defp set_docs(items) when is_list(items) do
        Enum.map(items, &set_docs/1)
      end

      defp set_docs(%Ash.Dsl.Entity{} = entity) do
        entity
        |> Map.put(:docs, Ash.Dsl.Extension.doc_entity(entity))
        |> Map.put(
          :entities,
          Enum.map(entity.entities || [], fn {key, value} -> {key, set_docs(value)} end)
        )
      end

      defp set_docs(%Ash.Dsl.Section{} = section) do
        section
        |> Map.put(:entities, set_docs(section.entities))
        |> Map.put(:sections, set_docs(section.sections))
        |> Map.put(:docs, Ash.Dsl.Extension.doc_section(section))
      end

      @doc false
      def transformers, do: @_transformers
    end
  end

  @doc false
  def prepare(extensions) do
    body =
      quote location: :keep do
        @extensions unquote(extensions)
      end

    imports =
      for extension <- extensions || [] do
        extension = Macro.expand_once(extension, __ENV__)

        quote location: :keep do
          require Ash.Dsl.Extension
          import unquote(extension), only: :macros
        end
      end

    [body | imports]
  end

  @doc false
  defmacro set_state(additional_persisted_data) do
    quote generated: true,
          location: :keep,
          bind_quoted: [additional_persisted_data: additional_persisted_data] do
      alias Ash.Dsl.Transformer

      persist =
        additional_persisted_data
        |> Keyword.put(:extensions, @extensions || [])
        |> Enum.into(%{})

      ash_dsl_config =
        {__MODULE__, :ash_sections}
        |> Process.get([])
        |> Enum.map(fn {_extension, section_path} ->
          {section_path,
           Process.get(
             {__MODULE__, :ash, section_path},
             []
           )}
        end)
        |> Enum.into(%{})
        |> Map.update(
          :persist,
          persist,
          &Map.merge(&1, persist)
        )

      @ash_dsl_config ash_dsl_config

      for {key, _value} <- Process.get() do
        if is_tuple(key) and elem(key, 0) == __MODULE__ do
          Process.delete(key)
        end
      end

      transformers_to_run =
        @extensions
        |> Enum.flat_map(& &1.transformers())
        |> Transformer.sort()
        |> Enum.reject(& &1.after_compile?())

      __MODULE__
      |> Ash.Dsl.Extension.run_transformers(
        transformers_to_run,
        ash_dsl_config,
        true
      )
    end
  end

  defmacro run_after_compile do
    quote do
      transformers_to_run =
        @extensions
        |> Enum.flat_map(& &1.transformers())
        |> Ash.Dsl.Transformer.sort()
        |> Enum.filter(& &1.after_compile?())

      __MODULE__
      |> Ash.Dsl.Extension.run_transformers(
        transformers_to_run,
        Module.get_attribute(__MODULE__, :ash_dsl_config),
        false
      )
    end
  end

  def run_transformers(mod, transformers, ash_dsl_config, store?) do
    Enum.reduce_while(transformers, ash_dsl_config, fn transformer, dsl ->
      result =
        try do
          transformer.transform(mod, dsl)
        rescue
          e ->
            if Exception.exception?(e) do
              reraise e, __STACKTRACE__
            else
              reraise "Exception in transformer #{inspect(transformer)}: \n\n#{Exception.message(e)}",
                      __STACKTRACE__
            end
        end

      case result do
        :ok ->
          {:cont, dsl}

        :halt ->
          {:halt, dsl}

        {:ok, new_dsl} ->
          if store? do
            Module.put_attribute(mod, :ash_dsl_config, new_dsl)
          end

          {:cont, new_dsl}

        {:error, error} ->
          raise_transformer_error(transformer, error)
      end
    end)
  end

  defp raise_transformer_error(transformer, error) do
    if Exception.exception?(error) do
      raise error
    else
      raise "Error while running transformer #{inspect(transformer)}: #{inspect(error)}"
    end
  end

  @doc false
  def all_section_paths(path, prior \\ [])
  def all_section_paths(nil, _), do: []
  def all_section_paths([], _), do: []

  def all_section_paths(sections, prior) do
    Enum.flat_map(sections, fn section ->
      nested = all_section_paths(section.sections, [section.name, prior])

      [Enum.reverse(prior) ++ [section.name] | nested]
    end)
    |> Enum.uniq()
  end

  @doc false
  def all_section_config_paths(path, prior \\ [])
  def all_section_config_paths(nil, _), do: []
  def all_section_config_paths([], _), do: []

  def all_section_config_paths(sections, prior) do
    Enum.flat_map(sections, fn section ->
      nested = all_section_config_paths(section.sections, [section.name, prior])

      fields =
        Enum.map(section.schema, fn {key, _} ->
          {Enum.reverse(prior) ++ [section.name], key}
        end)

      fields ++ nested
    end)
    |> Enum.uniq()
  end

  @doc false
  defmacro build(extension, module_prefix, sections) do
    quote bind_quoted: [sections: sections, extension: extension, module_prefix: module_prefix] do
      alias Ash.Dsl.Extension

      for section <- sections do
        Extension.build_section(extension, section, [], [], module_prefix)
      end
    end
  end

  @doc false
  defmacro build_section(extension, section, unimports \\ [], path \\ [], module_prefix \\ nil) do
    quote bind_quoted: [
            section: section,
            path: path,
            extension: extension,
            module_prefix: module_prefix,
            unimports: Macro.escape(unimports)
          ] do
      alias Ash.Dsl

      {section_modules, entity_modules, opts_module} =
        Dsl.Extension.do_build_section(
          module_prefix || __MODULE__,
          extension,
          section,
          path,
          unimports
        )

      @doc false
      # This macro argument is only called `body` so that it looks nicer
      # in the DSL docs

      @doc false
      defmacro unquote(section.name)(body) do
        opts_module = unquote(opts_module)
        section_path = unquote(path ++ [section.name])
        section = unquote(Macro.escape(section))
        unimports = unquote(Macro.escape(unimports))

        configured_imports =
          for module <- unquote(section.imports) do
            quote do
              import unquote(module)
            end
          end

        entity_imports =
          for module <- unquote(entity_modules) do
            quote do
              import unquote(module), only: :macros
            end
          end

        section_imports =
          for module <- unquote(section_modules) do
            quote do
              import unquote(module), only: :macros
            end
          end

        opts_import =
          if Map.get(unquote(Macro.escape(section)), :schema, []) == [] do
            []
          else
            [
              quote do
                import unquote(opts_module)
              end
            ]
          end

        configured_unimports =
          for module <- unquote(section.imports) do
            quote do
              import unquote(module), only: []
            end
          end

        entity_unimports =
          for module <- unquote(entity_modules) do
            quote do
              import unquote(module), only: []
            end
          end

        section_unimports =
          for module <- unquote(section_modules) do
            quote do
              import unquote(module), only: []
            end
          end

        opts_unimport =
          if Map.get(unquote(Macro.escape(section)), :schema, []) == [] do
            []
          else
            [
              quote do
                import unquote(opts_module), only: []
              end
            ]
          end

        entity_imports ++
          section_imports ++
          opts_import ++
          configured_imports ++
          unimports ++
          [
            quote do
              unquote(body[:do])

              current_config =
                Process.get(
                  {__MODULE__, :ash, unquote(section_path)},
                  %{entities: [], opts: []}
                )

              opts =
                case Ash.OptionsHelpers.validate(
                       current_config.opts,
                       Map.get(unquote(Macro.escape(section)), :schema, [])
                     ) do
                  {:ok, opts} ->
                    opts

                  {:error, error} ->
                    raise Ash.Error.Dsl.DslError,
                      module: __MODULE__,
                      message: error,
                      path: unquote(section_path)
                end

              Process.put(
                {__MODULE__, :ash, unquote(section_path)},
                %{
                  entities: current_config.entities,
                  opts: opts
                }
              )
            end
          ] ++
          configured_unimports ++
          opts_unimport ++ entity_unimports ++ section_unimports
      end
    end
  end

  defp entity_mod_name(mod, nested_entity_path, section_path, entity) do
    nested_entity_parts = Enum.map(nested_entity_path, &Macro.camelize(to_string(&1)))
    section_path_parts = Enum.map(section_path, &Macro.camelize(to_string(&1)))

    mod_parts =
      Enum.concat([
        [mod],
        section_path_parts,
        nested_entity_parts,
        [Macro.camelize(to_string(entity.name))]
      ])

    Module.concat(mod_parts)
  end

  defp section_mod_name(mod, path, section) do
    nested_mod_name =
      path
      |> Enum.drop(1)
      |> Enum.map(fn nested_section_name ->
        Macro.camelize(to_string(nested_section_name))
      end)

    Module.concat([mod | nested_mod_name] ++ [Macro.camelize(to_string(section.name))])
  end

  defp unimports(mod, section, path, opts_mod_name) do
    entity_modules =
      Enum.map(section.entities, fn entity ->
        entity_mod_name(mod, [], path ++ [section.name], entity)
      end)

    entity_unimports =
      for module <- entity_modules do
        quote do
          import unquote(module), only: []
        end
      end

    section_modules =
      Enum.map(section.sections, fn section ->
        section_mod_name(mod, path, section)
      end)

    section_unimports =
      for module <- section_modules do
        quote do
          import unquote(module), only: []
        end
      end

    opts_unimport =
      if section.schema == [] do
        []
      else
        [
          quote do
            import unquote(opts_mod_name), only: []
          end
        ]
      end

    opts_unimport ++ section_unimports ++ entity_unimports
  end

  @doc false
  def do_build_section(mod, extension, section, path, unimports) do
    opts_mod_name =
      if section.schema == [] do
        nil
      else
        Module.concat([mod, Macro.camelize(to_string(section.name)), Options])
      end

    entity_modules =
      Enum.map(section.entities, fn entity ->
        entity = %{
          entity
          | auto_set_fields: Keyword.merge(section.auto_set_fields, entity.auto_set_fields)
        }

        build_entity(
          mod,
          extension,
          path ++ [section.name],
          entity,
          section.deprecations,
          [],
          unimports ++
            unimports(
              mod,
              %{section | entities: Enum.reject(section.entities, &(&1.name == entity.name))},
              path,
              opts_mod_name
            )
        )
      end)

    section_modules =
      Enum.map(section.sections, fn nested_section ->
        nested_mod_name =
          path
          |> Enum.drop(1)
          |> Enum.map(fn nested_section_name ->
            Macro.camelize(to_string(nested_section_name))
          end)

        mod_name =
          Module.concat(
            [mod | nested_mod_name] ++ [Macro.camelize(to_string(nested_section.name))]
          )

        {:module, module, _, _} =
          Module.create(
            mod_name,
            quote do
              @moduledoc false
              alias Ash.Dsl

              require Dsl.Extension

              Dsl.Extension.build_section(
                extension,
                nested_section,
                path ++ [section.name],
                unimports ++
                  unimports(
                    mod,
                    %{
                      section
                      | section: Enum.reject(section.sections, &(&1.name == nested_section.name))
                    },
                    path,
                    opts_mod_name
                  ),
                nil
              )
            end,
            Macro.Env.location(__ENV__)
          )

        module
      end)

    if opts_mod_name do
      Module.create(
        opts_mod_name,
        quote bind_quoted: [
                section: Macro.escape(section),
                section_path: path ++ [section.name],
                extension: extension
              ] do
          @moduledoc false

          for {field, _opts} <- section.schema do
            defmacro unquote(field)(value) do
              section_path = unquote(Macro.escape(section_path))
              field = unquote(Macro.escape(field))
              extension = unquote(extension)
              section = unquote(Macro.escape(section))

              Ash.Dsl.Extension.maybe_deprecated(
                field,
                section.deprecations,
                section_path,
                __CALLER__
              )

              value =
                if field in section.modules do
                  Ash.Dsl.Extension.expand_alias(value, __CALLER__)
                else
                  value
                end

              quote do
                current_sections = Process.get({__MODULE__, :ash_sections}, [])

                unless {unquote(extension), unquote(section_path)} in current_sections do
                  Process.put({__MODULE__, :ash_sections}, [
                    {unquote(extension), unquote(section_path)} | current_sections
                  ])
                end

                current_config =
                  Process.get(
                    {__MODULE__, :ash, unquote(section_path)},
                    %{entities: [], opts: []}
                  )

                Process.put(
                  {__MODULE__, :ash, unquote(section_path)},
                  %{
                    current_config
                    | opts: Keyword.put(current_config.opts, unquote(field), unquote(value))
                  }
                )
              end
            end
          end
        end,
        Macro.Env.location(__ENV__)
      )
    end

    {section_modules, entity_modules, opts_mod_name}
  end

  @doc false
  def build_entity(
        mod,
        extension,
        section_path,
        entity,
        deprecations,
        nested_entity_path,
        unimports,
        nested_key \\ nil
      ) do
    mod_name = entity_mod_name(mod, nested_entity_path, section_path, entity)

    options_mod_name = Module.concat(mod_name, "Options")

    nested_entity_mods =
      Enum.flat_map(entity.entities, fn {key, entities} ->
        entities
        |> List.wrap()
        |> Enum.map(fn nested_entity ->
          nested_entity_mod_names =
            entity.entities
            |> Enum.flat_map(fn {key, entities} ->
              entities
              |> List.wrap()
              |> Enum.reject(&(&1.name == nested_entity.name))
              |> Enum.map(fn nested_entity ->
                entity_mod_name(
                  mod_name,
                  nested_entity_path ++ [key],
                  section_path,
                  nested_entity
                )
              end)
            end)

          unimports =
            unimports ++
              Enum.map([options_mod_name | nested_entity_mod_names], fn mod_name ->
                quote do
                  import unquote(mod_name), only: []
                end
              end)

          build_entity(
            mod_name,
            extension,
            section_path,
            nested_entity,
            nested_entity.deprecations,
            nested_entity_path ++ [key],
            unimports,
            key
          )
        end)
      end)

    Ash.Dsl.Extension.build_entity_options(
      options_mod_name,
      entity,
      nested_entity_path
    )

    args = Enum.map(entity.args, &Macro.var(&1, mod_name))

    Module.create(
      mod_name,
      quote bind_quoted: [
              extension: extension,
              entity: Macro.escape(entity),
              args: Macro.escape(args),
              section_path: Macro.escape(section_path),
              options_mod_name: Macro.escape(options_mod_name),
              nested_entity_mods: Macro.escape(nested_entity_mods),
              nested_entity_path: Macro.escape(nested_entity_path),
              deprecations: deprecations,
              unimports: Macro.escape(unimports),
              nested_key: nested_key
            ] do
        @moduledoc false
        defmacro unquote(entity.name)(unquote_splicing(args), opts \\ []) do
          section_path = unquote(Macro.escape(section_path))
          entity_schema = unquote(Macro.escape(entity.schema))
          entity = unquote(Macro.escape(entity))
          entity_name = unquote(Macro.escape(entity.name))
          entity_args = unquote(Macro.escape(entity.args))
          entity_deprecations = unquote(entity.deprecations)
          options_mod_name = unquote(Macro.escape(options_mod_name))
          source = unquote(__MODULE__)
          extension = unquote(Macro.escape(extension))
          nested_entity_mods = unquote(Macro.escape(nested_entity_mods))
          nested_entity_path = unquote(Macro.escape(nested_entity_path))
          deprecations = unquote(deprecations)
          unimports = unquote(Macro.escape(unimports))
          nested_key = unquote(nested_key)

          Ash.Dsl.Extension.maybe_deprecated(
            entity.name,
            deprecations,
            section_path ++ nested_entity_path,
            __CALLER__
          )

          arg_values =
            entity_args
            |> Enum.zip(unquote(args))
            |> Enum.map(fn {key, value} ->
              Ash.Dsl.Extension.maybe_deprecated(
                key,
                entity_deprecations,
                nested_entity_path,
                __CALLER__
              )

              if key in entity.modules do
                Ash.Dsl.Extension.expand_alias(value, __CALLER__)
              else
                value
              end
            end)

          opts =
            Enum.map(opts, fn {key, value} ->
              Ash.Dsl.Extension.maybe_deprecated(
                key,
                entity_deprecations,
                nested_entity_path,
                __CALLER__
              )

              if key in entity.modules do
                {key, Ash.Dsl.Extension.expand_alias(value, __CALLER__)}
              else
                {key, value}
              end
            end)

          code =
            unimports ++
              [
                quote do
                  section_path = unquote(section_path)
                  entity_name = unquote(entity_name)
                  extension = unquote(extension)
                  recursive_as = unquote(entity.recursive_as)
                  nested_key = unquote(nested_key)

                  original_nested_entity_path = Process.get(:recursive_builder_path)

                  nested_entity_path =
                    if is_nil(original_nested_entity_path) do
                      Process.put(:recursive_builder_path, [])
                      []
                    else
                      unless recursive_as || nested_key do
                        raise "Somehow got a nested entity without a `recursive_as` or `nested_key`"
                      end

                      path = (original_nested_entity_path || []) ++ [recursive_as || nested_key]

                      Process.put(
                        :recursive_builder_path,
                        path
                      )

                      path
                    end

                  current_sections = Process.get({__MODULE__, :ash_sections}, [])

                  keyword_opts =
                    Keyword.merge(
                      unquote(Keyword.delete(opts, :do)),
                      Enum.zip(unquote(entity_args), unquote(arg_values))
                    )

                  Process.put(
                    {:builder_opts, nested_entity_path},
                    keyword_opts
                  )

                  import unquote(options_mod_name)

                  require Ash.Dsl.Extension
                  Ash.Dsl.Extension.import_mods(unquote(nested_entity_mods))

                  unquote(opts[:do])

                  Process.put(:recursive_builder_path, original_nested_entity_path)

                  current_config =
                    Process.get(
                      {__MODULE__, :ash, section_path ++ nested_entity_path},
                      %{entities: [], opts: []}
                    )

                  import unquote(options_mod_name), only: []

                  require Ash.Dsl.Extension
                  Ash.Dsl.Extension.unimport_mods(unquote(nested_entity_mods))

                  opts = Process.delete({:builder_opts, nested_entity_path})

                  alias Ash.Dsl.Entity

                  nested_entity_keys =
                    unquote(Macro.escape(entity.entities))
                    |> Enum.map(&elem(&1, 0))
                    |> Enum.uniq()

                  nested_entities =
                    nested_entity_keys
                    |> Enum.reduce(%{}, fn key, acc ->
                      nested_path = section_path ++ nested_entity_path ++ [key]

                      entities =
                        {__MODULE__, :ash, nested_path}
                        |> Process.get(%{entities: []})
                        |> Map.get(:entities, [])

                      Process.delete({__MODULE__, :ash, nested_path})

                      Map.update(acc, key, entities, fn current_nested_entities ->
                        (current_nested_entities || []) ++ entities
                      end)
                    end)

                  built =
                    case Entity.build(unquote(Macro.escape(entity)), opts, nested_entities) do
                      {:ok, built} ->
                        built

                      {:error, error} ->
                        additional_path =
                          if opts[:name] do
                            [unquote(entity.name), opts[:name]]
                          else
                            [unquote(entity.name)]
                          end

                        message =
                          cond do
                            Exception.exception?(error) ->
                              Exception.message(error)

                            is_binary(error) ->
                              error

                            true ->
                              inspect(error)
                          end

                        raise Ash.Error.Dsl.DslError,
                          module: __MODULE__,
                          message: message,
                          path: section_path ++ additional_path
                    end

                  new_config = %{current_config | entities: current_config.entities ++ [built]}

                  unless {extension, section_path} in current_sections do
                    Process.put({__MODULE__, :ash_sections}, [
                      {extension, section_path} | current_sections
                    ])
                  end

                  Process.put(
                    {__MODULE__, :ash, section_path ++ nested_entity_path},
                    new_config
                  )
                end
              ]

          # This is (for some reason I'm not really sure why) necessary to keep the imports within a lexical scope
          quote do
            try do
              unquote(code)
            rescue
              e ->
                reraise e, __STACKTRACE__
            end
          end
        end
      end,
      Macro.Env.location(__ENV__)
    )

    mod_name
  end

  defmacro import_mods(mods) do
    for mod <- mods do
      quote do
        import unquote(mod)
      end
    end
  end

  defmacro unimport_mods(mods) do
    for mod <- mods do
      quote do
        import unquote(mod), only: []
      end
    end
  end

  @doc false
  def build_entity_options(
        module_name,
        entity,
        nested_entity_path
      ) do
    Module.create(
      module_name,
      quote bind_quoted: [
              entity: Macro.escape(entity),
              nested_entity_path: nested_entity_path
            ] do
        @moduledoc false

        for {key, _value} <- entity.schema do
          defmacro unquote(key)(value) do
            key = unquote(key)
            modules = unquote(entity.modules)
            deprecations = unquote(entity.deprecations)
            entity_name = unquote(entity.name)
            recursive_as = unquote(entity.recursive_as)

            nested_entity_path = Process.get(:recursive_builder_path)

            Ash.Dsl.Extension.maybe_deprecated(key, deprecations, nested_entity_path, __CALLER__)

            value =
              if key in modules do
                Ash.Dsl.Extension.expand_alias(value, __CALLER__)
              else
                value
              end

            quote do
              current_opts = Process.get({:builder_opts, nested_entity_path}, [])

              Process.put(
                {:builder_opts, nested_entity_path},
                Keyword.put(current_opts, unquote(key), unquote(value))
              )
            end
          end
        end
      end,
      file: __ENV__.file
    )

    module_name
  end

  @doc false
  def maybe_deprecated(field, deprecations, path, env) do
    if Keyword.has_key?(deprecations, field) do
      prefix =
        case Enum.join(path) do
          "" -> ""
          path -> "#{path}."
        end

      IO.warn(
        "The #{prefix}#{field} key will be deprecated in an upcoming release!\n\n#{deprecations[field]}",
        Macro.Env.stacktrace(env)
      )
    end
  end

  def expand_alias(ast, %Macro.Env{} = env) do
    Macro.postwalk(ast, fn
      {first, {:__aliases__, _, _} = node} ->
        {first, Macro.expand(node, %{env | function: {:ash_dsl_config, 0}, lexical_tracker: nil})}

      {{:__aliases__, _, _} = node, second} ->
        {Macro.expand(node, %{env | function: {:ash_dsl_config, 0}, lexical_tracker: nil}),
         second}

      {:__aliases__, _, _} = node ->
        Macro.expand(node, %{env | function: {:ash_dsl_config, 0}, lexical_tracker: nil})

      other ->
        other
    end)
  end
end

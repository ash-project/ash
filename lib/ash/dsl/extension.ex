defmodule Ash.Dsl.Extension do
  @moduledoc """
  An extension to the Ash DSL.

  This allows configuring custom DSL components, whos configurations
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

  @callback sections() :: [Ash.Dsl.Section.t()]
  @callback transformers() :: [module]

  @doc "Get the entities configured for a given section"
  def get_entities(resource, path) do
    Ash.Helpers.try_compile(resource)
    :persistent_term.get({resource, :ash, path}, %{entities: []}).entities
  end

  @doc "Get a value that was persisted while transforming or compiling the resource, e.g `:primary_key`"
  def get_persisted(resource, key, default \\ nil) do
    Ash.Helpers.try_compile(resource)
    :persistent_term.get({resource, key}, default)
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
          Ash.Helpers.try_compile(resource)

          Keyword.get(
            :persistent_term.get({resource, :ash, path}, %{opts: []}).opts,
            value,
            default
          )
      end
    else
      Ash.Helpers.try_compile(resource)

      Keyword.get(
        :persistent_term.get({resource, :ash, path}, %{opts: []}).opts,
        value,
        default
      )
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

  defp doc_section(section, depth) do
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
          "Examples:\n" <>
            Enum.map_join(examples, fn example ->
              """
              ```
              #{example}
              ```
              """
            end)
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

  defp doc_entity(entity, depth) do
    options = Ash.OptionsHelpers.docs(Keyword.drop(entity.schema, entity.hide))

    examples =
      case entity.examples do
        [] ->
          ""

        examples ->
          "Examples:\n" <>
            Enum.map_join(examples, fn example ->
              """
              ```
              #{example}
              ```
              """
            end)
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
    with {:ok, config} <- Application.fetch_env(:ash, resource),
         {:ok, value} <-
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
           end) do
      {:ok, value}
    end
  end

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [
            sections: opts[:sections] || [],
            transformers: opts[:transformers] || []
          ] do
      alias Ash.Dsl.Extension

      @behaviour Extension
      Extension.build(__MODULE__, sections)
      @_sections sections
      @_transformers transformers

      @doc false
      def sections, do: @_sections

      @doc false
      def transformers, do: @_transformers
    end
  end

  @doc false
  def prepare(extensions) do
    body =
      quote location: :keep do
        @extensions unquote(extensions)
        # Due to a few strange stateful bugs I've seen,
        # we clear the process of any potentially related state
        for {key, _value} <- Process.get() do
          if is_tuple(key) and elem(key, 0) == __MODULE__ do
            Process.delete(key)
          end
        end

        for {key, _value} <- :persistent_term.get() do
          if is_tuple(key) and elem(key, 0) == __MODULE__ do
            :persistent_term.erase(key)
          end
        end
      end

    imports =
      for extension <- extensions || [] do
        extension = Macro.expand_once(extension, __ENV__)

        quote location: :keep do
          require Ash.Dsl.Extension
          alias Ash.Dsl.Extension
          Extension.import_extension(unquote(extension))
        end
      end

    [body | imports]
  end

  @doc false
  defmacro set_state(additional_persisted_data \\ []) do
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

      Ash.Dsl.Extension.write_dsl_to_persistent_term(__MODULE__, ash_dsl_config)

      transformers_to_run =
        @extensions
        |> Enum.flat_map(& &1.transformers())
        |> Transformer.sort()

      __MODULE__
      |> Ash.Dsl.Extension.run_transformers(
        transformers_to_run,
        ash_dsl_config
      )
    end
  end

  defmacro load do
    quote do
      Ash.Dsl.Extension.write_dsl_to_persistent_term(
        __MODULE__,
        ash_dsl_config()
      )

      :ok
    end
  end

  def run_transformers(mod, transformers, ash_dsl_config) do
    Enum.reduce_while(transformers, ash_dsl_config, fn transformer, dsl ->
      result =
        try do
          transformer.transform(mod, dsl)
        rescue
          e ->
            if Exception.exception?(e) do
              reraise e, __STACKTRACE__
            else
              reraise "Exception in transformer #{inspect(transformer)}: \n\n#{
                        Exception.message(e)
                      }",
                      __STACKTRACE__
            end
        end

      case result do
        :halt ->
          {:halt, dsl}

        {:ok, new_dsl} ->
          write_dsl_to_persistent_term(mod, new_dsl)
          {:cont, new_dsl}

        {:error, error} ->
          raise_transformer_error(transformer, error)
      end
    end)
  end

  @doc false
  def write_dsl_to_persistent_term(mod, dsl) do
    dsl
    |> Map.delete(:persist)
    |> Enum.each(fn {section_path, value} ->
      :persistent_term.put({mod, :ash, section_path}, value)
    end)

    Enum.each(Map.get(dsl, :persist, %{}), fn {key, value} ->
      :persistent_term.put({mod, key}, value)
    end)

    dsl
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
  defmacro import_extension(extension) do
    quote do
      import unquote(extension), only: :macros
    end
  end

  @doc false
  defmacro build(extension, sections) do
    quote bind_quoted: [sections: sections, extension: extension] do
      alias Ash.Dsl.Extension

      for section <- sections do
        Extension.build_section(extension, section, true)
      end
    end
  end

  @doc false
  defmacro build_section(extension, section, doc?, path \\ []) do
    quote bind_quoted: [section: section, path: path, extension: extension, doc?: doc?] do
      alias Ash.Dsl

      {section_modules, entity_modules, opts_module} =
        Dsl.Extension.do_build_section(__MODULE__, extension, section, path)

      @doc false

      # This macro argument is only called `body` so that it looks nicer
      # in the DSL docs

      @doc false
      defmacro unquote(section.name)(body) do
        opts_module = unquote(opts_module)
        section_path = unquote(path ++ [section.name])
        section = unquote(Macro.escape(section))

        configured_imports =
          for module <- unquote(section.imports) do
            quote do
              import unquote(module)
            end
          end

        entity_imports =
          for module <- unquote(entity_modules) do
            quote do
              import unquote(module)
            end
          end

        section_imports =
          for module <- unquote(section_modules) do
            quote do
              import unquote(module)
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
                      module: unquote(__MODULE__),
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
          ] ++ configured_unimports ++ opts_unimport ++ entity_unimports ++ section_unimports
      end
    end
  end

  @doc false
  def do_build_section(mod, extension, section, path) do
    entity_modules =
      Enum.map(section.entities, fn entity ->
        build_entity(mod, extension, path ++ [section.name], entity)
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
          defmodule mod_name do
            alias Ash.Dsl
            @moduledoc false

            require Dsl.Extension

            Dsl.Extension.build_section(
              extension,
              nested_section,
              false,
              path ++ [section.name]
            )
          end

        module
      end)

    opts_mod_name =
      if section.schema == [] do
        nil
      else
        opts_mod_name = Module.concat([mod, Macro.camelize(to_string(section.name)), Options])

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

        opts_mod_name
      end

    {section_modules, entity_modules, opts_mod_name}
  end

  @doc false
  def build_entity(mod, extension, section_path, entity, nested_entity_path \\ []) do
    nested_entity_parts = Enum.map(nested_entity_path, &Macro.camelize(to_string(&1)))

    mod_parts =
      Enum.concat([[mod], nested_entity_parts, [Macro.camelize(to_string(entity.name))]])

    mod_name = Module.concat(mod_parts)

    options_mod_name = Module.concat(mod_name, "Options")

    nested_entity_mods =
      Enum.flat_map(entity.entities, fn {key, entities} ->
        entities
        |> List.wrap()
        |> Enum.map(fn entity ->
          build_entity(
            mod_name,
            extension,
            section_path,
            entity,
            nested_entity_path ++ [key]
          )
        end)
      end)

    Ash.Dsl.Extension.build_entity_options(
      options_mod_name,
      entity.schema,
      entity.modules,
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
              nested_entity_path: Macro.escape(nested_entity_path)
            ] do
        @moduledoc false
        defmacro unquote(entity.name)(unquote_splicing(args), opts \\ []) do
          section_path = unquote(Macro.escape(section_path))
          entity_schema = unquote(Macro.escape(entity.schema))
          entity = unquote(Macro.escape(entity))
          entity_name = unquote(Macro.escape(entity.name))
          entity_args = unquote(Macro.escape(entity.args))
          options_mod_name = unquote(Macro.escape(options_mod_name))
          source = unquote(__MODULE__)
          extension = unquote(Macro.escape(extension))
          nested_entity_mods = unquote(Macro.escape(nested_entity_mods))
          nested_entity_path = unquote(Macro.escape(nested_entity_path))

          arg_values =
            entity_args
            |> Enum.zip(unquote(args))
            |> Enum.map(fn {key, value} ->
              if key in entity.modules do
                Ash.Dsl.Extension.expand_alias(value, __CALLER__)
              else
                value
              end
            end)

          opts =
            Enum.map(opts, fn {key, value} ->
              if key in entity.modules do
                {key, Ash.Dsl.Extension.expand_alias(value, __CALLER__)}
              else
                {key, value}
              end
            end)

          quote do
            # This `try do` block scopes the imports/unimports properly
            try do
              section_path = unquote(section_path)
              entity_name = unquote(entity_name)
              extension = unquote(extension)

              current_sections = Process.get({__MODULE__, :ash_sections}, [])

              Process.put(
                {:builder_opts, unquote(nested_entity_path)},
                Keyword.merge(
                  unquote(Keyword.delete(opts, :do)),
                  Enum.zip(unquote(entity_args), unquote(arg_values))
                )
              )

              import unquote(options_mod_name)

              Ash.Dsl.Extension.import_mods(unquote(nested_entity_mods))

              unquote(opts[:do])

              current_config =
                Process.get(
                  {__MODULE__, :ash, section_path ++ unquote(nested_entity_path)},
                  %{entities: [], opts: []}
                )

              import unquote(options_mod_name), only: []

              Ash.Dsl.Extension.unimport_mods(unquote(nested_entity_mods))

              opts = Process.delete({:builder_opts, unquote(nested_entity_path)})

              alias Ash.Dsl.Entity

              nested_entities =
                unquote(Macro.escape(entity.entities))
                |> Enum.map(&elem(&1, 0))
                |> Enum.uniq()
                |> Enum.reduce(%{}, fn key, acc ->
                  nested_path = section_path ++ unquote(nested_entity_path) ++ [key]

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
                      module: unquote(__MODULE__),
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
                {__MODULE__, :ash, section_path ++ unquote(nested_entity_path)},
                new_config
              )
            rescue
              e -> reraise e, __STACKTRACE__
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
  def build_entity_options(module_name, schema, modules, nested_entity_path) do
    Module.create(
      module_name,
      quote bind_quoted: [
              schema: Macro.escape(schema),
              nested_entity_path: nested_entity_path,
              modules: modules
            ] do
        @moduledoc false

        for {key, _value} <- schema do
          defmacro unquote(key)(value) do
            key = unquote(key)
            nested_entity_path = unquote(nested_entity_path)
            modules = unquote(modules)

            value =
              if key in modules do
                Ash.Dsl.Extension.expand_alias(value, __CALLER__)
              else
                value
              end

            quote do
              current_opts = Process.get({:builder_opts, unquote(nested_entity_path)}, [])

              Process.put(
                {:builder_opts, unquote(nested_entity_path)},
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

  def expand_alias(ast, env) do
    Macro.postwalk(ast, fn
      {:__aliases__, _, _} = node ->
        Macro.expand(node, %{env | function: {:ash_dsl_config, 0}})

      other ->
        other
    end)
  end
end

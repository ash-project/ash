# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Extension do
  @moduledoc """
  An extension to the Spark DSL.

  This allows configuring custom DSL components, whose configurations
  can then be read back. This guide is still a work in progress, but should
  serve as a decent example of what is possible. Open issues on Github if you
  have any issues/something is unclear.

  The example at the bottom shows how you might build a (not very contextually
  relevant) DSL extension that would be used like so:

      defmodule MyApp.Vehicle do
        use Spark.Dsl
      end

      defmodule MyApp.MyResource do
        use MyApp.Vehicle,
          extensions: [MyApp.CarExtension]

        cars do
          car :ford, :focus, trim: :sedan
          car :toyota, :corolla
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

        @car %Spark.Dsl.Entity{
          name: :car,
          describe: "Adds a car",
          examples: [
            "car :ford, :focus"
          ],
          target: MyApp.Car,
          args: [:make, :model],
          schema: @car_schema
        }

        @cars %Spark.Dsl.Section{
          name: :cars, # The DSL constructor will be `cars`
          describe: \"\"\"
          Configure what cars are available.

          More, deeper explanation. Always have a short one liner explanation,
          an empty line, and then a longer explanation.
          \"\"\",
          entities: [
            @car # See `Spark.Dsl.Entity` docs
          ],
          schema: [
            default_manufacturer: [
              type: :atom,
              doc: "The default manufacturer"
            ]
          ]
        }

        use Spark.Dsl.Extension, sections: [@cars]
      end


  Often, we will need to do complex validation/validate based on the configuration
  of other resources. Due to the nature of building compile time DSLs, there are
  many restrictions around that process. To support these complex use cases, extensions
  can include `transformers` which can validate/transform the DSL state after all basic
  sections/entities have been created. See `Spark.Dsl.Transformer` for more information.
  Transformers are provided as an option to `use`, like so:

      use Spark.Dsl.Extension, sections: [@cars], transformers: [
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
          Spark.Dsl.Extension.get_entities(resource, [:cars])
        end
      end

      MyApp.Cars.cars(MyResource)
      # [%MyApp.Car{...}, %MyApp.Car{...}]

  See the documentation for `Spark.Dsl.Section` and `Spark.Dsl.Entity` for more information
  """

  @type t :: module

  @callback sections() :: [Spark.Dsl.section()]
  @callback module_imports() :: [module]
  @callback transformers() :: [module]
  @callback verifiers() :: [module]
  @callback persisters() :: [module]
  @callback explain(map) :: String.t() | nil
  @callback add_extensions() :: [module]

  @optional_callbacks explain: 1

  @parallel_compile Application.compile_env(:spark, :parallel_compile, false)

  defp persisted!(resource, key, default) do
    resource.persisted(key, default)
  rescue
    _ in [UndefinedFunctionError, ArgumentError] ->
      try do
        Map.get(Module.get_attribute(resource, :persisted) || %{}, key)
      rescue
        ArgumentError ->
          try do
            resource.persisted(key)
          rescue
            _ ->
              reraise ArgumentError,
                      """
                      `#{inspect(resource)}` is not a Spark DSL module.
                      """,
                      __STACKTRACE__
          end
      end
  end

  defp fetch_persisted!(resource, key) do
    resource.fetch_persisted(key)
  rescue
    _ in [UndefinedFunctionError, ArgumentError] ->
      try do
        Map.fetch(Module.get_attribute(resource, :persisted) || %{}, key)
      rescue
        ArgumentError ->
          try do
            resource.fetch_persisted(key)
          rescue
            _ ->
              reraise ArgumentError,
                      """
                      `#{inspect(resource)}` is not a Spark DSL module.
                      """,
                      __STACKTRACE__
          end
      end
  end

  @doc false
  def get_attribute(mod, attr) do
    Module.get_attribute(mod, attr)
  rescue
    ArgumentError ->
      mod.module_info(:attributes)[attr]
  end

  @doc """
  Get the annotation for a section at the given path.
  """
  @spec get_section_anno(map | module, atom | list(atom)) :: :erl_anno.anno() | nil
  def get_section_anno(dsl_state, path)

  def get_section_anno(%struct{}, path) do
    get_section_anno(struct, path)
  end

  def get_section_anno(map, path) when not is_list(path) do
    get_section_anno(map, [path])
  end

  def get_section_anno(map, path) when is_map(map) do
    Spark.Dsl.Transformer.get_section_anno(map, path)
  end

  def get_section_anno(resource, path) do
    get_config_entry_with_fallback(
      resource,
      path,
      fn -> resource.section_anno(path) end,
      :section_anno,
      & &1,
      &Spark.Dsl.Transformer.get_section_anno(&1, path)
    )
  end

  @doc """
  Get the annotation for a specific option in a section.
  """
  @spec get_opt_anno(map | module, atom | list(atom), atom) :: :erl_anno.anno() | nil
  def get_opt_anno(dsl_state, path, opt_name)

  def get_opt_anno(%struct{}, path, opt_name) do
    get_opt_anno(struct, path, opt_name)
  end

  def get_opt_anno(map, path, opt_name) when not is_list(path) do
    get_opt_anno(map, [path], opt_name)
  end

  def get_opt_anno(map, path, opt_name) when is_map(map) do
    Spark.Dsl.Transformer.get_opt_anno(map, path, opt_name)
  end

  def get_opt_anno(resource, path, opt_name) do
    get_config_entry_with_fallback(
      resource,
      path,
      fn -> resource.opt_anno(path, opt_name) end,
      :opts_anno,
      &Keyword.get(&1, opt_name),
      &Spark.Dsl.Transformer.get_opt_anno(&1, path, opt_name)
    )
  end

  @doc "Get the entities configured for a given section"
  def get_entities(map, nil), do: get_entities(map, [])

  def get_entities(%struct{}, path) do
    get_entities(struct, path)
  end

  def get_entities(map, path) when not is_list(path), do: get_entities(map, [path])

  def get_entities(map, path) when is_map(map) do
    Spark.Dsl.Transformer.get_entities(map, path) || []
  end

  def get_entities(resource, path) do
    get_config_entry_with_fallback(
      resource,
      path,
      fn -> resource.entities(path) end,
      :entities,
      & &1,
      &Spark.Dsl.Transformer.get_entities(&1, path)
    )
  end

  @doc "Get a value that was persisted while transforming or compiling the resource, e.g `:primary_key`"
  def get_persisted(resource, key, default \\ nil)

  def get_persisted(%struct{}, key, default) do
    get_persisted(struct, key, default)
  end

  def get_persisted(map, key, default) when is_map(map) do
    Spark.Dsl.Transformer.get_persisted(map, key, default)
  end

  def get_persisted(resource, :module, _) when is_atom(resource) do
    resource
  end

  def get_persisted(resource, key, default) do
    persisted!(resource, key, default)
  end

  @doc "Fetch a value that was persisted while transforming or compiling the resource, e.g `:primary_key`"
  def fetch_persisted(%struct{}, key) do
    fetch_persisted(struct, key)
  end

  def fetch_persisted(map, key) when is_map(map) do
    Spark.Dsl.Transformer.fetch_persisted(map, key)
  end

  def fetch_persisted(resource, :module) when is_atom(resource) do
    {:ok, resource}
  end

  def fetch_persisted(resource, key) do
    fetch_persisted!(resource, key)
  end

  @doc """
  Get an option value for a section at a given path.

  Checks to see if it has been overridden via configuration.
  """
  def get_opt(resource, path, value, default \\ nil, configurable? \\ false) do
    case fetch_opt(resource, path, value, configurable?) do
      {:ok, value} -> value
      :error -> default
    end
  end

  def fetch_opt(resource, path, value, configurable? \\ false)

  def fetch_opt(%struct{}, path, value, configurable?) do
    fetch_opt(struct, path, value, configurable?)
  end

  def fetch_opt(map, path, value, configurable?) when not is_list(path) do
    fetch_opt(map, [path], value, configurable?)
  end

  def fetch_opt(map, path, value, configurable?) when is_map(map) do
    if configurable? do
      case get_opt_config(Spark.Dsl.Transformer.get_persisted(map, :module), path, value) do
        {:ok, value} ->
          {:ok, value}

        _ ->
          Spark.Dsl.Transformer.fetch_option(map, path, value)
      end
    else
      Spark.Dsl.Transformer.fetch_option(map, path, value)
    end
  end

  def fetch_opt(resource, path, key, configurable?) do
    if configurable? do
      case get_opt_config(resource, path, key) do
        {:ok, value} ->
          {:ok, value}

        _ ->
          do_fetch_opt(resource, path, key)
      end
    else
      do_fetch_opt(resource, path, key)
    end
  end

  defp do_fetch_opt(resource, path, key) do
    get_config_entry_with_fallback(
      resource,
      :lists.droplast(path),
      fn -> resource.fetch_opt(path, key) end,
      :options,
      &Keyword.fetch(&1, key),
      &Spark.Dsl.Transformer.fetch_option(&1, path, key)
    )
  end

  defdelegate doc(sections, depth \\ 1), to: Spark.CheatSheet
  defdelegate doc_index(sections, depth \\ 1), to: Spark.CheatSheet
  @doc false
  defdelegate doc_entity(entity, depth \\ 1), to: Spark.CheatSheet
  @doc false
  defdelegate doc_section(section, depth \\ 1), to: Spark.CheatSheet

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

  def set_docs(items) when is_list(items) do
    Enum.map(items, &set_docs/1)
  end

  def set_docs(%Spark.Dsl.Entity{} = entity) do
    entity
    |> Map.put(:docs, Spark.Dsl.Extension.doc_entity(entity))
    |> Map.put(
      :entities,
      Enum.map(entity.entities || [], fn {key, value} -> {key, set_docs(value)} end)
    )
  end

  def set_docs(%Spark.Dsl.Section{} = section) do
    section
    |> Map.put(:entities, set_docs(section.entities))
    |> Map.put(:sections, set_docs(section.sections))
    |> Map.put(:docs, Spark.Dsl.Extension.doc_section(section))
  end

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [
            sections: opts[:sections] || [],
            transformers: opts[:transformers] || [],
            verifiers: opts[:verifiers] || [],
            persisters: opts[:persisters] || [],
            dsl_patches: opts[:dsl_patches] || [],
            imports: opts[:imports] || [],
            module_prefix: opts[:module_prefix],
            add_extensions: opts[:add_extensions] || []
          ],
          generated: true do
      alias Spark.Dsl.Extension
      module_prefix = module_prefix || __MODULE__

      @_sections sections
                 |> Enum.map(&Extension.validate_and_transform_section(&1, __MODULE__))
                 |> Extension.set_docs()
      @_dsl_patches Extension.validate_and_transform_dsl_patches(dsl_patches, __MODULE__)
      @_transformers transformers
      @_verifiers verifiers
      @_persisters persisters
      @_imports imports
      @_add_extensions add_extensions

      @behaviour Extension
      Extension.build(__MODULE__, module_prefix, @_sections, @_dsl_patches)

      @after_verify Spark.Dsl.Extension

      @doc false
      def sections, do: @_sections
      @doc false
      def verifiers, do: [Spark.Dsl.Verifiers.VerifyEntityUniqueness | @_verifiers]
      @doc false
      def persisters, do: @_persisters
      @doc false
      def module_imports, do: @_imports
      @doc false
      def module_prefix, do: unquote(module_prefix) || __MODULE__
      @doc false
      def transformers, do: @_transformers
      @doc false
      def dsl_patches, do: @_dsl_patches
      @doc false
      def add_extensions, do: @_add_extensions
    end
  end

  @doc false
  def explain(spark, opts, extensions, dsl_state) do
    prefix =
      case spark.explain(dsl_state, opts) do
        nil ->
          ""

        explanation ->
          """
          #{explanation}

          """
      end

    extension_explanations =
      extensions
      |> Enum.filter(&function_exported?(&1, :explain, 1))
      |> Enum.map(& &1.explain(dsl_state))
      |> Enum.reject(fn docs -> docs in [nil, ""] end)
      |> Enum.join("\n\n")

    prefix <> extension_explanations
  end

  @doc false
  def prepare(extensions) do
    body =
      quote generated: true, location: :keep do
        Module.register_attribute(__MODULE__, :extensions, persist: true)
        Module.register_attribute(__MODULE__, :validate_sections, persist: true, accumulate: true)
        @extensions unquote(extensions)
        @persist {:spark_extensions, @extensions}
      end

    imports =
      for extension <- extensions || [] do
        extension = Macro.expand_once(extension, __ENV__)

        sections =
          try do
            extension.sections()
          rescue
            _ -> []
          end

        only_sections =
          sections
          |> Enum.reject(& &1.top_level?)
          |> Enum.map(&{&1.name, 1})

        quote generated: true, location: :keep do
          require Spark.Dsl.Extension
          import unquote(extension), only: unquote(only_sections)
        end
      end

    top_level_imports =
      Enum.flat_map(extensions || [], fn extension ->
        extension = Macro.expand_once(extension, __ENV__)

        sections =
          try do
            extension.sections()
          rescue
            _ ->
              []
          end

        sections
        |> Enum.filter(& &1.top_level?)
        |> Enum.flat_map(fn section ->
          section_mod_name =
            Spark.Dsl.Extension.section_mod_name(extension.module_prefix(), [], section)

          configured_imports =
            for module <- section.imports do
              quote generated: true do
                import unquote(module)
              end
            end

          entity_modules =
            Enum.map(
              section.entities,
              &Spark.Dsl.Extension.entity_mod_name(section_mod_name, [], [], &1)
            )

          entity_imports =
            for module <- entity_modules do
              quote generated: true do
                import unquote(module), only: :macros
              end
            end

          section_imports =
            for nested_section <- section.sections do
              module =
                Spark.Dsl.Extension.section_mod_name(
                  extension.module_prefix(),
                  [section.name],
                  nested_section
                )

              quote generated: true do
                import unquote(module), only: :macros
              end
            end

          opts_import =
            if Map.get(section, :schema, []) == [] do
              []
            else
              opts_module = Spark.Dsl.Extension.module_concat([section_mod_name, Options])

              [
                quote generated: true do
                  import unquote(opts_module)
                end
              ]
            end

          patch_modules =
            extensions
            |> Spark.Dsl.Extension.get_entity_dsl_patches([section.name])
            |> Enum.reject(&(&1 in entity_modules))

          patch_imports =
            for module <- patch_modules do
              quote generated: true do
                import unquote(module), only: :macros
              end
            end

          opts_import ++
            section_imports ++ entity_imports ++ patch_imports ++ configured_imports
        end)
      end)

    configured_imports =
      extensions
      |> Kernel.||([])
      |> Enum.flat_map(fn extension ->
        extension = Macro.expand_once(extension, __ENV__)
        extension.module_imports()
      end)
      |> Enum.map(fn module ->
        quote generated: true, location: :keep do
          import unquote(module)
        end
      end)

    [body | imports ++ configured_imports ++ top_level_imports]
  end

  @doc false
  defmacro set_state(additional_persisted_data, fragments, transform? \\ true) do
    quote generated: true,
          bind_quoted: [
            additional_persisted_data: additional_persisted_data,
            transform?: transform?,
            fragments: fragments
          ] do
      alias Spark.Dsl.Transformer

      persist =
        additional_persisted_data
        |> Keyword.put(:extensions, @extensions || [])
        |> Enum.into(%{})

      validate_sections =
        Module.get_attribute(__MODULE__, :validate_sections)
        |> List.wrap()
        |> Enum.concat(Enum.flat_map(fragments || [], & &1.validate_sections()))
        |> Enum.uniq_by(&elem(&1, 0))

      spark_dsl_config =
        {__MODULE__, :spark_sections}
        |> Process.get([])
        |> Enum.map(fn {_extension, section_path} ->
          {section_path,
           Process.get(
             {__MODULE__, :spark, section_path},
             Spark.Dsl.Extension.default_section_config()
           )}
        end)
        |> Enum.into(%{})
        |> Map.update(
          :persist,
          persist,
          &Map.merge(&1, persist)
        )
        |> Spark.Dsl.handle_fragments(fragments)

      spark_dsl_config =
        if transform? do
          Enum.reduce(validate_sections, spark_dsl_config, fn
            {section_path, validator, extension}, dsl_config ->
              validator.__set_and_validate_options__(
                dsl_config,
                section_path,
                __MODULE__,
                extension
              )
          end)
        else
          spark_dsl_config
        end

      for {key, _value} <- Process.get() do
        if is_tuple(key) and elem(key, 0) == __MODULE__ do
          Process.delete(key)
        end
      end

      transformers_to_run =
        if transform? do
          @extensions
          |> Enum.flat_map(& &1.transformers())
          |> Transformer.sort()
          |> Enum.reject(& &1.after_compile?())
          |> Enum.concat(Enum.flat_map(@extensions, & &1.persisters()))
        else
          []
        end

      spark_dsl_config =
        try do
          __MODULE__
          |> Spark.Dsl.Extension.run_transformers(
            transformers_to_run,
            spark_dsl_config,
            __ENV__
          )
          |> Spark.Dsl.Extension.await_tasks()
        rescue
          e in Spark.Error.DslError ->
            if e.location do
              Spark.Dsl.Extension.diagnostic_warning(e)

              reraise %{e | module: __MODULE__}, []
            else
              reraise %{e | module: __MODULE__},
                      (e.stacktrace && e.stacktrace.stacktrace) || __STACKTRACE__
            end
        end

      @spark_dsl_config spark_dsl_config
    end
  end

  if Application.compile_env(:spark, :skip_diagnostic_warnings, false) do
    @doc false
    def diagnostic_warning(_e) do
      :ok
    end
  else
    @doc false
    def diagnostic_warning(e) do
      Spark.Warning.warn(e.message, e.location)
    end
  end

  @doc false
  def await_tasks(dsl) do
    Task.await_many(dsl[:persist][:spark_compile_tasks] || [], :infinity)
    %{dsl | persist: Map.delete(dsl.persist, :spark_compile_tasks)}
  end

  def run_transformers(mod, transformers, spark_dsl_config, env) do
    spark_dsl_config = Map.update!(spark_dsl_config, :persist, &Map.put(&1, :env, env))

    Enum.reduce_while(transformers, spark_dsl_config, fn transformer, dsl ->
      result =
        try do
          transformer.transform(dsl)
        rescue
          e in Spark.Error.DslError ->
            reraise %{e | module: mod},
                    (e.stacktrace && e.stacktrace.stacktrace) || __STACKTRACE__

          e ->
            reraise "Exception in transformer #{inspect(transformer)} on #{inspect(mod)}: \n\n#{Exception.message(e)}",
                    __STACKTRACE__
        end

      case result do
        :ok ->
          {:cont, dsl}

        :halt ->
          {:halt, dsl}

        {:warn, new_dsl, warnings} ->
          warnings
          |> List.wrap()
          |> Enum.each(fn
            {warning, location} ->
              Spark.Warning.warn(warning, location, Macro.Env.stacktrace(env))

            warning ->
              Spark.Warning.warn(warning, nil, Macro.Env.stacktrace(env))
          end)

          {:cont, new_dsl}

        {:ok, new_dsl} ->
          {:cont, new_dsl}

        {:error, %Spark.Error.DslError{} = e} ->
          raise_transformer_error(transformer, %{e | module: mod})

        {:error, error} ->
          raise_transformer_error(transformer, error)

        other ->
          raise """
          Invalid return from transformer: #{inspect(transformer)}

          Expected one of:

          * `:ok`
          * `:halt`
          * `{:warn, dsl_state, warnings}`
          * `{:ok, dsl_state}`
          * `{:error, error}`

          Got:

          #{inspect(other)}
          """
      end
    end)
  end

  defp raise_transformer_error(
         _transformer,
         %Spark.Error.DslError{stacktrace: %{stacktrace: stacktrace}} = error
       )
       when not is_nil(stacktrace) do
    reraise error, stacktrace
  end

  defp raise_transformer_error(transformer, error) do
    if is_exception(error) do
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

  defp validate_and_transform_dsl_patch(
         %Spark.Dsl.Patch.AddEntity{entity: entity} = dsl_patch,
         module
       ) do
    entity = validate_and_transform_entity(entity, [], module)
    %{dsl_patch | entity: entity}
  end

  defp validate_and_transform_dsl_patch(dsl_patch, _module), do: dsl_patch

  def validate_and_transform_dsl_patches(dsl_patches, module \\ nil) do
    Enum.map(dsl_patches, &validate_and_transform_dsl_patch(&1, module))
  end

  @doc false
  defmacro build(extension, module_prefix, sections, dsl_patches) do
    quote generated: true,
          bind_quoted: [
            dsl_patches: dsl_patches,
            sections: sections,
            extension: extension,
            module_prefix: module_prefix
          ] do
      alias Spark.Dsl.Extension

      {:ok, agent} = Agent.start_link(fn -> [] end)
      agent_and_pid = {agent, self()}

      Enum.each(sections, fn section ->
        Spark.Dsl.Extension.async_compile(agent_and_pid, fn ->
          Extension.build_section(
            agent_and_pid,
            extension,
            section,
            [],
            module_prefix
          )
        end)
      end)

      Enum.each(dsl_patches, fn
        %Spark.Dsl.Patch.AddEntity{
          section_path: section_path,
          entity: entity
        } ->
          Spark.Dsl.Extension.async_compile(agent_and_pid, fn ->
            Extension.build_entity(
              agent_and_pid,
              module_prefix,
              extension,
              section_path,
              entity,
              [],
              [],
              []
            )
          end)
      end)

      Spark.Dsl.Extension.await_all_tasks(agent)

      Agent.stop(agent)
    end
  end

  @doc false
  def validate_and_transform_section(
        %Spark.Dsl.Section{entities: entities} = section,
        module \\ nil
      ) do
    %{
      section
      | entities: Enum.map(entities, &validate_and_transform_entity(&1, [section.name], module))
    }
  end

  @doc """
  Validates and transforms an entity structure, ensuring nested entities are properly formatted.

  This function recursively processes a DSL entity and its nested entities, converting
  single entity values to lists where needed and validating the structure.

  ## Parameters

  - `entity` - The entity to validate and transform
  - `path` - The current path in the DSL structure (for error reporting)
  - `module` - The module context (for error reporting)

  ## Returns

  Returns the transformed entity with normalized nested entity structures.
  """
  def validate_and_transform_entity(entity, path \\ [], module \\ nil)

  def validate_and_transform_entity(%Spark.Dsl.Entity{} = entity, path, module) do
    # Include the entity's name in the path when processing nested entities
    nested_path = if entity.name, do: path ++ [entity.name], else: path

    entities =
      entity.entities
      |> List.wrap()
      |> Enum.map(fn
        {key, %Spark.Dsl.Entity{} = value} ->
          {key, [validate_and_transform_entity(value, nested_path ++ [key], module)]}

        {key, values} when is_list(values) ->
          # Already a list, keep as is
          {key,
           Enum.map(values, &validate_and_transform_entity(&1, nested_path ++ [key], module))}

        {key, value} ->
          # Non-entity, non-list value - this is invalid
          raise Spark.Error.DslError,
            module: module,
            path: nested_path ++ [key],
            message:
              "nested entity '#{key}' must be an entity or list of entities, got: #{inspect(value)}"
      end)

    %{entity | entities: entities}
  end

  def validate_and_transform_entity(_, path, module) do
    raise Spark.Error.DslError,
      module: module,
      path: path,
      message: "Invalid entity structure"
  end

  @doc false
  defmacro build_section(
             agent,
             extension,
             section,
             path \\ [],
             module_prefix \\ nil
           ) do
    quote bind_quoted: [
            agent: agent,
            section: section,
            path: path,
            extension: extension,
            module_prefix: module_prefix
          ],
          generated: true do
      alias Spark.Dsl

      {section_modules, entity_modules, opts_module} =
        Dsl.Extension.do_build_section(
          agent,
          module_prefix,
          extension,
          section,
          path
        )

      @doc false
      def __set_and_validate_options__(
            dsl_config,
            unquote(path ++ [section.name]) = section_path,
            module,
            extension
          ) do
        schema = unquote(Macro.escape(Map.get(section, :schema, [])))

        dsl_config
        |> Map.put_new(section_path, Spark.Dsl.Extension.default_section_config())
        |> Map.update!(section_path, fn config ->
          validated_opts =
            case Spark.Options.validate(Keyword.new(config[:opts] || []), schema) do
              {:ok, opts} ->
                opts

              {:error, error} ->
                raise Spark.Error.DslError,
                  module: module,
                  message: error,
                  path: section_path,
                  location: config[:section_anno]
            end

          # Preserve all existing fields, just update opts
          %{config | opts: validated_opts}
        end)
      end

      unless section.top_level? && path == [] do
        defmacro unquote(section.name)(body) do
          opts_module = unquote(opts_module)
          section_path = unquote(path ++ [section.name])
          extension = unquote(extension)

          section_anno = Spark.Dsl.Extension.macro_env_anno(__CALLER__, body[:do])

          entity_modules = unquote(entity_modules)

          patch_modules =
            Spark.Dsl.Extension.get_attribute(__CALLER__.module, :extensions)
            |> Spark.Dsl.Extension.get_entity_dsl_patches(section_path)
            |> Enum.reject(&(&1 in entity_modules))

          section_imports =
            for module <- unquote(section_modules) do
              quote generated: true do
                import unquote(module), only: :macros
              end
            end

          import_statements =
            Enum.concat([
              unquote(section.imports),
              unquote(entity_modules),
              patch_modules,
              unquote(section_modules),
              unquote(List.wrap(opts_module))
            ])
            |> Spark.Dsl.Extension.Imports.import_solving_conflicts(__CALLER__)

          all_the_code =
            import_statements ++
              [
                quote generated: true do
                  unquote(body[:do])

                  current_sections = Process.get({__MODULE__, :spark_sections}, [])

                  unless {unquote(extension), unquote(section_path)} in current_sections do
                    Process.put({__MODULE__, :spark_sections}, [
                      {unquote(extension), unquote(section_path)} | current_sections
                    ])
                  end

                  # Store section annotation
                  current_config =
                    Process.get(
                      {__MODULE__, :spark, unquote(section_path)},
                      Spark.Dsl.Extension.default_section_config()
                    )

                  Process.put(
                    {__MODULE__, :spark, unquote(section_path)},
                    %{current_config | section_anno: unquote(Macro.escape(section_anno))}
                  )

                  @validate_sections {unquote(section_path), unquote(__MODULE__),
                                      unquote(extension)}
                end
              ]

          after_define = unquote(section.after_define)

          quote do
            with do
              unquote(all_the_code)

              case unquote(after_define) do
                nil ->
                  :ok

                {m, f} ->
                  Code.eval_quoted(apply(m, f, []), [], __ENV__)
              end
            end
          end
        end
      end
    end
  end

  @doc false
  def entity_mod_name(mod, nested_entity_path, section_path, entity) do
    nested_entity_parts = Enum.map(nested_entity_path, &Macro.camelize(to_string(&1)))
    section_path_parts = Enum.map(section_path, &Macro.camelize(to_string(&1)))

    mod_parts =
      Enum.concat([
        [mod],
        section_path_parts,
        nested_entity_parts,
        [Macro.camelize(to_string(entity.name))]
      ])

    Spark.Dsl.Extension.module_concat(mod_parts)
  end

  @doc false
  def section_mod_name(mod, path, section) do
    nested_mod_name =
      path
      |> Enum.drop(1)
      |> Enum.map(fn nested_section_name ->
        Macro.camelize(to_string(nested_section_name))
      end)

    Spark.Dsl.Extension.module_concat(
      [mod | nested_mod_name] ++ [Macro.camelize(to_string(section.name))]
    )
  end

  @doc false
  def do_build_section(agent, mod, extension, section, path) do
    opts_mod_name =
      if section.schema == [] do
        nil
      else
        Spark.Dsl.Extension.module_concat([mod, Macro.camelize(to_string(section.name)), Options])
      end

    entity_modules =
      Enum.reduce(section.entities, [], fn entity, entity_modules ->
        entity = %{
          entity
          | auto_set_fields: Keyword.merge(section.auto_set_fields, entity.auto_set_fields)
        }

        entity_mod =
          build_entity(
            agent,
            mod,
            extension,
            path ++ [section.name],
            entity,
            section.deprecations,
            []
          )

        if entity.recursive_as do
          [entity_mod | entity_modules]
        else
          [entity_mod | entity_modules]
        end
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
          Spark.Dsl.Extension.module_concat(
            [mod | nested_mod_name] ++ [Macro.camelize(to_string(nested_section.name))]
          )

        Spark.Dsl.Extension.async_compile(agent, fn ->
          {:module, module, _, _} =
            Module.create(
              mod_name,
              quote generated: true do
                @moduledoc false
                alias Spark.Dsl

                require Dsl.Extension

                Dsl.Extension.build_section(
                  unquote(agent),
                  unquote(extension),
                  unquote(Macro.escape(nested_section)),
                  unquote(path ++ [section.name]),
                  unquote(mod)
                )
              end,
              Macro.Env.location(__ENV__)
            )

          module
        end)

        mod_name
      end)

    if opts_mod_name do
      Module.create(
        opts_mod_name,
        quote generated: true,
              bind_quoted: [
                section: Macro.escape(section),
                section_path: path ++ [section.name],
                extension: extension
              ] do
          @moduledoc false
          for {field, config} <- section.schema do
            defmacro unquote(field)(value) do
              section_path = unquote(Macro.escape(section_path))
              field = unquote(Macro.escape(field))
              extension = unquote(extension)
              type = unquote(Macro.escape(config[:type]))
              deprecations = unquote(Macro.escape(section.deprecations))

              opt_anno = Spark.Dsl.Extension.macro_env_anno(__CALLER__, nil)

              Spark.Dsl.Extension.maybe_deprecated(
                field,
                deprecations,
                section_path,
                __CALLER__
              )

              {value, function} =
                Spark.Dsl.Extension.SectionOption.value_and_function(
                  value,
                  field,
                  type,
                  __CALLER__,
                  unquote(Macro.escape(section.modules)),
                  unquote(Macro.escape(section.no_depend_modules))
                )

              quote generated: true do
                unquote(function)

                Spark.Dsl.Extension.SectionOption.set_section_option(
                  __MODULE__,
                  unquote(extension),
                  unquote(section_path),
                  unquote(field),
                  unquote(value),
                  unquote(Macro.escape(opt_anno))
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
  # sobelow_skip ["DOS.BinToAtom"]
  def build_entity(
        agent,
        mod,
        extension,
        section_path,
        entity,
        deprecations,
        nested_entity_path,
        nested_key \\ nil
      ) do
    mod_name = entity_mod_name(mod, nested_entity_path, section_path, entity)

    options_mod_name = Spark.Dsl.Extension.module_concat(mod_name, "Options")

    nested_entity_mods =
      Enum.reduce(entity.entities, [], fn
        {key, entities}, nested_entity_mods ->
          entities
          |> Enum.reduce(
            nested_entity_mods,
            fn nested_entity, nested_entity_mods ->
              entity_mod =
                build_entity(
                  agent,
                  mod,
                  extension,
                  section_path,
                  nested_entity,
                  nested_entity.deprecations,
                  nested_entity_path ++ [entity.name],
                  key
                )

              [entity_mod | nested_entity_mods]
            end
          )
      end)

    Spark.Dsl.Extension.build_entity_options(
      options_mod_name,
      entity,
      nested_entity_path
    )

    args =
      entity.args
      |> Enum.map(fn
        {:optional, name} ->
          {:optional, name, nil}

        other ->
          other
      end)
      |> Enum.map(fn
        {:optional, name, default} ->
          {:\\, [], [{name, [], Elixir}, {:__spark_not_specified__, default}]}

        other ->
          Macro.var(other, Elixir)
      end)

    entity_args = Spark.Dsl.Entity.arg_names(entity)
    arg_vars = Enum.map(entity_args, &Macro.var(&1, Elixir))

    async_compile(agent, fn ->
      Module.create(
        mod_name,
        quote generated: true,
              location: :keep,
              bind_quoted: [
                extension: extension,
                entity: Macro.escape(entity),
                args: Macro.escape(args),
                arg_vars: Macro.escape(arg_vars),
                section_path: Macro.escape(section_path),
                entity_args: Macro.escape(entity_args),
                options_mod_name: Macro.escape(options_mod_name),
                nested_entity_mods: Macro.escape(nested_entity_mods),
                nested_entity_path: Macro.escape(nested_entity_path),
                deprecations: deprecations,
                nested_key: nested_key,
                mod: mod
              ] do
          def __build__(module, opts, nested_entities, anno, opts_anno) do
            case Spark.Dsl.Entity.build(
                   unquote(Macro.escape(entity)),
                   opts,
                   nested_entities,
                   anno,
                   opts_anno
                 ) do
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
                    is_exception(error) ->
                      Exception.message(error)

                    is_binary(error) ->
                      error

                    true ->
                      inspect(error)
                  end

                raise Spark.Error.DslError,
                  module: module,
                  message: message,
                  path: unquote(section_path) ++ additional_path,
                  location: anno
            end
          end

          @moduledoc false
          defmacro unquote(entity.name)(unquote_splicing(args), opts \\ nil) do
            section_path = unquote(Macro.escape(section_path))
            entity_schema = unquote(Macro.escape(entity.schema))
            entity = unquote(Macro.escape(entity))
            entity_name = unquote(Macro.escape(entity.name))
            entity_args = unquote(Macro.escape(entity_args))
            arg_vars = unquote(Macro.escape(arg_vars))
            entity_deprecations = unquote(entity.deprecations)
            options_mod_name = unquote(Macro.escape(options_mod_name))
            source = unquote(__MODULE__)
            extension = unquote(Macro.escape(extension))
            nested_entity_mods = unquote(Macro.escape(nested_entity_mods))
            nested_entity_path = unquote(Macro.escape(nested_entity_path))
            deprecations = unquote(deprecations)
            nested_key = unquote(nested_key)
            mod = unquote(mod)

            require Spark.Dsl.Extension

            anno = Spark.Dsl.Extension.macro_env_anno(__CALLER__, opts[:do])

            Spark.Dsl.Extension.maybe_deprecated(
              entity.name,
              deprecations,
              section_path ++ nested_entity_path,
              __CALLER__
            )

            {args_without_opts, opts} =
              entity_args
              |> Enum.zip([unquote_splicing(arg_vars)])
              |> Spark.Dsl.Extension.escape_quoted(entity_schema, __CALLER__)
              |> Spark.Dsl.Extension.shuffle_opts_to_end(
                entity.args,
                entity_schema,
                entity.entities,
                opts
              )

            if not Keyword.keyword?(opts) do
              raise ArgumentError,
                    "Expected an options list in #{entity.name} got #{Macro.to_string(opts)}"
            end

            {opts, opt_funs} =
              Enum.reduce(opts, {[], []}, fn {key, value}, {keyword, opt_funs} ->
                {value, function} = Spark.CodeHelpers.lift_functions(value, key, __CALLER__)
                keyword = [{key, value} | keyword]

                if function do
                  {keyword, [function | opt_funs]}
                else
                  {keyword, opt_funs}
                end
              end)

            {arg_values, funs} =
              args_without_opts
              |> Enum.reduce({[], []}, fn {key, arg_value}, {args, funs} ->
                Spark.Dsl.Extension.maybe_deprecated(
                  key,
                  entity_deprecations,
                  nested_entity_path,
                  __CALLER__
                )

                arg_value =
                  cond do
                    key in entity.modules ->
                      Spark.Dsl.Extension.expand_alias(arg_value, __CALLER__)

                    key in entity.no_depend_modules ->
                      Spark.Dsl.Extension.expand_alias_no_require(arg_value, __CALLER__)

                    true ->
                      arg_value
                  end

                {arg_value, new_function} =
                  Spark.CodeHelpers.lift_functions(arg_value, key, __CALLER__)

                if is_nil(arg_value) &&
                     Enum.any?(
                       entity.args,
                       &(is_tuple(&1) && elem(&1, 0) == :optional && elem(&1, 1) == key)
                     ) do
                  {args, funs}
                else
                  {[{key, arg_value} | args], [new_function | funs]}
                end
              end)

            other_extension_modules =
              Spark.Dsl.Extension.get_attribute(__CALLER__.module, :extensions)
              |> Enum.reject(&(&1 == extension))
              |> Enum.flat_map(fn extension ->
                extension.dsl_patches()
                |> Enum.filter(fn
                  %Spark.Dsl.Patch.AddEntity{section_path: ^section_path} ->
                    true

                  _ ->
                    false
                end)
                |> Enum.map(& &1.entity)
                |> Enum.concat(
                  Spark.Dsl.Extension.get_recursive_entities_for_path(
                    extension.sections(),
                    section_path
                  )
                )
                |> Enum.uniq_by(& &1.name)
                |> Enum.map(&{extension, &1})
              end)
              |> Enum.map(fn {extension, entity} ->
                Spark.Dsl.Extension.entity_mod_name(
                  extension.module_prefix(),
                  [],
                  section_path,
                  entity
                )
              end)

            opts =
              Enum.map(opts, fn {key, value} ->
                Spark.Dsl.Extension.maybe_deprecated(
                  key,
                  entity_deprecations,
                  nested_entity_path,
                  __CALLER__
                )

                cond do
                  key in entity.modules ->
                    {key, Spark.Dsl.Extension.expand_alias(value, __CALLER__)}

                  key in entity.no_depend_modules ->
                    {key, Spark.Dsl.Extension.expand_alias_no_require(value, __CALLER__)}

                  true ->
                    {key, value}
                end
              end)

            entity_keys =
              entity.entities
              |> Enum.map(&elem(&1, 0))
              |> Enum.uniq()

            import_statements =
              Enum.concat([
                other_extension_modules,
                entity.imports,
                List.wrap(options_mod_name),
                nested_entity_mods
              ])
              |> Spark.Dsl.Extension.Imports.import_solving_conflicts(__CALLER__)

            code =
              import_statements ++
                funs ++
                opt_funs ++
                [
                  quote generated: true do
                    handle_data =
                      Spark.Dsl.Extension.Entity.setup(
                        __MODULE__,
                        unquote(entity.recursive_as),
                        unquote(nested_key),
                        unquote(Keyword.delete(opts, :do)),
                        unquote(arg_values),
                        unquote(Macro.escape(anno))
                      )

                    unquote(opts[:do])

                    Spark.Dsl.Extension.Entity.handle(
                      __MODULE__,
                      unquote(section_path),
                      unquote(entity_keys),
                      unquote(__MODULE__),
                      unquote(extension),
                      handle_data
                    )
                  end
                ]

            quote generated: true do
              with do
                unquote(code)
              end
            end
          end
        end,
        Macro.Env.location(__ENV__)
      )
    end)

    mod_name
  end

  @doc false
  def build_entity_options(
        module_name,
        entity,
        nested_entity_path
      ) do
    entity =
      case entity.recursive_as do
        nil ->
          entity

        recursive_as ->
          %{entity | entities: Keyword.put_new(entity.entities || [], recursive_as, [])}
      end

    Module.create(
      module_name,
      quote generated: true,
            bind_quoted: [
              entity: Macro.escape(entity),
              nested_entity_path: nested_entity_path
            ] do
        @moduledoc false

        for {key, config} <- entity.schema,
            !Spark.Dsl.Extension.required_arg?(key, entity.args) do
          defmacro unquote(key)(value) do
            Spark.Dsl.Extension.maybe_deprecated(
              unquote(key),
              unquote(entity.deprecations),
              unquote(nested_entity_path),
              __CALLER__
            )

            {value, function} =
              Spark.Dsl.Extension.EntityOption.value_and_function(
                value,
                unquote(key),
                unquote(Macro.escape(config[:type])),
                __CALLER__,
                unquote(entity.modules),
                unquote(entity.no_depend_modules)
              )

            key = unquote(key)

            quote generated: true do
              unquote(function)

              Spark.Dsl.Extension.EntityOption.set_entity_option(
                __MODULE__,
                unquote(key),
                unquote(value),
                unquote(Spark.Dsl.Extension.macro_env_anno(__CALLER__, nil))
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
  def escape_quoted(options, schema, _caller) do
    Enum.map(options, fn {name, value} ->
      case schema[name][:type] do
        :quoted ->
          {name, Macro.escape(value)}

        _ ->
          {name, value}
      end
    end)
  end

  @doc false
  def required_arg?(key, args) do
    key in args
  end

  @doc false
  def maybe_deprecated(field, deprecations, path, env) do
    if Keyword.has_key?(deprecations, field) do
      prefix =
        case Enum.join(path) do
          "" -> ""
          path -> "#{path}."
        end

      Spark.Warning.warn_deprecated(
        "The #{prefix}#{field} key",
        "will be deprecated in an upcoming release!\n\n#{deprecations[field]}",
        nil,
        Macro.Env.stacktrace(env)
      )
    end
  end

  def expand_alias(ast, %Macro.Env{} = env) do
    Macro.prewalk(ast, fn
      {:%, meta, [left, right]} ->
        {:%, meta, [left, expand_alias(right, env)]}

      {:__aliases__, _, _} = node ->
        # This is basically just `Macro.expand_literal/2`
        do_expand(node, %{env | function: {:module_info, 0}})

      other ->
        other
    end)
  end

  def expand_alias_no_require(ast, env) do
    Macro.prewalk(ast, fn
      {:%, meta, [{:__MODULE__, _, _} = node, right]} ->
        {:struct!, meta, [node, right]}

      {:%, meta, [left, right]} ->
        {:%, meta, [left, expand_alias_no_require(right, env)]}

      {:__aliases__, _, parts} = node ->
        try do
          # Dear Jose, I know you would hate this if you saw it 😢.
          # I will propose a utility for doing this with a public
          # API soon, but this had to do for the short term.

          Enum.flat_map(env.aliases, fn {alias_value, _destination} ->
            if List.last(Module.split(alias_value)) ==
                 to_string(List.first(parts)) do
              [alias_value]
            else
              []
            end
          end)
          |> Enum.each(fn alias_used ->
            env
            |> Macro.Env.lookup_alias_as(alias_used)
            |> Enum.each(fn alias_used ->
              Kernel.LexicalTracker.alias_dispatch(env.lexical_tracker, alias_used)
            end)

            Kernel.LexicalTracker.alias_dispatch(env.lexical_tracker, alias_used)
          end)
        rescue
          _e ->
            :ok
        end

        do_expand(node, %{env | lexical_tracker: nil})

      other ->
        other
    end)
  end

  @doc false
  def do_expand(ast, env) do
    {ast, :ok} = expand_literals(ast, :ok, fn node, :ok -> {Macro.expand(node, env), :ok} end)
    ast
  end

  def expand_literals(ast, acc, fun)

  def expand_literals({:__aliases__, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)

    if :lists.all(&is_atom/1, args) do
      fun.({:__aliases__, meta, args}, acc)
    else
      {{:__aliases__, meta, args}, acc}
    end
  end

  def expand_literals({:__MODULE__, _meta, ctx} = node, acc, fun) when is_atom(ctx) do
    fun.(node, acc)
  end

  def expand_literals({:%, meta, [left, right]}, acc, fun) do
    {left, acc} = expand_literals(left, acc, fun)
    {right, acc} = expand_literals(right, acc, fun)
    {{:%, meta, [left, right]}, acc}
  end

  def expand_literals({:%{}, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)
    {{:%{}, meta, args}, acc}
  end

  def expand_literals({:{}, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)
    {{:{}, meta, args}, acc}
  end

  def expand_literals({left, right}, acc, fun) do
    {left, acc} = expand_literals(left, acc, fun)
    {right, acc} = expand_literals(right, acc, fun)
    {{left, right}, acc}
  end

  def expand_literals(list, acc, fun) when is_list(list) do
    :lists.mapfoldl(&expand_literals(&1, &2, fun), acc, list)
  end

  def expand_literals(
        {{:., _, [{:__aliases__, _, [:Application]}, :compile_env]} = node, meta,
         [app, key, default]},
        acc,
        fun
      ) do
    {default, acc} = expand_literals(default, acc, fun)
    {{node, meta, [app, key, default]}, acc}
  end

  def expand_literals(term, acc, _fun), do: {term, acc}

  def monotonic_number(key) do
    (Process.get({:spark_monotonic_number, key}) || -1) + 1
  end

  def spark_function_info({:or, types}) do
    Enum.find_value(types, &spark_function_info/1)
  end

  def spark_function_info({:spark_function_behaviour, _, _, {module, arity}}) do
    {module, arity}
  end

  def spark_function_info({:spark_function_behaviour, _, {module, arity}}) do
    {module, arity}
  end

  def spark_function_info(_) do
    nil
  end

  def shuffle_opts_to_end(keyword, entity_args, schema, entities, nil) do
    default_values =
      entity_args
      |> Enum.reduce(%{}, fn
        {:optional, name}, defaults ->
          Map.put(defaults, name, nil)

        {:optional, name, default}, defaults ->
          Map.put(defaults, name, default)

        _, defaults ->
          defaults
      end)

    entity_arg_names =
      Enum.map(entity_args, fn
        {:optional, name, _} -> name
        {:optional, name} -> name
        name -> name
      end)

    if Keyword.drop(schema, entity_arg_names) == [] && entities == [] do
      {
        replace_not_specified(keyword),
        []
      }
    else
      last_specified_option =
        entity_arg_names
        |> Enum.reverse()
        |> Enum.map(fn name -> {name, Keyword.get(keyword, name)} end)
        |> Enum.drop_while(fn
          {_k, {:__spark_not_specified__, _}} ->
            true

          _ ->
            false
        end)
        |> Enum.at(0)

      with {to_take_default, last_specified_value} <- last_specified_option,
           true <- Keyword.keyword?(last_specified_value) do
        keyword =
          Enum.flat_map(keyword, fn {k, v} ->
            if k == to_take_default do
              if Keyword.has_key?(last_specified_value, k) do
                []
              else
                if Map.has_key?(default_values, to_take_default) do
                  [{k, {:__spark_not_specified__, Map.get(default_values, to_take_default)}}]
                else
                  []
                end
              end
            else
              [{k, v}]
            end
          end)

        shift_right_until_index =
          Enum.find_index(entity_arg_names, fn name -> name == to_take_default end)

        replaced_required_arg? =
          Enum.any?(entity_args, &(&1 == to_take_default))

        has_optional_args_before? =
          entity_args
          |> Enum.take(shift_right_until_index)
          |> Enum.any?(fn
            name ->
              not is_atom(name)
          end)

        keyword =
          if has_optional_args_before? && replaced_required_arg? do
            shifting =
              Enum.take(entity_arg_names, shift_right_until_index + 1)

            case shifting do
              [] ->
                keyword

              shifting ->
                shift(keyword, shifting, default_values)
            end
          else
            keyword
          end

        {
          replace_not_specified(keyword),
          last_specified_value
        }
      else
        _ ->
          {
            replace_not_specified(keyword),
            []
          }
      end
    end
  end

  def shuffle_opts_to_end(keyword, _entity_args, _, _, opts) do
    {replace_not_specified(keyword), opts}
  end

  defp replace_not_specified(keyword) do
    Enum.map(keyword, fn
      {k, {:__spark_not_specified__, v}} ->
        {k, v}

      {k, v} ->
        {k, v}
    end)
  end

  defp shift(keyword, [k | _] = list, default_values) do
    do_shift(keyword, list, {:__spark_not_specified__, Map.get(default_values, k)})
  end

  defp do_shift(keyword, [], _), do: keyword

  defp do_shift(keyword, [k], current_value) do
    set_preserving_order(keyword, k, current_value)
  end

  defp do_shift(keyword, [k | rest], current_value) do
    new_value = Keyword.get(keyword, k)

    keyword
    |> set_preserving_order(k, current_value)
    |> do_shift(rest, new_value)
  end

  defp set_preserving_order(keyword, k, v) do
    if Keyword.has_key?(keyword, k) do
      Enum.map(keyword, fn
        {^k, _} ->
          {k, v}

        {k, v} ->
          {k, v}
      end)
    else
      keyword ++ [{k, v}]
    end
  end

  @doc false
  def await_all_tasks(agent) do
    Agent.get_and_update(
      agent,
      fn state ->
        case state do
          [] ->
            {:stop, []}

          funs ->
            {{:tasks, funs}, []}
        end
      end,
      :infinity
    )
    |> case do
      :stop ->
        :ok

      {:tasks, funs} ->
        funs
        |> Enum.map(&do_async_compile/1)
        |> Task.await_many(:infinity)

        await_all_tasks(agent)
    end
  end

  @doc false
  def async_compile({agent, _pid}, func) do
    if @parallel_compile do
      Agent.update(
        agent,
        fn funs ->
          [func | funs]
        end,
        :infinity
      )
    else
      func.()
    end
  end

  @doc false
  def module_concat(value1, value2) do
    module_concat([value1, value2])
  end

  def module_concat(values) do
    values
    |> List.wrap()
    |> List.flatten()
    |> Enum.map(fn value ->
      value
      |> to_string()
      |> String.replace("?", "QuestionMark")
    end)
    |> Module.concat()
  end

  @doc false
  def do_async_compile(fun) do
    case :erlang.get(:elixir_compiler_info) do
      :undefined ->
        Task.async(fun)

      _ ->
        Kernel.ParallelCompiler.async(fun)
    end
  end

  def get_entity_dsl_patches(extensions, section_path) do
    alias Spark.Dsl.Patch.AddEntity

    section =
      extensions
      |> Enum.flat_map(& &1.sections())
      |> get_section_at_path(section_path, %{patchable?: false, entities: []})

    for extension <- extensions,
        %AddEntity{section_path: ^section_path, entity: entity} <- extension.dsl_patches(),
        reduce: [] do
      entities ->
        if section.patchable? || Enum.any?(section.entities, &(&1.target == entity.target)) do
          entities ++ [entity_mod_name(extension.module_prefix(), [], section_path, entity)]
        else
          Spark.Warning.warn(
            "Attempt to add an entity with a patch to a non-patchable DSL section that has no compatible entities"
          )

          entities
        end
    end
  end

  defp get_section_at_path(sections, [name], default) do
    sections
    |> Enum.filter(&(&1.name == name))
    |> case do
      [section] -> section
      _ -> default
    end
  end

  defp get_section_at_path(sections, [head | tail], default) do
    sections
    |> Enum.filter(&(&1.name == head))
    |> Enum.flat_map(& &1.sections)
    |> get_section_at_path(tail, default)
  end

  def get_recursive_entities_for_path(sections, [name]) do
    sections
    |> Enum.find(&(&1.name == name))
    |> case do
      %{entities: entities} ->
        entities

      _ ->
        []
    end
    |> Enum.filter(& &1.recursive_as)
  end

  def get_recursive_entities_for_path(sections, [name | rest]) do
    sections
    |> Enum.filter(&(&1.name == name))
    |> get_recursive_entities_for_path(rest)
  end

  @spec default_section_config() :: %{
          section_anno: :erl_anno.anno() | nil,
          entities: list(),
          opts: Keyword.t(),
          opts_anno: Keyword.t(:erl_anno.anno() | nil)
        }
  def default_section_config,
    do: %{
      section_anno: nil,
      entities: [],
      opts: [],
      opts_anno: []
    }

  @spec get_config_entry_with_fallback(
          module(),
          list(atom()),
          (-> result),
          atom(),
          (any() -> result),
          (map() -> result)
        ) :: result
        when result: term()
  defp get_config_entry_with_fallback(
         resource,
         path,
         direct_fn,
         process_key,
         process_extractor,
         transformer_fn
       ) do
    direct_fn.()
  rescue
    _ in [UndefinedFunctionError, ArgumentError] ->
      try do
        case Process.get({resource, :spark, path}) do
          %{^process_key => value} ->
            process_extractor.(value)

          _ ->
            dsl = Module.get_attribute(resource, :spark_dsl_config) || %{}
            transformer_fn.(dsl)
        end
      rescue
        ArgumentError ->
          try do
            direct_fn.()
          rescue
            _ ->
              reraise ArgumentError,
                      """
                      `#{inspect(resource)}` is not a Spark DSL module.
                      """,
                      __STACKTRACE__
          end
      end
  end

  @spec macro_env_anno(env :: Macro.Env.t(), do_block :: Macro.t()) :: :erl_anno.anno()
  def macro_env_anno(env, do_block) do
    if Code.get_compiler_option(:debug_info) do
      anno = :erl_anno.new(env.line)
      anno = :erl_anno.set_file(String.to_charlist(env.file), anno)
      maybe_set_end_location(anno, do_block)
    end
  end

  @spec maybe_set_end_location(:erl_anno.anno(), Macro.t() | nil) :: :erl_anno.anno()
  if function_exported?(:erl_anno, :set_end_location, 2) do
    defp maybe_set_end_location(anno, do_block)
    defp maybe_set_end_location(anno, nil), do: anno

    defp maybe_set_end_location(anno, {_, meta, _}) when is_list(meta) do
      end_of_expression_meta = Keyword.get(meta, :end_of_expression, [])

      location = ast_meta_to_location(end_of_expression_meta) || ast_meta_to_location(meta)

      if location do
        :erl_anno.set_end_location(location, anno)
      else
        anno
      end
    end

    defp ast_meta_to_location(meta) when is_list(meta) do
      case {Keyword.fetch(meta, :line), Keyword.fetch(meta, :column)} do
        {{:ok, line}, {:ok, col}} -> {line, col}
        {{:ok, line}, :error} -> line
        {:error, :error} -> nil
      end
    end
  else
    defp maybe_set_end_location(anno, _do_block), do: anno
  end

  def __after_verify__(module) do
    dsl_patch_structs =
      for patch <- module.dsl_patches(), entity <- patch_entities(patch), do: entity.target

    section_structs =
      for section <- module.sections(), entity <- section_entities(section), do: entity.target

    (dsl_patch_structs ++ section_structs)
    |> Enum.uniq()
    |> Enum.reject(&Map.has_key?(&1.__struct__(), :__spark_metadata__))
    |> Enum.each(
      &Spark.Warning.warn_deprecated(
        "Entity without __spark_metadata__ field",
        "Entity #{inspect(&1)} does not define a `__spark_metadata__` field. " <>
          "This field is required to access source annotations. " <>
          "Add `__spark_metadata__: nil` to the defstruct for #{inspect(&1)}."
      )
    )

    :ok
  end

  defp patch_entities(%Spark.Dsl.Patch.AddEntity{entity: entity}) do
    nested_entities(entity)
  end

  defp section_entities(%Spark.Dsl.Section{
         entities: entities,
         sections: sections
       }) do
    [
      entities,
      Enum.flat_map(sections, &section_entities/1)
    ]
    |> Enum.concat()
    |> Enum.flat_map(&nested_entities/1)
  end

  defp nested_entities(%Spark.Dsl.Entity{entities: entities} = entity) do
    [
      entity
      | Enum.flat_map(entities, fn
          {_key, nested_entities} -> List.wrap(nested_entities)
          other -> List.wrap(other)
        end)
    ]
  end
end

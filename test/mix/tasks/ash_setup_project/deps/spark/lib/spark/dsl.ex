# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl do
  @using_schema [
    single_extension_kinds: [
      type: {:list, :atom},
      default: [],
      doc:
        "The extension kinds that are allowed to have a single value. For example: `[:data_layer]`"
    ],
    many_extension_kinds: [
      type: {:list, :atom},
      default: [],
      doc:
        "The extension kinds that can have multiple values. e.g `[notifiers: [Notifier1, Notifier2]]`"
    ],
    untyped_extensions?: [
      type: :boolean,
      default: true,
      doc: "Whether or not to support an `extensions` key which contains untyped extensions"
    ],
    extension_kind_types: [
      type: :keyword_list,
      default: [],
      doc:
        "A keyword list of extension kinds and their types, e.g `[authorizers: {:list, {:behaviour, Ash.Authorizer}}]`"
    ],
    extension_kind_docs: [
      type: :keyword_list,
      default: [],
      doc:
        "A keyword list of extension kinds and a short documentation snippet to be used when autocompleting that option"
    ],
    default_extensions: [
      type: :keyword_list,
      default: [],
      doc: """
      The extensions that are included by default. e.g `[data_layer: Default, notifiers: [Notifier1]]`
      Default values for single extension kinds are overwritten if specified by the implementor, while many extension
      kinds are appended to if specified by the implementor.
      """
    ],
    opt_schema: [
      type: :keyword_list,
      default: [],
      doc: """
      A schema for additional options to accept when calling `use YourSpark`
      """
    ],
    opts_to_document: [
      type: {:or, [{:literal, :all}, {:list, :atom}]},
      default: :all,
      doc: """
      A list of `t:atom/0` or `:all`. Spark automatically detects options and documents them in `@moduledoc`.
      You can instruct Spark to use only a subset of options, e.g. `opts_to_document: [:fragments]`.
      """
    ]
  ]

  @type entity :: %Spark.Dsl.Entity{}

  @type section :: %Spark.Dsl.Section{}

  @moduledoc """
  The primary entry point for defining a DSL.

  To define a DSL, add `use Spark.Dsl, ...options`. The options supported with `use Spark.Dsl` are:

  #{Spark.Options.docs(@using_schema)}

  See the callbacks defined in this module to augment the behavior/compilation of the module getting a Dsl.

  ## Schemas/Data Types

  For more information, see `Spark.Options`.
  """

  @type opts :: keyword()
  @type t :: map()

  @doc """
  Validate/add options. Those options will be passed to `handle_opts` and `handle_before_compile`
  """
  @callback init(opts) :: {:ok, opts} | {:error, String.t() | term}

  @doc """
  Validate/add options. Those options will be passed to `handle_opts` and `handle_before_compile`
  """
  @callback explain(t(), opts) :: String.t() | nil

  @doc """
  Handle options in the context of the module. Must return a `quote` block.

  If you want to persist anything in the DSL persistence layer,
  use `@persist {:key, value}`. It can be called multiple times to
  persist multiple times.
  """
  @callback handle_opts(keyword()) :: Macro.t()

  @doc """
  A callback that is called in the `after_verify` hook. Only runs on versions of Elixir >= 1.14.0
  """
  @callback verify(module, keyword()) :: term

  @doc """
  Handle options in the context of the module, after all extensions have been processed. Must return a `quote` block.
  """
  @callback handle_before_compile(keyword()) :: Macro.t()

  defmacro __using__(opts) do
    opts = Spark.Options.validate!(opts, @using_schema)

    their_opt_schema =
      Enum.map(opts[:single_extension_kinds], fn extension_kind ->
        {extension_kind,
         type: opts[:extension_kind_types][extension_kind] || {:behaviour, Spark.Dsl.Extension},
         default: opts[:default_extensions][extension_kind],
         doc:
           opts[:extension_kind_docs][extension_kind] ||
             "#{extension_kind} extensions to add to the `#{inspect(__MODULE__)}`"}
      end) ++
        Enum.map(opts[:many_extension_kinds], fn extension_kind ->
          {extension_kind,
           [
             type:
               opts[:extension_kind_types][extension_kind] ||
                 {:list, {:behaviour, Spark.Dsl.Extension}},
             default: [],
             doc:
               opts[:extension_kind_docs][extension_kind] ||
                 "#{extension_kind} extensions to add to the `#{inspect(__MODULE__)}`"
           ]}
        end)

    their_opt_schema =
      if opts[:untyped_extensions?] do
        Keyword.put(their_opt_schema, :extensions,
          type: {:list, {:behaviour, Spark.Dsl.Extension}},
          doc: "A list of DSL extensions to add to the `#{inspect(__MODULE__)}`"
        )
      else
        their_opt_schema
      end

    their_opt_schema =
      Keyword.merge(their_opt_schema,
        otp_app: [type: :atom, doc: "The otp_app to use for any application configurable options"],
        fragments: [
          type: {:list, :module},
          doc:
            "Fragments to include in the `#{inspect(__MODULE__)}`. See the fragments guide for more."
        ]
      )

    their_opt_schema = Keyword.merge(opts[:opt_schema] || [], their_opt_schema)
    opts_to_document = opts[:opts_to_document]

    quote bind_quoted: [
            their_opt_schema: their_opt_schema,
            opts_to_document: opts_to_document,
            parent_opts: opts,
            parent: __CALLER__.module
          ],
          location: :keep,
          generated: true do
      require Spark.Dsl.Extension
      @dialyzer {:nowarn_function, handle_opts: 1, handle_before_compile: 1}
      Module.register_attribute(__MODULE__, :spark_dsl, persist: true)
      Module.register_attribute(__MODULE__, :spark_default_extensions, persist: true)
      Module.register_attribute(__MODULE__, :spark_extension_kinds, persist: true)
      @spark_dsl true
      @spark_default_extensions parent_opts[:default_extensions]
                                |> Keyword.values()
                                |> List.flatten()
                                |> Enum.flat_map(fn module ->
                                  if function_exported?(module, :add_extensions, 0),
                                    do: [module | module.add_extensions()],
                                    else: [module]
                                end)
                                |> Enum.sort()
                                |> Enum.uniq()
      @spark_extension_kinds List.wrap(parent_opts[:many_extension_kinds]) ++
                               List.wrap(parent_opts[:single_extension_kinds])

      @behaviour Spark.Dsl

      @their_opt_schema their_opt_schema

      @doc false
      def opt_schema, do: @their_opt_schema

      schema_to_document =
        if is_list(opts_to_document) do
          Enum.filter(@their_opt_schema, fn {opt, _} -> opt in opts_to_document end)
        else
          @their_opt_schema
        end

      cond do
        @moduledoc == false or schema_to_document == [] ->
          :ok

        @moduledoc ->
          @moduledoc """
          #{@moduledoc}

          ### Options

          #{Spark.Options.docs(schema_to_document)}
          """

        true ->
          @moduledoc """
          ### Options

          #{Spark.Options.docs(schema_to_document)}
          """
      end

      @doc false
      def init(opts), do: {:ok, opts}

      @doc false
      def explain(_, _), do: nil

      @doc false
      def verify(_, _), do: :ok

      @doc false
      def default_extensions, do: @spark_default_extensions
      @doc false
      def default_extension_kinds, do: List.wrap(unquote(parent_opts[:default_extensions]))
      @doc false
      def many_extension_kinds, do: List.wrap(unquote(parent_opts[:many_extension_kinds]))
      @doc false
      def single_extension_kinds, do: List.wrap(unquote(parent_opts[:single_extension_kinds]))

      @doc false
      def handle_opts(opts) do
        quote do
        end
      end

      @doc false
      def handle_before_compile(opts) do
        quote do
        end
      end

      defmacro __using__(opts) do
        opts =
          if Macro.quoted_literal?(opts) do
            opts
          else
            []
          end

        parent = unquote(parent)
        parent_opts = unquote(parent_opts)
        their_opt_schema = unquote(their_opt_schema)
        require Spark.Dsl.Extension

        fragments =
          if Keyword.keyword?(opts) do
            opts[:fragments]
            |> List.wrap()
            |> Enum.map(&Spark.Dsl.Extension.do_expand(&1, __CALLER__))
          else
            []
          end

        {opts, extensions} =
          parent_opts[:default_extensions]
          |> Enum.reduce(opts, fn {key, defaults}, opts ->
            Keyword.update(opts, key, defaults, fn current_value ->
              cond do
                key in parent_opts[:single_extension_kinds] ->
                  fragments_set =
                    Enum.filter(fragments, fn fragment ->
                      fragment.opts()
                      |> Spark.Dsl.expand_modules(parent_opts, __CALLER__)
                      |> elem(0)
                      |> Keyword.get(key)
                    end)

                  cond do
                    current_value && !Enum.empty?(fragments_set) ->
                      raise "#{key} is being set as an option, but is also set in fragments: #{Enum.map_join(fragments_set, ", ", &inspect/1)}"

                    Enum.count(fragments_set) > 1 ->
                      raise "#{key} is being set by multiple fragments: #{Enum.map_join(fragments_set, ", ", &inspect/1)}"

                    true ->
                      current_value || List.first(fragments_set) || defaults
                  end

                key in parent_opts[:many_extension_kinds] || key == :extensions ->
                  List.wrap(current_value) ++ List.wrap(defaults)

                true ->
                  current_value
              end
            end)
          end)
          |> Spark.Dsl.expand_modules(parent_opts, __CALLER__)

        fragment_extensions = Enum.flat_map(fragments, & &1.extensions())

        opts =
          Enum.reduce(fragments, opts, fn fragment, opts ->
            fragment.opts()
            |> Spark.Dsl.expand_modules(parent_opts, __CALLER__)
            |> elem(0)
            |> Enum.reduce(opts, fn {key, value}, opts ->
              cond do
                key in parent_opts[:single_extension_kinds] ->
                  Keyword.put(opts, key, value)

                key in parent_opts[:many_extension_kinds] ->
                  Keyword.update(opts, key, value, fn current_extensions ->
                    Enum.uniq(current_extensions ++ List.wrap(value))
                  end)

                true ->
                  opts
              end
            end)
          end)

        extensions =
          extensions
          |> Enum.concat(fragment_extensions)
          |> Enum.flat_map(&[&1 | &1.add_extensions()])
          |> Enum.uniq()

        if :elixir_module.mode(__CALLER__.module) == :all do
          Module.put_attribute(__CALLER__.module, :extensions, extensions)
        end

        body =
          quote generated: true, location: :keep do
            Module.register_attribute(__MODULE__, :extensions, persist: true)
            @extensions unquote(extensions)

            opts =
              unquote(
                opts
                |> Keyword.take([:extensions] ++ @spark_extension_kinds)
                |> Keyword.merge(
                  opts
                  |> Keyword.drop([:extensions] ++ @spark_extension_kinds)
                  |> Spark.Dsl.Extension.expand_alias_no_require(__CALLER__)
                )
              )
              |> Spark.Options.validate!(unquote(their_opt_schema))
              |> unquote(__MODULE__).init()
              |> Spark.Dsl.unwrap()

            parent = unquote(parent)
            parent_opts = unquote(parent_opts)
            their_opt_schema = unquote(their_opt_schema)

            @their_opt_schema their_opt_schema

            @opts opts
            @before_compile Spark.Dsl

            Module.register_attribute(__MODULE__, :spark_is, persist: true)

            @spark_is parent
            @spark_parent parent

            @doc false
            def spark_is, do: @spark_is

            Module.register_attribute(__MODULE__, :persist, accumulate: true)

            opts
            |> @spark_parent.handle_opts()
            |> Code.eval_quoted([], __ENV__)

            if opts[:otp_app] do
              @persist {:otp_app, opts[:otp_app]}
            end

            @persist {:module, __MODULE__}
            @persist {:file, __ENV__.file}

            for single_extension_kind <- parent_opts[:single_extension_kinds] do
              @persist {single_extension_kind, opts[single_extension_kind]}
              Module.put_attribute(__MODULE__, single_extension_kind, opts[single_extension_kind])
            end

            for many_extension_kind <- parent_opts[:many_extension_kinds] do
              @persist {many_extension_kind, opts[many_extension_kind] || []}
              Module.put_attribute(
                __MODULE__,
                many_extension_kind,
                opts[many_extension_kind] || []
              )
            end
          end

        preparations = Spark.Dsl.Extension.prepare(extensions)
        [body | preparations]
      end

      defoverridable init: 1,
                     handle_opts: 1,
                     handle_before_compile: 1,
                     explain: 2,
                     __using__: 1,
                     verify: 2
    end
  end

  @doc false
  def unwrap({:ok, value}), do: value
  def unwrap({:error, error}), do: raise(error)

  @doc false
  def expand_modules(opts, their_opt_schema, env) do
    Enum.reduce(opts, {[], []}, fn {key, value}, {opts, extensions} ->
      cond do
        key in their_opt_schema[:single_extension_kinds] ->
          mod = Macro.expand(value, env)

          extensions =
            if mod != :embedded && Spark.implements_behaviour?(mod, Spark.Dsl.Extension) do
              [mod | extensions]
            else
              extensions
            end

          {Keyword.put(opts, key, mod), extensions}

        key in their_opt_schema[:many_extension_kinds] || key == :extensions ->
          mods =
            value
            |> List.wrap()
            |> Enum.map(&Macro.expand(&1, env))

          extensions =
            extensions ++
              Enum.filter(mods, &Spark.implements_behaviour?(&1, Spark.Dsl.Extension))

          {Keyword.put(opts, key, mods), extensions}

        true ->
          {Keyword.put(opts, key, value), extensions}
      end
    end)
  end

  defmacro __before_compile__(env) do
    parent = Module.get_attribute(env.module, :spark_parent)
    opts = Module.get_attribute(env.module, :opts)
    parent_code = parent.handle_before_compile(opts)

    verify_code =
      quote generated: true do
        @after_verify {__MODULE__, :__verify_spark_dsl__}

        @doc false
        def __verify_spark_dsl__(module) do
          unquote(parent).verify(module, @opts)

          transformers_to_run =
            @extensions
            |> Enum.flat_map(& &1.transformers())
            |> Spark.Dsl.Transformer.sort()
            |> Enum.filter(& &1.after_compile?())

          errors =
            @extensions
            |> Enum.flat_map(& &1.verifiers())
            |> Enum.flat_map(fn verifier ->
              try do
                case verifier.verify(@spark_dsl_config) do
                  :ok ->
                    []

                  {:warn, warnings} ->
                    warnings
                    |> List.wrap()
                    |> Enum.each(fn
                      {warning, location} ->
                        Spark.Warning.warn(warning, location, Macro.Env.stacktrace(__ENV__))

                      warning ->
                        Spark.Warning.warn(warning, nil, Macro.Env.stacktrace(__ENV__))
                    end)

                    []

                  {:error, error} ->
                    List.wrap(error)
                end
              rescue
                e ->
                  [e]
              end
            end)

          case Enum.uniq(errors) do
            [] ->
              :ok

            [%Spark.Error.DslError{stacktrace: %{stacktrace: stacktrace}} = error] ->
              reraise error, stacktrace

            [error] ->
              raise error

            errors ->
              raise Spark.Error.DslError,
                message:
                  "Multiple Errors Occurred\n\n" <>
                    Enum.map_join(errors, "\n---\n", fn
                      %Spark.Error.DslError{stacktrace: %{stacktrace: stacktrace}} = error ->
                        Exception.format(:error, error, stacktrace)

                      error ->
                        {:current_stacktrace, stacktrace} =
                          Process.info(self(), :current_stacktrace)

                        Exception.format(:error, error, stacktrace)
                    end)
          end

          __MODULE__
          |> Spark.Dsl.Extension.run_transformers(
            transformers_to_run,
            @spark_dsl_config,
            __ENV__
          )
        catch
          kind, %Spark.Error.DslError{location: location} = reason ->
            Spark.Warning.warn(
              Exception.format(kind, reason, __STACKTRACE__),
              location,
              __STACKTRACE__
            )

          kind, reason ->
            Spark.Warning.warn(
              """
              Exception while verifying `#{inspect(__MODULE__)}:`
              #{Exception.format(kind, reason, __STACKTRACE__)}
              """,
              nil,
              __STACKTRACE__
            )
        end
      end

    code =
      quote generated: true,
            bind_quoted: [dsl: __MODULE__, parent: parent, fragments: opts[:fragments]] do
        require Spark.Dsl.Extension

        for extension <- @extensions do
          for section <- extension.sections() do
            if section.top_level? do
              current_config =
                Process.get(
                  {__MODULE__, :spark, [section.name]},
                  Spark.Dsl.Extension.default_section_config()
                )

              opts =
                case Spark.Options.validate(
                       current_config.opts,
                       Map.get(section, :schema, [])
                     ) do
                  {:ok, opts} ->
                    opts

                  {:error, error} ->
                    raise Spark.Error.DslError,
                      module: __MODULE__,
                      message: error,
                      path: [section.name],
                      location: current_config.section_anno
                end

              Process.put(
                {__MODULE__, :spark, [section.name]},
                %{current_config | entities: current_config.entities, opts: opts}
              )
            end
          end
        end

        Module.register_attribute(__MODULE__, :spark_is, persist: true)
        Module.put_attribute(__MODULE__, :spark_is, @spark_is)

        Spark.Dsl.Extension.set_state(@persist, fragments)

        for {block, bindings} <- Enum.reverse(@spark_dsl_config[:eval] || []) do
          Code.eval_quoted(block, bindings, __ENV__)
        end

        # remove
        def __spark_placeholder__, do: nil

        @doc false
        for {path, %{entities: entities}} <- @spark_dsl_config do
          def entities(unquote(path)), do: unquote(Macro.escape(entities || []))
        end

        def entities(_), do: []

        @doc false
        for {path, %{opts: opts}} <- @spark_dsl_config, is_list(path) do
          for {key, value} <- opts do
            def fetch_opt(unquote(path), unquote(key)) do
              {:ok, unquote(Macro.escape(value))}
            end
          end
        end

        def fetch_opt(_, _), do: :error

        @doc false
        @spec section_anno(list(atom)) :: :erl_anno.anno() | nil
        for {path, %{section_anno: section_anno}} <- @spark_dsl_config,
            is_list(path),
            not is_nil(section_anno) do
          def section_anno(unquote(path)) do
            unquote(Macro.escape(section_anno))
          end
        end

        def section_anno(_), do: nil

        @doc false
        @spec opt_anno(list(atom), atom) :: :erl_anno.anno() | nil
        for {path, %{opts_anno: opts_anno}} <- @spark_dsl_config,
            is_list(path),
            is_list(opts_anno) do
          for {key, anno} <- opts_anno do
            def opt_anno(unquote(path), unquote(key)) do
              unquote(Macro.escape(anno))
            end
          end
        end

        def opt_anno(_, _), do: nil

        @doc false
        @spec opts_anno(list(atom)) :: Keyword.t()
        for {path, %{opts_anno: opts_anno}} <- @spark_dsl_config,
            is_list(path),
            is_list(opts_anno) do
          def opts_anno(unquote(path)) do
            unquote(Macro.escape(opts_anno))
          end
        end

        def opts_anno(_), do: []

        for {path, %{opts: opts}} <- @spark_dsl_config, is_list(path) do
          def section_opts(unquote(path)) do
            unquote(Macro.escape(opts))
          end
        end

        @doc false
        def section_opts(_) do
          []
        end

        @spark_dsl_config @spark_dsl_config
                          |> Map.delete(:eval)
                          |> Map.update!(:persist, &Map.drop(&1, [:env]))

        @persisted @spark_dsl_config[:persist]

        @doc false
        def persisted(key, value), do: persisted() |> Map.get(key, value)

        @doc false
        def fetch_persisted(key), do: Map.fetch(persisted(), key)

        persisted_keys =
          {:persist,
           quote do
             persisted()
           end}

        section_keys =
          for {path, _} <- @spark_dsl_config, is_list(path) do
            {Macro.escape(path),
             quote do
               %{
                 section_anno: section_anno(unquote(path)),
                 entities: entities(unquote(path)),
                 opts: section_opts(unquote(path)),
                 opts_anno: opts_anno(unquote(path))
               }
             end}
          end

        @spark_dsl_config_quoted {:%{}, [], [persisted_keys | section_keys]}
        @persisted_quoted persisted_keys

        @doc false
        def spark_dsl_config do
          unquote(@spark_dsl_config_quoted)
        end

        @doc false
        def persisted do
          @persisted
        end

        cond do
          @moduledoc == false ->
            :ok

          @moduledoc ->
            @moduledoc """
            #{@moduledoc}

            #{Spark.Dsl.Extension.explain(parent, @opts, @extensions, @spark_dsl_config)}
            """

          true ->
            @moduledoc Spark.Dsl.Extension.explain(parent, @opts, @extensions, @spark_dsl_config)
        end
      end

    [code, parent_code, verify_code]
  end

  def is?(module, type) when is_atom(module) do
    module.spark_is() == type
  rescue
    _ -> false
  end

  def is?(_module, _type), do: false

  def handle_fragments(dsl_config, fragments) do
    fragments
    |> List.wrap()
    |> Enum.reduce(dsl_config, fn fragment, acc ->
      config = Map.delete(fragment.spark_dsl_config(), :persist)

      Map.merge(acc, config, fn
        key, left_config, right_config ->
          %{
            entities: (left_config[:entities] || []) ++ (right_config[:entities] || []),
            opts:
              merge_with_warning(
                left_config[:opts] || [],
                right_config[:opts] || [],
                key,
                "fragment: #{fragment}"
              ),
            opts_anno:
              Keyword.merge(left_config[:opts_anno] || [], right_config[:opts_anno] || []),
            section_anno: right_config[:section_anno] || left_config[:section_anno]
          }
      end)
    end)
  end

  @doc false
  def merge_with_warning(left, right, path, overwriting_by \\ nil) do
    Keyword.merge(left, right, fn
      _, left, left ->
        left

      key, left, right ->
        by =
          if overwriting_by do
            " by #{overwriting_by}"
          else
            ""
          end

        Spark.Warning.warn(
          "#{Enum.join(path ++ [key], ".")} is being overwritten from #{inspect(left)} to #{inspect(right)}#{by}"
        )

        right
    end)
  end
end

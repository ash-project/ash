defmodule Ash.Dsl do
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
    default_extensions: [
      type: :keyword_list,
      default: [],
      doc: """
      The extensions that are included by default. e.g `[data_layer: Default, notifiers: [Notifier1]]`
      Default values for single extension kinds are overwritten if specified by the implementor, while many extension
      kinds are appended to if specified by the implementor.
      """
    ]
  ]

  @moduledoc """
  The primary entry point for adding a DSL to a module.

  To add a DSL to a module, add `use Ash.Dsl, ...options`. The options supported with `use Ash.Dsl` are:

  #{Ash.OptionsHelpers.docs(@using_schema)}

  See the callbacks defined in this module to augment the behavior/compilation of the module getting a Dsl.
  """

  @type opts :: Keyword.t()
  @doc """
  Validate/add options. Those options will be passed to `handle_opts` and `handle_before_compile`
  """
  @callback init(opts) :: {:ok, opts} | {:error, String.t() | term}
  @doc """
  Handle options in the context of the module. Must return a `quote` block.

  If you want to persist anything in the DSL persistence layer,
  use `@persist {:key, value}`. It can be called multiple times to
  persist multiple times.
  """
  @callback handle_opts(Keyword.t()) :: Macro.t()
  @doc """
  Handle options in the context of the module, after all extensions have been processed. Must return a `quote` block.
  """
  @callback handle_before_compile(Keyword.t()) :: Macro.t()

  defmacro __using__(opts) do
    opts = Ash.OptionsHelpers.validate!(opts, @using_schema)

    their_opt_schema =
      Enum.map(opts[:single_extension_kinds], fn extension_kind ->
        {extension_kind, type: :atom, default: opts[:default_extensions][extension_kind]}
      end) ++
        Enum.map(opts[:many_extension_kinds], fn extension_kind ->
          {extension_kind, type: {:list, :atom}, default: []}
        end)

    their_opt_schema =
      if opts[:untyped_extensions?] do
        Keyword.put(their_opt_schema, :extensions, type: {:list, :atom})
      else
        their_opt_schema
      end

    quote bind_quoted: [
            their_opt_schema: their_opt_schema,
            parent_opts: opts,
            parent: __CALLER__.module
          ] do
      @dialyzer {:nowarn_function, handle_opts: 1, handle_before_compile: 1}

      def init(opts), do: {:ok, opts}

      def handle_opts(opts) do
        quote do
        end
      end

      def handle_before_compile(opts) do
        quote do
        end
      end

      defoverridable init: 1, handle_opts: 1, handle_before_compile: 1

      defmacro __using__(opts) do
        parent = unquote(parent)
        parent_opts = unquote(parent_opts)
        their_opt_schema = unquote(their_opt_schema)

        {opts, extensions} =
          parent_opts[:default_extensions]
          |> Enum.reduce(opts, fn {key, defaults}, opts ->
            Keyword.update(opts, key, defaults, fn current_value ->
              cond do
                key in parent_opts[:single_extension_kinds] ->
                  current_value || defaults

                key in parent_opts[:many_extension_kinds] || key == :extensions ->
                  List.wrap(current_value) ++ List.wrap(defaults)

                true ->
                  opts
              end
            end)
          end)
          |> Ash.Dsl.expand_modules(parent_opts, __CALLER__)

        opts =
          opts
          |> Ash.OptionsHelpers.validate!(their_opt_schema)
          |> init()
          |> Ash.Dsl.unwrap()

        body =
          quote do
            parent = unquote(parent)
            opts = unquote(opts)
            parent_opts = unquote(parent_opts)
            their_opt_schema = unquote(their_opt_schema)

            @opts opts
            @before_compile Ash.Dsl
            @ash_is parent
            @ash_parent parent

            Module.register_attribute(__MODULE__, :persist, accumulate: true)

            opts
            |> @ash_parent.handle_opts()
            |> Code.eval_quoted([], __ENV__)

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

        preparations = Ash.Dsl.Extension.prepare(extensions)
        [body | preparations]
      end
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
          mod = Macro.expand(value, %{env | lexical_tracker: nil})

          extensions =
            if Ash.Helpers.implements_behaviour?(mod, Ash.Dsl.Extension) do
              [mod | extensions]
            else
              extensions
            end

          {Keyword.put(opts, key, mod), extensions}

        key in their_opt_schema[:many_extension_kinds] || key == :extensions ->
          mods =
            value |> List.wrap() |> Enum.map(&Macro.expand(&1, %{env | lexical_tracker: nil}))

          extensions =
            extensions ++
              Enum.filter(mods, &Ash.Helpers.implements_behaviour?(&1, Ash.Dsl.Extension))

          {Keyword.put(opts, key, mods), extensions}

        true ->
          {key, value}
      end
    end)
  end

  defmacro __before_compile__(_env) do
    quote unquote: false do
      @type t :: __MODULE__

      Module.register_attribute(__MODULE__, :ash_is, persist: true)
      Module.put_attribute(__MODULE__, :ash_is, @ash_is)
      @on_load :on_load
      ash_dsl_config = Macro.escape(Ash.Dsl.Extension.set_state(@persist))

      def ash_dsl_config do
        unquote(ash_dsl_config)
      end

      def on_load do
        Ash.Dsl.Extension.load()
      end

      @opts
      |> @ash_parent.handle_before_compile()
      |> Code.eval_quoted([], __ENV__)
    end
  end

  def is?(module, type) do
    Ash.Helpers.try_compile(module)

    type in List.wrap(module.module_info(:attributes)[:ash_is])
  rescue
    _ ->
      false
  end
end

defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Dsl`

  For more information on the resource DSL, see `Ash.Dsl`
  """

  alias Ash.Dsl.Extension

  defmacro __using__(opts) do
    data_layer = Macro.expand(opts[:data_layer], __CALLER__)

    authorizers =
      opts[:authorizers]
      |> List.wrap()
      |> Enum.map(&Macro.expand(&1, __CALLER__))

    extensions =
      if data_layer && Ash.implements_behaviour?(data_layer, Ash.Dsl.Extension) do
        [data_layer, Ash.Dsl]
      else
        [Ash.Dsl]
      end

    authorizer_extensions =
      Enum.filter(authorizers, &Ash.implements_behaviour?(&1, Ash.Dsl.Extension))

    extensions = Enum.concat([extensions, opts[:extensions] || [], authorizer_extensions])

    body =
      quote bind_quoted: [opts: opts] do
        @before_compile Ash.Resource

        @authorizers opts[:authorizers] || []
        @data_layer opts[:data_layer]
      end

    preparations = Extension.prepare(extensions)

    [body | preparations]
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro __before_compile__(_env) do
    quote do
      @doc false
      alias Ash.Dsl.Extension

      :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
      :persistent_term.put({__MODULE__, :authorizers}, @authorizers)

      Extension.set_state(false)
      :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
      :persistent_term.put({__MODULE__, :authorizers}, @authorizers)
      Extension.set_state(true)

      def raw_dsl do
        @ash_dsl_config
      end

      use Supervisor

      def start_link(args) do
        Supervisor.start_link(__MODULE__, args, name: __MODULE__)
      end

      def init(_init_arg) do
        :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
        :persistent_term.put({__MODULE__, :authorizers}, @authorizers)
        Extension.set_state(true)

        Supervisor.init([], strategy: :one_for_one)
      end

      require Ash.Schema

      Ash.Schema.define_schema()
    end
  end
end

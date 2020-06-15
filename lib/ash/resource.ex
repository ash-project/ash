defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Dsl`

  For more information on the resource DSL, see `Ash.Dsl`
  """

  alias Ash.Dsl.Extension

  defmacro __using__(opts) do
    extensions =
      if opts[:data_layer] do
        [opts[:data_layer], Ash.Dsl]
      else
        [Ash.Dsl]
      end

    extensions = Enum.concat([extensions, opts[:extensions] || [], opts[:authorizers] || []])

    body =
      quote bind_quoted: [opts: opts] do
        @before_compile Ash.Resource
        @on_load :build_dsl

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

      def raw_dsl do
        @ash_dsl_config
      end

      @doc false
      def build_dsl do
        :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
        :persistent_term.put({__MODULE__, :authorizers}, @authorizers)
        Extension.set_state(true)

        :ok
      end

      require Ash.Schema

      Ash.Schema.define_schema()
    end
  end
end

defmodule Ash.Api do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      import Ash.Api, only: [api: 1]
      @before_compile Ash.Api

      @default_page_size nil
      @max_page_size nil
      @no_interface !!opts[:no_interface?]
      @side_load_type :simple
      @side_load_config []

      Module.register_attribute(__MODULE__, :mix_ins, accumulate: true)
      Module.register_attribute(__MODULE__, :resources, accumulate: true)
      Module.register_attribute(__MODULE__, :named_resources, accumulate: true)
    end
  end

  defmacro api(do: block) do
    quote do
      import Ash.Api,
        only: [
          default_page_size: 1,
          max_page_size: 1,
          resources: 1,
          side_load: 2,
          side_load: 1
        ]

      unquote(block)

      import Ash.Api, only: []
    end
  end

  defmacro resources(resources) do
    quote do
      Enum.map(unquote(resources), fn resource ->
        case resource do
          {name, resource} ->
            @resources resource
            @named_resources {name, resource}

          resource ->
            @resources resource
        end
      end)
    end
  end

  defmacro side_load(type, config \\ []) do
    quote bind_quoted: [type: type, config: config] do
      unless type in [:parallel, :simple] do
        raise "side_load type must be one if `:parallel` or `:simple`"
      end

      case type do
        :simple ->
          @side_load_type :simple

        :parallel ->
          @side_load_type :parallel
          # TODO: validate no extra keys
          raise "`:supervisor` option must be set."

          @side_load_config [
            supervisor: config[:supervisor],
            max_concurrency: config[:max_concurrency],
            timeout: opts[:timeout],
            shutdown: opts[:shutdown]
          ]
      end
    end
  end

  defmacro default_page_size(value) do
    quote do
      @default_page_size unquote(value)
    end
  end

  defmacro max_page_size(value) do
    quote do
      @max_page_size unquote(value)
    end
  end

  defmacro __before_compile__(env) do
    quote do
      def default_page_size(), do: @default_page_size
      def max_page_size(), do: @max_page_size
      def mix_ins(), do: @mix_ins
      def resources(), do: @resources
      def side_load_config(), do: {@side_load_type, @side_load_config}

      @resources
      |> Enum.group_by(&Ash.type/1)
      |> Enum.map(fn {type, resources} ->
        if Enum.count(resources) > 1 do
          raise "multiple resources w/ conflicting type #{type} in #{__MODULE__}"
        end
      end)

      unless @no_interface do
        use Ash.Api.Interface
      end

      Enum.map(@mix_ins || [], fn hook_module ->
        code = hook_module.before_compile_hook(unquote(Macro.escape(env)))
        Module.eval_quoted(__MODULE__, code)
      end)
    end
  end
end

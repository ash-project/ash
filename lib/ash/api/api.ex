defmodule Ash.Api do
  @moduledoc """
  An Api allows you to interact with your resources, anc holds non-resource-specific configuration.

  Your Api can also house config that is not resource specific.
  Defining a resource won't do much for you. Once you have some resources defined,
  you include them in an Api like so:

  ```elixir
  defmodule MyApp.Api do
    use Ash.Api

    resources [OneResource, SecondResource]
  end
  ```

  Then you can interact through that Api with the actions that those resources expose.
  For example: `MyApp.Api.create(OneResource, %{attributes: %{name: "thing"}})`, or
  `MyApp.Api.read(query)`. Corresponding actions must
  be defined in your resources in order to call them through the Api.
  """
  defmacro __using__(_) do
    quote do
      @before_compile Ash.Api

      @side_load_type :simple

      Module.register_attribute(__MODULE__, :extensions, accumulate: true)
      Module.register_attribute(__MODULE__, :resources, accumulate: true)
      Module.register_attribute(__MODULE__, :named_resources, accumulate: true)

      import Ash.Api,
        only: [
          resources: 1
        ]
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

  defmacro __before_compile__(env) do
    quote generated: true do
      def extensions(), do: @extensions
      def resources(), do: @resources

      def get_resource(mod) when mod in @resources, do: {:ok, mod}

      def get_resource(name) do
        Keyword.fetch(@named_resources, name)
      end

      use Ash.Api.Interface

      Enum.map(@extensions, fn hook_module ->
        code = hook_module.before_compile_hook(unquote(Macro.escape(env)))
        Module.eval_quoted(__MODULE__, code)
      end)
    end
  end
end

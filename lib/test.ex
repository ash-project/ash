defmodule Ash.Test do
  defmacro test_resource(name, type, opts \\ [], do: block) do
    quote do
      module_name = Module.concat(__MODULE__, String.capitalize(unquote(name)))

      Module.put_attribute(
        __MODULE__,
        unquote(opts[:attr]) || String.to_atom(unquote(name)),
        module_name
      )

      defmodule module_name do
        use Ash.Resource, name: unquote(name), type: unquote(type)
        use Ash.DataLayer.Ets, private?: true
        unquote(block)
      end

      module_name
    end
  end

  defmacro test_api(opts, do: block) do
    quote do
      opts = unquote(opts)
      module_name = Module.concat(__MODULE__, Api)
      Module.put_attribute(__MODULE__, opts[:attr] || :api, module_name)

      resources =
        Enum.map(
          opts[:resources],
          fn
            {:@, _, [{attr, _, nil}]} ->
              Module.get_attribute(__MODULE__, attr)

            resource when is_atom(resource) ->
              Module.get_attribute(__MODULE__, resource) || resource
          end
        )

      defmodule module_name do
        use Ash.Api

        api do
          resources resources
        end

        unquote(block)
      end

      module_name
    end
  end

  defmacro test_api(opts) do
    quote do
      test_api(unquote(opts)) do
        :ok
      end
    end
  end
end

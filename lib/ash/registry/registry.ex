defmodule Ash.Registry do
  @moduledoc """
  A registry allows you to separate your resources from your `api` module, to reduce improve compile times and reduce compile time dependencies.

  For example:

  ```elixir
  defmodule MyApp.MyRegistry do
    use Ash.Registry,
      extensions: [Ash.Registry.ResourceValidations]
      # This extension adds some simple validations, for example that
      # all related resources are also present in this registry.

    entries do
      entry MyApp.Resource
      entry MyApp.OtherResource
    end
  end
  ```
  """

  @type t :: module

  use Spark.Dsl, default_extensions: [extensions: [Ash.Registry.Dsl]]

  @deprecated "use Ash.Registry.Info.warn_on_empty?/1 instead"
  defdelegate warn_on_empty?(registry), to: Ash.Registry.Info

  @deprecated "use Ash.Registry.Info.entries/1 instead"
  defdelegate entries(registry), to: Ash.Registry.Info
end

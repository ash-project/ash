defmodule Ash.Reactor do
  @moduledoc """
  `Ash.Reactor` is a [`Reactor`](https://hex.pm/packages/reactor) extension
  which provides steps for working with Ash resources and actions.
  """

  @ash %Spark.Dsl.Section{
    name: :ash,
    describe: "Ash-related configuration for the `Ash.Reactor` extension",
    schema: [
      default_api: [
        type: {:behaviour, Ash.Api},
        doc: "An API to use by default when calling actions",
        required: false
      ]
    ]
  }

  use Spark.Dsl.Extension,
    sections: [@ash],
    transformers: [__MODULE__.Dsl.ActionTransformer],
    dsl_patches:
      ~w[Create]
      |> Enum.map(&Module.concat(__MODULE__.Dsl, &1))
      |> Enum.map(&%Spark.Dsl.Patch.AddEntity{section_path: [:reactor], entity: &1.__entity__()})

  @doc false
  @spec __using__(Keyword.t()) :: Macro.output()
  defmacro __using__(opts) do
    opts = Keyword.update(opts, :extensions, [Ash.Reactor], &[Ash.Reactor | &1])

    quote do
      use Reactor, unquote(opts)
    end
  end
end

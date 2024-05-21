defmodule Ash.Reactor do
  @moduledoc """
  `Ash.Reactor` is a [`Reactor`](https://hex.pm/packages/reactor) extension
  which provides steps for working with Ash resources and actions.

  See the [Ash Reactor Guide](https://hexdocs.pm/ash/reactor.html) for more
  information.
  """

  @ash %Spark.Dsl.Section{
    name: :ash,
    describe: "Ash-related configuration for the `Ash.Reactor` extension",
    schema: [
      default_domain: [
        type: {:behaviour, Ash.Domain},
        doc: "A domain to use by default when calling actions",
        required: false
      ]
    ]
  }
  @type action ::
          Ash.Reactor.Dsl.Action.t()
          | Ash.Reactor.Dsl.BulkCreate.t()
          | Ash.Reactor.Dsl.BulkUpdate.t()
          | Ash.Reactor.Dsl.Create.t()
          | Ash.Reactor.Dsl.Destroy.t()
          | Ash.Reactor.Dsl.Read.t()
          | Ash.Reactor.Dsl.ReadOne.t()
          | Ash.Reactor.Dsl.Update.t()

  use Spark.Dsl.Extension,
    sections: [@ash],
    transformers: [Ash.Reactor.Dsl.ActionTransformer, Ash.Reactor.Dsl.MiddlewareTransformer],
    dsl_patches:
      [
        Ash.Reactor.Dsl.Action,
        Ash.Reactor.Dsl.BulkCreate,
        Ash.Reactor.Dsl.BulkUpdate,
        Ash.Reactor.Dsl.Change,
        Ash.Reactor.Dsl.Create,
        Ash.Reactor.Dsl.Destroy,
        Ash.Reactor.Dsl.ReadOne,
        Ash.Reactor.Dsl.Read,
        Ash.Reactor.Dsl.Transaction,
        Ash.Reactor.Dsl.Update
      ]
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

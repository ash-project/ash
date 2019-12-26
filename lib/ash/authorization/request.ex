defmodule Ash.Authorization.Request do
  defstruct [
    :resource,
    :authorization_steps,
    :filter,
    :action_type,
    :relationship,
    :strict_access?
  ]

  @type t :: %__MODULE__{
          action_type: atom,
          resource: Ash.resource(),
          authorization_steps: list(term),
          filter: Ash.Filter.t(),
          relationship: list(atom),
          strict_access?: boolean
        }

  def new(opts) do
    opts =
      opts
      |> Keyword.put_new(:relationship, [])
      |> Keyword.put_new(:strict_access?, true)

    struct!(__MODULE__, opts)
  end
end

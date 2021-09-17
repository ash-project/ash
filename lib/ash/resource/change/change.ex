defmodule Ash.Resource.Change do
  @moduledoc """
  The behaviour for an action-specific resource change.

  To implement one, simply implement the behaviour. `c:init/1` is defined automatically
  by `use Ash.Resource.Change`, but can be implemented if you want to validate/transform any
  options passed to the module.

  The main function is `c:change/3`. It takes the changeset, any options that were provided
  when this change was configured on a resource, and the context, which currently only has
  the actor.
  """
  defstruct [:change]

  @doc false
  def schema do
    [
      change: [
        type: {:ash_behaviour, Ash.Resource.Change, Ash.Resource.Change.Builtins},
        doc: """
        The module and options for a change.
        """,
        required: true
      ]
    ]
  end

  @doc false
  def change({module, opts}) when is_atom(module) do
    if Keyword.keyword?(opts) do
      {:ok, {module, opts}}
    else
      {:error, "Expected opts to be a keyword, got: #{inspect(opts)}"}
    end
  end

  def change(module) when is_atom(module), do: {:ok, {module, []}}

  def change(other) do
    {:error, "Expected a module and opts, got: #{inspect(other)}"}
  end

  @doc false
  def transform(%{change: {module, opts}} = change) do
    case module.init(opts) do
      {:ok, opts} -> {:ok, %{change | change: {module, opts}}}
      {:error, error} -> {:error, error}
    end
  end

  @type context :: %{actor: Ash.Resource.record()} | %{}
  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback change(Ash.Changeset.t(), Keyword.t(), context) :: Ash.Changeset.t()

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Change

      def init(opts), do: {:ok, opts}

      defoverridable init: 1
    end
  end
end

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
  defstruct [:change, :on, :only_when_valid?, where: []]

  @type t :: %__MODULE__{}

  @doc false
  def schema do
    [
      on: [
        type: {:custom, __MODULE__, :on, []},
        default: [:create, :update],
        doc: """
        The action types the validation should run on.

        Many validations don't make sense in the context of deletion, so by default it is left out of the list.
        """
      ],
      only_when_valid?: [
        type: :boolean,
        default: false,
        doc: """
        If the change should only be run on valid changes. By default, all changes are run unless stated otherwise here.

        For 2.0 this may become the default.
        """
      ],
      change: [
        type: {:ash_behaviour, Ash.Resource.Change, Ash.Resource.Change.Builtins},
        doc: """
        The module and options for a change.
        """,
        required: true
      ],
      where: [
        type:
          {:list, {:ash_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins}},
        required: false,
        default: [],
        doc: """
        Validations that should pass in order for this validation to apply.
        These validations failing will not invalidate the changes, but instead just result in this change being ignored.
        """
      ]
    ]
  end

  def action_schema do
    Keyword.delete(schema(), :on)
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
  def on(list) do
    list
    |> List.wrap()
    |> Enum.all?(&(&1 in [:create, :update, :destroy]))
    |> case do
      true ->
        {:ok, List.wrap(list)}

      false ->
        {:error, "Expected items of [:create, :update, :destroy], got: #{inspect(list)}"}
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

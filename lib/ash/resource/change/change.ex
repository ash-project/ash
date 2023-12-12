defmodule Ash.Resource.Change do
  @moduledoc """
  The behaviour for an action-specific resource change.

  `c:init/1` is defined automatically by `use Ash.Resource.Change`, but can be implemented if you want to validate/transform any
  options passed to the module.

  The main function is `c:change/3`. It takes the changeset, any options that were provided
  when this change was configured on a resource, and the context, which currently only has
  the actor.
  """
  defstruct [:change, :on, :only_when_valid?, :description, :always_atomic?, where: []]

  @type t :: %__MODULE__{}
  @type ref :: {module(), Keyword.t()} | module()

  @doc false
  def schema do
    [
      on: [
        type: {:wrap_list, {:in, [:create, :update, :destroy]}},
        default: [:create, :update],
        doc: """
        The action types the validation should run on. Destroy actions are omitted by default as most changes don't make sense for a destroy.
        """
      ],
      only_when_valid?: [
        type: :boolean,
        default: false,
        doc: """
        If the change should only be run on valid changes. By default, all changes are run unless stated otherwise here.
        """
      ],
      description: [
        type: :string,
        doc: "An optional description for the change"
      ],
      change: [
        type:
          {:spark_function_behaviour, Ash.Resource.Change, Ash.Resource.Change.Builtins,
           {Ash.Resource.Change.Function, 2}},
        doc: """
        The module and options for a change. Also accepts a function that takes the changeset and the context. See `Ash.Resource.Change.Builtins` for builtin changes.
        """,
        required: true
      ],
      where: [
        type:
          {:list,
           {:spark_function_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins,
            {Ash.Resource.Validation.Function, 1}}},
        required: false,
        default: [],
        doc: """
        Validations that should pass in order for this validation to apply. These validations failing will result in this validation being ignored.
        """
      ],
      always_atomic?: [
        type: :boolean,
        default: false,
        doc:
          "By default, changes are only run atomically if all changes will be run atomically or if there is no `change/3` callback defined. Set this to `true` to run it atomically always."
      ]
    ]
  end

  @doc false
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

  @type context :: %{
          optional(:actor) => Ash.Resource.record() | nil,
          optional(:tenant) => String.t() | nil,
          optional(:authorize?) => boolean() | nil,
          optional(:tracer) => Ash.Tracer.t() | [Ash.Tracer.t()] | nil,
          optional(any) => any
        }

  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback change(changeset :: Ash.Changeset.t(), opts :: Keyword.t(), context :: context) ::
              Ash.Changeset.t()

  @doc """
  Replaces `change/3` for batch actions, allowing to optimize changes for bulk actions.
  """
  @callback batch_change(
              changesets :: [Ash.Changeset.t()],
              opts :: Keyword.t(),
              context :: context
            ) ::
              Enumerable.t(Ash.Changeset.t() | Ash.Notifier.Notification.t())

  @doc """
  Runs on each batch before it is dispatched to the data layer.
  """
  @callback before_batch(
              changesets :: [Ash.Changeset.t()],
              opts :: Keyword.t(),
              context :: context
            ) ::
              Enumerable.t(Ash.Changeset.t() | Ash.Notifier.Notification.t())

  @doc """
  Runs on each batch result after it is dispatched to the data layer.
  """
  @callback after_batch(
              changesets_and_results :: [{Ash.Changeset.t(), Ash.Resource.record()}],
              opts :: Keyword.t(),
              context :: context
            ) ::
              Enumerable.t(
                {:ok, Ash.Resource.record()}
                | {:error, Ash.Error.t()}
                | Ash.Notifier.Notification.t()
              )

  @callback atomic(Ash.Changeset.t(), Keyword.t(), context()) ::
              {:atomic, %{atom() => Ash.Expr.t()}}
              | {:non_atomic, Ash.Changeset.t()}
              | {:error, term()}

  @callback after_atomic(Ash.Changeset.t(), Keyword.t(), Ash.Resource.record()) ::
              {:ok, Ash.Resource.record()} | {:error, term()}

  @optional_callbacks before_batch: 3,
                      after_batch: 3,
                      batch_change: 3,
                      change: 3,
                      atomic: 3,
                      after_atomic: 3

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Change
      require Ash.Expr

      def init(opts), do: {:ok, opts}
      def atomic(_opts, _context), do: :not_atomic

      defoverridable init: 1, atomic: 2
    end
  end

  defmacro __before_compile__(_env) do
    quote generated: true do
      unless Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
        @impl Ash.Resource.Validation
        if Module.defines?(__MODULE__, {:atomic, 3}, :def) do
          def atomic?, do: true
        else
          def atomic?, do: false
        end
      end
    end
  end
end

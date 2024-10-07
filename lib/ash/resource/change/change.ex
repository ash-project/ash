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
        The action types the change should run on. Destroy actions are omitted by default as most changes don't make sense for a destroy.
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
          {:wrap_list,
           {:spark_function_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins,
            {Ash.Resource.Validation.Function, 2}}},
        required: false,
        default: [],
        doc: """
        Validations that should pass in order for this change to apply. These validations failing will result in this change being ignored.
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

  defmodule Context do
    @moduledoc """
    The context for a change.

    This is passed into various callbacks for `Ash.Resource.Change`.
    """
    defstruct [:actor, :tenant, :authorize?, :tracer, bulk?: false]

    @type t :: %__MODULE__{
            actor: Ash.Resource.record() | nil,
            tenant: term(),
            authorize?: boolean() | nil,
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil,
            bulk?: boolean
          }
  end

  @type context :: Context.t()

  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback change(changeset :: Ash.Changeset.t(), opts :: Keyword.t(), context :: Context.t()) ::
              Ash.Changeset.t()

  @doc """
  Replaces `change/3` for batch actions, allowing to optimize changes for bulk actions.

  You can define only `batch_change/3`, and it will be used for both single and batch actions.
  It cannot, however, be used in place of the `atomic/3` callback.
  """
  @callback batch_change(
              changesets :: [Ash.Changeset.t()],
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              Enumerable.t(Ash.Changeset.t())

  @doc """
  Runs on each batch before it is dispatched to the data layer.
  """
  @callback before_batch(
              changesets :: [Ash.Changeset.t()],
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              Enumerable.t(Ash.Changeset.t() | Ash.Notifier.Notification.t())

  @doc """
  Runs on each batch result after it is dispatched to the data layer.
  """
  @callback after_batch(
              changesets_and_results :: [{Ash.Changeset.t(), Ash.Resource.record()}],
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              :ok
              | Enumerable.t(
                  {:ok, Ash.Resource.record()}
                  | {:error, Ash.Error.t()}
                  | Ash.Notifier.Notification.t()
                )

  @doc """
  Whether or not batch callbacks should be run (if they are defined). Defaults to `true`.
  """
  @callback batch_callbacks?(
              changesets_or_query :: [Ash.Changeset.t()] | Ash.Query.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              boolean

  @callback atomic(changeset :: Ash.Changeset.t(), opts :: Keyword.t(), context :: Context.t()) ::
              {:ok, Ash.Changeset.t()}
              | {:atomic, %{optional(atom()) => Ash.Expr.t() | {:atomic, Ash.Expr.t()}}}
              | {:atomic, Ash.Changeset.t(), %{optional(atom()) => Ash.Expr.t()}}
              | {:atomic, Ash.Changeset.t(), %{optional(atom()) => Ash.Expr.t()},
                 list(
                   {:atomic, involved_fields :: list(atom) | :*, condition_expr :: Ash.Expr.t(),
                    error_expr :: Ash.Expr.t()}
                 )}
              | {:not_atomic, String.t()}
              | :ok
              | {:error, term()}

  @callback atomic?() :: boolean

  @callback has_change?() :: boolean

  @callback has_batch_change?() :: boolean
  @callback has_after_batch?() :: boolean
  @callback has_before_batch?() :: boolean

  @optional_callbacks before_batch: 3,
                      after_batch: 3,
                      batch_change: 3,
                      change: 3,
                      atomic: 3

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Change
      @before_compile Ash.Resource.Change

      import Ash.Expr
      require Ash.Query

      @impl true
      def init(opts), do: {:ok, opts}

      @impl true
      def batch_callbacks?(_, _, _), do: true

      defoverridable init: 1, batch_callbacks?: 3
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:change, 3}, :def) do
        @impl true
        def has_change?, do: true
      else
        if Module.defines?(__MODULE__, {:batch_change, 3}, :def) do
          @impl true
          def change(changeset, opts, context) do
            changeset
            |> simulate_before_batch(opts, context)
            |> Ash.Changeset.before_action(fn changeset ->
              Enum.at(batch_change([changeset], opts, context), 0)
            end)
            |> simulate_after_batch(opts, context)
          end

          if Module.defines?(__MODULE__, {:before_batch, 3}, :def) do
            defp simulate_before_batch(changeset, opts, context) do
              Ash.Changeset.before_action(changeset, fn changeset ->
                {[changeset], notifications} =
                  Enum.split_with(
                    apply(__MODULE__, :before_batch, [[changeset], opts, context]),
                    fn
                      %Ash.Notifier.Notification{} ->
                        false

                      %Ash.Changeset{} ->
                        true

                      other ->
                        raise "Expected before_batch/3 to return a list of changesets and notifications, got: #{inspect(other)}"
                    end
                  )

                {changeset, %{notifications: notifications}}
              end)
            end
          else
            defp simulate_before_batch(changeset, _opts, _context) do
              changeset
            end
          end

          if Module.defines?(__MODULE__, {:after_batch, 3}, :def) do
            defp simulate_after_batch(changeset, opts, context) do
              Ash.Changeset.after_action(changeset, fn changeset, result ->
                apply(__MODULE__, :after_batch, [[{changeset, result}], opts, context])
                |> then(fn
                  :ok -> [{:ok, result}]
                  other -> other
                end)
                |> Enum.reduce({[], [], []}, fn item, {records, errors, notifications} ->
                  case item do
                    {:ok, record} -> {[record | records], errors, notifications}
                    {:error, error} -> {records, [error | errors], notifications}
                    %Ash.Notifier.Notification{} -> {records, errors, [item | notifications]}
                  end
                end)
                |> case do
                  {[record], [], notifications} ->
                    {:ok, record, notifications}

                  {other, [], _notifications} ->
                    raise "Invalid return value from `after_batch/3`. Expected exactly one record: #{inspect(other)}"

                  {_, errors, _notifications} ->
                    {:error, errors}
                end
              end)
            end
          else
            defp simulate_after_batch(changeset, _opts, _context), do: changeset
          end

          @impl true
          def has_change?, do: true
        else
          @impl true
          def has_change?, do: false
        end
      end

      if Module.defines?(__MODULE__, {:batch_change, 3}, :def) do
        @impl true
        def has_batch_change?, do: true
      else
        @impl true
        def has_batch_change?, do: false
      end

      if Module.defines?(__MODULE__, {:before_batch, 3}, :def) do
        @impl true
        def has_before_batch?, do: true
      else
        @impl true
        def has_before_batch?, do: false
      end

      if Module.defines?(__MODULE__, {:after_batch, 3}, :def) do
        @impl true
        def has_after_batch?, do: true
      else
        @impl true
        def has_after_batch?, do: false
      end

      if Module.defines?(__MODULE__, {:atomic, 3}, :def) do
        unless Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
          @impl true
          def atomic?, do: true
        end
      else
        unless Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
          @impl true
          def atomic?, do: false
        end

        @impl true
        def atomic(_changeset, _opts, _context) do
          {:not_atomic, "#{inspect(__MODULE__)} does not implement `atomic/3`"}
        end
      end
    end
  end
end

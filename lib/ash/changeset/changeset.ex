defmodule Ash.Changeset do
  @moduledoc """
  Changesets are used to create and update data in Ash.

  Create a changeset with `new/1` or `new/2`, and alter the attributes
  and relationships using the functions provided in this module.  Nothing in this module
  actually incurs changes in a data layer. To commit a changeset, see `Ash.create/2`
  and `Ash.update/2`.

  # Changeset lifecycle

  ## Action Lifecycle

  The following example illustrates the hook lifecycle of a changeset.

  ```elixir
  defmodule AshChangesetLifeCycleExample do
    def change(changeset, _, _) do
      changeset
      # execute code both before and after the transaction
      |> Ash.Changeset.around_transaction(fn changeset, callback ->
        callback.(changeset)
      end)
      # execute code before the transaction is started. Use for things like external calls
      |> Ash.Changeset.before_transaction(fn changeset -> changeset end)
      # execute code in the transaction, before and after the data layer is called
      |> Ash.Changeset.around_action(fn changeset, callback ->
        callback.(changeset)
      end)
      # execute code in the transaction, before the data layer is called
      |> Ash.Changeset.before_action(fn changeset -> changeset end)
      # execute code in the transaction, after the data layer is called, only if the action is successful
      |> Ash.Changeset.after_action(fn changeset, result -> {:ok, result} end)
      # execute code after the transaction, both in success and error cases
      |> Ash.Changeset.after_transaction(fn changeset, success_or_error_result -> success_or_error_result end
    end
  end
  ```
  """

  defstruct [
    :__validated_for_action__,
    :action_type,
    :action,
    :domain,
    :data,
    :handle_errors,
    :resource,
    :tenant,
    :to_tenant,
    :timeout,
    dirty_hooks: [],
    invalid_keys: MapSet.new(),
    filter: nil,
    added_filter: nil,
    action_failed?: false,
    atomics: [],
    atomic_validations: [],
    after_action: [],
    after_transaction: [],
    arguments: %{},
    around_action: [],
    around_transaction: [],
    attributes: %{},
    before_action: [],
    before_transaction: [],
    no_atomic_constraints: [],
    context: %{},
    context_changes: %{},
    defaults: [],
    errors: [],
    params: %{},
    action_select: [],
    atomic_after_action: [],
    attribute_changes: %{},
    atomic_changes: [],
    casted_attributes: %{},
    casted_arguments: %{},
    phase: :pending,
    relationships: %{},
    select: nil,
    load: [],
    valid?: true
  ]

  defimpl Inspect do
    import Inspect.Algebra

    @spec inspect(Ash.Changeset.t(), Inspect.Opts.t()) ::
            {:doc_cons, :doc_line | :doc_nil | binary | tuple,
             :doc_line | :doc_nil | binary | tuple}
            | {:doc_group,
               :doc_line
               | :doc_nil
               | binary
               | {:doc_collapse, pos_integer}
               | {:doc_force, any}
               | {:doc_break | :doc_color | :doc_cons | :doc_fits | :doc_group | :doc_string, any,
                  any}
               | {:doc_nest, any, :cursor | :reset | non_neg_integer, :always | :break},
               :inherit | :self}
    def inspect(changeset, opts) do
      context = Map.delete(changeset.context, :private)

      context =
        if context == %{} do
          empty()
        else
          concat("context: ", to_doc(context, opts))
        end

      tenant =
        if changeset.tenant do
          concat(
            "tenant: ",
            to_doc(changeset.to_tenant, opts)
          )
        else
          empty()
        end

      domain =
        if changeset.domain do
          concat("domain: ", to_doc(changeset.domain, opts))
        else
          empty()
        end

      select =
        if changeset.select do
          concat("select: ", to_doc(changeset.select, opts))
        else
          empty()
        end

      load =
        if changeset.load && changeset.load != [] do
          concat("load: ", to_doc(changeset.load, opts))
        else
          empty()
        end

      atomics =
        if Enum.empty?(changeset.atomics) do
          empty()
        else
          concat("atomics: ", to_doc(changeset.atomics, opts))
        end

      filter =
        case changeset.filter do
          nil ->
            empty()

          %Ash.Filter{expression: nil} ->
            empty()

          _ ->
            concat("filter: ", to_doc(changeset.filter, opts))
        end

      container_doc(
        "#Ash.Changeset<",
        [
          domain,
          concat("action_type: ", inspect(changeset.action_type)),
          concat("action: ", inspect(changeset.action && changeset.action.name)),
          tenant,
          concat("attributes: ", to_doc(changeset.attributes, opts)),
          atomics,
          concat("relationships: ", to_doc(changeset.relationships, opts)),
          arguments(changeset, opts),
          concat("errors: ", to_doc(changeset.errors, opts)),
          filter,
          concat("data: ", to_doc(changeset.data, opts)),
          context,
          concat("valid?: ", to_doc(changeset.valid?, opts)),
          select,
          load
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end

    defp arguments(changeset, opts) do
      if changeset.action do
        if Enum.empty?(changeset.action.arguments) do
          empty()
        else
          arg_string =
            changeset.action.arguments
            |> Enum.filter(fn argument ->
              match?({:ok, _}, Ash.Changeset.fetch_argument(changeset, argument.name))
            end)
            |> Map.new(fn argument ->
              value = Ash.Changeset.get_argument(changeset, argument.name)

              if argument.sensitive? do
                {argument.name, Ash.Helpers.redact(value)}
              else
                {argument.name, value}
              end
            end)
            |> to_doc(opts)

          concat(["arguments: ", arg_string])
        end
      else
        empty()
      end
    end
  end

  @type after_action_fun ::
          (t, Ash.Resource.record() ->
             {:ok, Ash.Resource.record()}
             | {:ok, Ash.Resource.record(), [Ash.Notifier.Notification.t()]}
             | {:error, any})

  @type after_transaction_fun ::
          (t, {:ok, Ash.Resource.record()} | {:error, any} ->
             {:ok, Ash.Resource.record()} | {:error, any})

  @type before_action_fun :: (t -> t | {t, %{notifications: [Ash.Notifier.Notification.t()]}})

  @type before_transaction_fun :: (t -> t)

  @type around_action_result ::
          {:ok, Ash.Resource.record(), t(), %{notifications: list(Ash.Notifier.Notification.t())}}
          | {:error, Ash.Error.t()}
  @type around_action_callback :: (t -> around_action_result)
  @type around_action_fun :: (t, around_action_callback -> around_action_result)

  @type around_transaction_result :: {:ok, Ash.Resource.record()} | {:error, any}
  @type around_transaction_callback :: (t -> around_transaction_result)
  @type around_transaction_fun :: (t, around_transaction_callback -> around_transaction_result)

  @phases [
    :atomic,
    :pending,
    :validate,
    :before_action,
    :after_action,
    :before_transaction,
    :after_transaction,
    :around_action,
    :around_transaction
  ]

  @type phase :: unquote(Enum.reduce(@phases, &{:|, [], [&1, &2]}))

  @type t :: %__MODULE__{
          __validated_for_action__: atom | nil,
          action: Ash.Resource.Actions.action() | nil,
          action_failed?: boolean,
          action_type: Ash.Resource.Actions.action_type() | nil,
          after_action: [after_action_fun | {after_action_fun, map}],
          after_transaction: [after_transaction_fun | {after_transaction_fun, map}],
          atomics: Keyword.t(),
          domain: module | nil,
          arguments: %{optional(atom) => any},
          around_action: [around_action_fun | {around_action_fun, map}],
          around_transaction: [around_transaction_fun | {around_transaction_fun, map}],
          attributes: %{optional(atom) => any},
          before_action: [before_action_fun | {before_action_fun, map}],
          before_transaction: [before_transaction_fun | {before_transaction_fun, map}],
          context: map,
          filter: Ash.Filter.t() | nil,
          added_filter: Ash.Filter.t() | nil,
          data: Ash.Resource.record() | nil,
          defaults: [atom],
          errors: [Ash.Error.t()],
          handle_errors:
            nil | (t, error :: any -> :ignore | t | (error :: any) | {error :: any, t}),
          invalid_keys: MapSet.t(),
          params: %{optional(atom | binary) => any},
          phase: phase(),
          relationships: %{
            optional(atom) =>
              %{optional(atom | binary) => any} | [%{optional(atom | binary) => any}]
          },
          resource: module,
          select: [atom] | nil,
          load: keyword(keyword),
          tenant: term(),
          timeout: pos_integer() | nil,
          valid?: boolean
        }

  @type error_info ::
          String.t()
          | [
              {:field, atom()}
              | {:fields, [atom()]}
              | {:message, String.t()}
              | {:value, any()}
            ]
          | %{:__struct__ => atom(), required(atom()) => any()}

  alias Ash.Error.{
    Changes.InvalidArgument,
    Changes.InvalidAttribute,
    Changes.InvalidChanges,
    Changes.InvalidRelationship,
    Changes.NoSuchAttribute,
    Changes.NoSuchRelationship,
    Changes.Required,
    Invalid.NoSuchInput,
    Invalid.NoSuchResource
  }

  require Ash.Tracer
  import Ash.Expr
  require Logger

  defmodule OriginalDataNotAvailable do
    @moduledoc "A value placed in changeset.data to indicate that the original data is not available"
    defstruct reason: :atomic_query_update
    @type t :: %__MODULE__{reason: :atomic_query_update}
  end

  defmacrop maybe_already_validated_error!(changeset, alternative \\ nil) do
    {function, arity} = __CALLER__.function

    if alternative do
      quote do
        changeset = unquote(changeset)

        if changeset.__validated_for_action__ && !changeset.context[:private][:in_before_action?] do
          IO.warn("""
          Changeset has already been validated for action #{inspect(changeset.__validated_for_action__)}.

          For safety, we prevent any changes after that point because they will bypass validations or other action logic.. To proceed anyway,
          you can use `#{unquote(alternative)}/#{unquote(arity)}`. However, you should prefer a pattern like the below, which makes
          any custom changes *before* calling the action.

            Resource
            |> Ash.Changeset.new()
            |> Ash.Changeset.#{unquote(function)}(...)
            |> Ash.Changeset.for_create(...)
          """)
        end
      end
    else
      quote do
        changeset = unquote(changeset)

        if changeset.__validated_for_action__ && !changeset.context[:private][:in_before_action?] do
          IO.warn("""
          Changeset has already been validated for action #{inspect(changeset.__validated_for_action__)}.

          For safety, we prevent any changes using `#{unquote(function)}/#{unquote(arity)}` after that point because they will bypass validations or other action logic.
          Instead, you should change or set this value before calling the action, like so:

            Resource
            |> Ash.Changeset.new()
            |> Ash.Changeset.#{unquote(function)}(...)
            |> Ash.Changeset.for_create(...)
          """)
        end
      end
    end
  end

  @doc """
  A guard which checks if the Changeset is valid.
  """
  @spec is_valid(t) :: Macro.output()
  defguard is_valid(changeset) when is_struct(changeset, __MODULE__) and changeset.valid? == true

  @doc """
  Returns a new changeset over a resource.

  *Warning*: You almost always want to use `for_action` or `for_create`, etc. over this function if possible.

  You can use this to start a changeset and make changes prior to calling `for_action`. This is not typically
  necessary, but can be useful as an escape hatch. For example:

  ```elixir
  Resource
  |> Ash.Changeset.new()
  |> Ash.Changeset.change_attribute(:name, "foobar")
  |> Ash.Changeset.for_action(...)
  ```
  """
  @spec new(Ash.Resource.t() | Ash.Resource.record()) :: t

  def new(record_or_resource) do
    {resource, record, action_type} =
      case record_or_resource do
        %resource{} = record -> {resource, record, :update}
        resource -> {resource, struct(resource), :create}
      end

    tenant =
      record
      |> Map.get(:__metadata__, %{})
      |> Map.get(:tenant, nil)

    context = Ash.Resource.Info.default_context(resource) || %{}

    if Ash.Resource.Info.resource?(resource) do
      %__MODULE__{resource: resource, data: record, action_type: action_type}
      |> set_context(context)
      |> set_tenant(tenant)
    else
      %__MODULE__{
        resource: resource,
        action_type: action_type,
        data: struct(resource)
      }
      |> add_error(NoSuchResource.exception(resource: resource))
      |> set_tenant(tenant)
      |> set_context(context)
    end
  end

  @doc """
  Ensure that only the specified attributes are present in the results.

  The first call to `select/2` will replace the default behavior of selecting
  all attributes. Subsequent calls to `select/2` will combine the provided
  fields unless the `replace?` option is provided with a value of `true`.

  If a field has been deselected, selecting it again will override that (because a single list of fields is tracked for selection)

  Primary key attributes always selected and cannot be deselected.

  When attempting to load a relationship (or manage it with `Ash.Changeset.manage_relationship/3`),
  if the source field is not selected on the query/provided data an error will be produced. If loading
  a relationship with a query, an error is produced if the query does not select the destination field
  of the relationship.

  Datalayers currently are not notified of the `select` for a changeset(unlike queries), and creates/updates select all fields when they are performed.
  A select provided on a changeset sets the unselected fields to `nil` before returning the result.

  Use `ensure_selected/2` if you wish to make sure a field has been selected, without deselecting any other fields.
  """
  def select(changeset, fields, opts \\ []) do
    if opts[:replace?] do
      case fields do
        %MapSet{} = fields -> %{changeset | select: Enum.to_list(fields)}
        fields -> %{changeset | select: Enum.uniq(List.wrap(fields))}
      end
    else
      case fields do
        %MapSet{} ->
          %{
            changeset
            | select: MapSet.union(MapSet.new(changeset.select || []), fields) |> MapSet.to_list()
          }

        fields ->
          %{changeset | select: Enum.uniq(List.wrap(fields) ++ (changeset.select || []))}
      end
    end
  end

  @doc false
  def set_action_select(%{action: nil} = changeset) do
    %{
      changeset
      | action_select:
          MapSet.to_list(
            Ash.Resource.Info.selected_by_default_attribute_names(changeset.resource)
          )
    }
  end

  def set_action_select(changeset) do
    if Ash.DataLayer.data_layer_can?(changeset.resource, :action_select) do
      required =
        Ash.Resource.Info.action_select(changeset.resource, changeset.action.name) || []

      select =
        changeset.select ||
          MapSet.to_list(
            Ash.Resource.Info.selected_by_default_attribute_names(changeset.resource)
          )

      %{
        changeset
        | action_select: Enum.uniq(List.wrap(required) |> Enum.concat(select))
      }
    else
      %{
        changeset
        | action_select:
            MapSet.to_list(
              Ash.Resource.Info.selected_by_default_attribute_names(changeset.resource)
            )
      }
    end
  end

  @doc """
  Calls the provided load statement on the result of the action at the very end of the action.
  """
  @spec load(t(), term()) :: t()
  def load(changeset, load) do
    query =
      changeset.resource
      |> Ash.Query.new()
      |> Map.put(:errors, [])
      |> Ash.Query.load(changeset.load)
      |> Ash.Query.load(load)

    changeset = %{
      changeset
      | load: Enum.concat(changeset.load || [], List.wrap(load))
    }

    Enum.reduce(query.errors, changeset, &add_error(&2, &1))
  end

  @doc """
  Ensures that the given attributes are selected.

  The first call to `select/2` will *limit* the fields to only the provided fields.
  Use `ensure_selected/2` to say "select this field (or these fields) without deselecting anything else".

  See `select/2` for more.
  """
  def ensure_selected(changeset, fields) do
    if changeset.select do
      Ash.Changeset.select(changeset, List.wrap(fields))
    else
      to_select = Ash.Resource.Info.selected_by_default_attribute_names(changeset.resource)

      Ash.Changeset.select(changeset, to_select)
    end
  end

  @doc """
  Ensure the the specified attributes are `nil` in the changeset results.
  """
  def deselect(changeset, fields) do
    select =
      if changeset.select do
        changeset.select
      else
        changeset.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)
      end

    select = select -- List.wrap(fields)

    select(changeset, select, replace?: true)
  end

  def selecting?(changeset, field) do
    case changeset.select do
      nil ->
        not is_nil(Ash.Resource.Info.attribute(changeset.resource, field))

      select ->
        if field in select do
          true
        else
          attribute = Ash.Resource.Info.attribute(changeset.resource, field)

          attribute && attribute.primary_key?
        end
    end || loading?(changeset, field)
  end

  @doc """
  Returns true if the field/relationship or path to field/relationship is being loaded.

  It accepts an atom or a list of atoms, which is treated for as a "path", i.e:

      Resource |> Ash.Changeset.load(friends: [enemies: [:score]]) |> Ash.Changeset.loading?([:friends, :enemies, :score])
      iex> true

      Resource |> Ash.Changeset.load(friends: [enemies: [:score]]) |> Ash.Changeset.loading?([:friends, :score])
      iex> false

      Resource |> Ash.Changeset.load(friends: [enemies: [:score]]) |> Ash.Changeset.loading?(:friends)
      iex> true
  """
  def loading?(changeset, path) do
    changeset.resource
    |> Ash.Query.new()
    |> Ash.Query.load(changeset.load)
    |> Ash.Query.loading?(path)
  end

  @doc """
  Returns a list of attributes, aggregates, relationships, and calculations that are being loaded

  Provide a list of field types to narrow down the returned results.
  """
  def accessing(
        changeset,
        types \\ [:attributes, :relationships, :calculations, :attributes],
        only_public? \\ true
      ) do
    changeset.resource
    |> Ash.Query.new()
    |> Ash.Query.load(changeset.load)
    |> Map.put(:select, changeset.select)
    |> Ash.Query.accessing(types, only_public?)
  end

  @spec fully_atomic_changeset(
          resource :: Ash.Resource.t(),
          action :: atom() | Ash.Resource.Actions.action(),
          params :: map(),
          opts :: Keyword.t()
        ) :: Ash.Changeset.t() | {:not_atomic, String.t()}
  def fully_atomic_changeset(resource, action, params, opts \\ []) do
    action =
      case action do
        action when is_atom(action) -> Ash.Resource.Info.action(resource, action)
        action -> action
      end

    if action.manual do
      {:not_atomic,
       "manual action `#{inspect(resource)}.#{action.name}` cannot be performed atomically"}
    else
      changeset =
        resource
        |> Ash.Changeset.new()
        |> then(fn changeset ->
          if data = opts[:data] do
            Map.put(changeset, :data, data)
          else
            Map.put(changeset, :data, %OriginalDataNotAvailable{})
          end
        end)
        |> Map.put(:context, opts[:context] || %{})
        |> Map.put(:params, params)
        |> Map.put(:action, action)
        |> Map.put(:no_atomic_constraints, opts[:no_atomic_constraints] || [])
        |> Map.put(:action_type, action.type)
        |> Map.put(:atomics, opts[:atomics] || [])
        |> Ash.Changeset.set_tenant(opts[:tenant])

      {changeset, _opts} =
        Ash.Actions.Helpers.set_context_and_get_opts(
          opts[:domain] || Ash.Resource.Info.domain(resource),
          changeset,
          opts
        )

      changeset = set_phase(changeset, :atomic)

      with :ok <- verify_notifiers_support_atomic(resource, action),
           %Ash.Changeset{} = changeset <-
             atomic_params(changeset, action, params, opts),
           %Ash.Changeset{} = changeset <- set_argument_defaults(changeset, action),
           %Ash.Changeset{} = changeset <- require_arguments(changeset, action),
           %Ash.Changeset{} = changeset <- atomic_changes(changeset, action),
           %Ash.Changeset{} = changeset <- atomic_defaults(changeset),
           %Ash.Changeset{} = changeset <- atomic_update(changeset, opts[:atomic_update] || []),
           %Ash.Changeset{} = changeset <-
             hydrate_atomic_refs(changeset, opts[:actor], Keyword.take(opts, [:eager?])),
           %Ash.Changeset{} = changeset <- apply_atomic_constraints(changeset, opts[:actor]) do
        %{
          changeset
          | atomics: Keyword.merge(Map.to_list(changeset.attributes), changeset.atomics),
            attributes: %{}
        }
      else
        {:not_atomic, reason} ->
          {:not_atomic, reason}
      end
    end
    |> case do
      {:not_atomic, reason} ->
        {:not_atomic, reason}

      changeset ->
        clear_phase(changeset)
    end
  end

  defp atomic_defaults(changeset) do
    with %__MODULE__{} <- atomic_static_update_defaults(changeset) do
      atomic_lazy_update_defaults(changeset)
    end
  end

  defp atomic_static_update_defaults(changeset) do
    changeset.resource
    |> Ash.Resource.Info.static_default_attributes(:update)
    |> Enum.reject(fn attribute ->
      Ash.Changeset.changing_attribute?(changeset, attribute.name)
    end)
    |> Enum.reduce_while(changeset, fn attribute, changeset ->
      case Ash.Type.cast_atomic(
             attribute.type,
             attribute.update_default,
             attribute.constraints
           ) do
        {:atomic, atomic} ->
          {:cont, atomic_update(changeset, attribute.name, {:atomic, atomic})}

        {:ok, value} ->
          allow_nil? =
            attribute.allow_nil? and attribute.name not in changeset.action.require_attributes

          if is_nil(value) and !allow_nil? do
            {:cont, add_required_attribute_error(changeset, attribute)}
          else
            {:cont,
             %{
               changeset
               | attributes: Map.put(changeset.attributes, attribute.name, value),
                 atomics: Keyword.delete(changeset.atomics, attribute.name)
             }
             |> store_casted_attribute(attribute.name, value, true)}
          end

        {:error, error} ->
          {:cont,
           add_invalid_errors(attribute.update_default, :attribute, changeset, attribute, error)}

        {:not_atomic, reason} ->
          {:halt, {:not_atomic, reason}}
      end
    end)
  end

  defp atomic_lazy_update_defaults(changeset) do
    changeset.resource
    |> Ash.Resource.Info.lazy_matching_default_attributes(:update)
    |> Enum.concat(
      Ash.Resource.Info.lazy_non_matching_default_attributes(changeset.resource, :update)
    )
    |> Enum.reject(fn attribute ->
      Ash.Changeset.changing_attribute?(changeset, attribute.name)
    end)
    |> Enum.reduce_while(changeset, fn attribute, changeset ->
      cond do
        attribute.update_default == (&DateTime.utc_now/0) ->
          {:cont, atomic_update(changeset, attribute.name, Ash.Expr.expr(now()))}

        attribute.update_default == (&Ash.UUID.generate/0) ->
          {:cont, atomic_update(changeset, attribute.name, Ash.Expr.expr(^Ash.UUID.generate()))}

        true ->
          {:halt,
           {:not_atomic,
            "update_default for `#{inspect(attribute.name)}` cannot be done atomically: #{inspect(attribute.update_default)}"}}
      end
    end)
  end

  defp verify_notifiers_support_atomic(resource, action) do
    resource
    |> Ash.Resource.Info.notifiers()
    |> Enum.filter(fn notifier ->
      notifier.requires_original_data?(resource, action)
    end)
    |> case do
      [] ->
        :ok

      notifiers ->
        {:not_atomic,
         "notifiers #{inspect(notifiers)} require original data for #{inspect(resource)}.#{action.name}"}
    end
  end

  defp atomic_changes(changeset, action) do
    changes =
      action.changes
      |> Enum.concat(Ash.Resource.Info.changes(changeset.resource, changeset.action_type))
      |> then(fn changes ->
        if changeset.action.skip_global_validations? do
          changes
        else
          Enum.concat(
            changes,
            Ash.Resource.Info.validations(changeset.resource, changeset.action_type)
          )
        end
      end)

    context = %{
      actor: changeset.context[:private][:actor],
      tenant: changeset.tenant,
      authorize?: changeset.context[:private][:authorize?] || false,
      tracer: changeset.context[:private][:tracer]
    }

    changeset = set_phase(changeset, :atomic)

    Enum.reduce_while(changes, changeset, fn
      %{change: _} = change, changeset ->
        case run_atomic_change(changeset, change, context) do
          {:not_atomic, reason} ->
            {:halt, {:not_atomic, reason}}

          changeset ->
            {:cont, changeset}
        end

      %{validation: _} = validation, changeset ->
        case run_atomic_validation(changeset, validation, context) do
          {:not_atomic, reason} ->
            {:halt, {:not_atomic, reason}}

          changeset ->
            {:cont, changeset}
        end
    end)
    |> case do
      {:not_atomic, reason} -> {:not_atomic, reason}
      %__MODULE__{} = changeset -> clear_phase(changeset)
    end
  end

  @doc false
  def split_atomic_conditions(%{where: []} = validation, _changeset, _actor, _context) do
    {:ok, validation}
  end

  def split_atomic_conditions(
        %{where: [{module, opts} | rest]} = validation,
        changeset,
        actor,
        context
      ) do
    if module.has_validate?() do
      opts =
        Ash.Actions.Helpers.templated_opts(opts, actor, changeset.arguments, changeset.context)

      {:ok, opts} = module.init(opts)

      case module.validate(
             changeset,
             opts,
             context
           ) do
        :ok -> split_atomic_conditions(%{validation | where: rest}, changeset, actor, context)
        _ -> :skip
      end
    else
      if module.atomic?() do
        case split_atomic_conditions(%{validation | where: rest}, changeset, actor, context) do
          {:ok, %{where: remaining} = validation} ->
            {:ok, %{validation | where: [{module, opts} | remaining]}}

          other ->
            other
        end
      else
        raise "Module #{module} must define one of `atomic/3` or `validate/3`"
      end
    end
  end

  @doc false
  def run_atomic_validation(changeset, %{where: where} = validation, context) do
    with {:atomic, condition} <- atomic_condition(where, changeset, context) do
      case condition do
        false ->
          changeset

        true ->
          do_run_atomic_validation(changeset, validation, context)

        where_condition ->
          do_run_atomic_validation(changeset, validation, context, where_condition)
      end
    end
  end

  defp do_run_atomic_validation(
         changeset,
         %{validation: {module, validation_opts}, message: message},
         context,
         where_condition \\ nil
       ) do
    case List.wrap(
           module.atomic(
             changeset,
             validation_opts,
             struct(Ash.Resource.Validation.Context, Map.put(context, :message, message))
           )
         ) do
      [{:atomic, _, _, _} | _] = atomics ->
        Enum.reduce(atomics, changeset, fn
          {:atomic, _fields, condition_expr, error_expr}, changeset ->
            condition_expr =
              if where_condition do
                expr(^where_condition and ^condition_expr)
              else
                condition_expr
              end

            condition_expr = rewrite_atomics(changeset, condition_expr)

            validate_atomically(changeset, condition_expr, error_expr)
        end)

      [:ok] ->
        changeset

      [{:error, error}] ->
        if message do
          error = override_validation_message(error, message)
          Ash.Changeset.add_error(changeset, error)
        else
          Ash.Changeset.add_error(changeset, error)
        end

      [{:not_atomic, error}] ->
        {:not_atomic, error}
    end
  end

  defp rewrite_atomics(changeset, expr) do
    Ash.Expr.walk_template(expr, fn
      {:_atomic_ref, ref} ->
        atomic_ref(changeset, ref)

      other ->
        other
    end)
  end

  def run_atomic_change(
        changeset,
        %{change: {module, change_opts}, where: where},
        context
      ) do
    change_opts =
      Ash.Expr.fill_template(
        change_opts,
        changeset.context.private[:actor],
        changeset.arguments,
        changeset.context
      )

    with {:atomic, changeset, atomic_changes, validations} <-
           atomic_with_changeset(
             module.atomic(changeset, change_opts, struct(Ash.Resource.Change.Context, context)),
             changeset
           ),
         {:atomic, condition} <- atomic_condition(where, changeset, context) do
      changeset =
        case condition do
          true ->
            apply_atomic_update(changeset, atomic_changes)

          false ->
            changeset

          condition ->
            atomic_changes =
              Map.new(atomic_changes, fn {key, value} ->
                new_value =
                  expr(
                    if ^condition do
                      ^value
                    else
                      ^ref(key)
                    end
                  )

                {key, new_value}
              end)

            apply_atomic_update(changeset, atomic_changes)
        end

      Enum.reduce(
        List.wrap(validations),
        changeset,
        fn {:atomic, _, condition_expr, error_expr}, changeset ->
          validate_atomically(changeset, condition_expr, error_expr)
        end
      )
    else
      {:ok, changeset} ->
        changeset

      {:not_atomic, reason} ->
        {:not_atomic, reason}

      :ok ->
        changeset
    end
  end

  defp apply_atomic_update(changeset, atomics) when is_list(atomics) or is_map(atomics) do
    Enum.reduce(atomics, changeset, fn {key, value}, changeset ->
      apply_atomic_update(changeset, key, value)
    end)
  end

  defp apply_atomic_update(changeset, key, {:atomic, value}) do
    %{
      changeset
      | atomics: Keyword.put(changeset.atomics, key, value),
        no_atomic_constraints: [key | changeset.no_atomic_constraints]
    }
    |> record_atomic_update_for_atomic_upgrade(key, value)
  end

  defp apply_atomic_update(changeset, key, value) do
    attribute = Ash.Resource.Info.attribute(changeset.resource, key)

    value =
      Ash.Expr.walk_template(value, fn
        {:_atomic_ref, field} ->
          atomic_ref(changeset, field)

        other ->
          other
      end)

    case Ash.Type.cast_atomic(attribute.type, value, attribute.constraints) do
      {:atomic, value} ->
        value =
          if attribute.primary_key? do
            value
          else
            set_error_field(value, attribute.name)
          end

        %{changeset | atomics: Keyword.put(changeset.atomics, attribute.name, value)}
        |> record_atomic_update_for_atomic_upgrade(attribute.name, value)

      {:not_atomic, message} ->
        {:not_atomic,
         "Cannot atomically update #{inspect(changeset.resource)}.#{attribute.name}: #{message}"}

      {:ok, value} ->
        allow_nil? =
          attribute.allow_nil? and attribute.name not in changeset.action.require_attributes

        if is_nil(value) and !allow_nil? do
          add_required_attribute_error(changeset, attribute)
        else
          %{changeset | attributes: Map.put(changeset.attributes, attribute.name, value)}
          |> store_casted_attribute(attribute.name, value, true)
        end

      {:error, error} ->
        {:cont, add_invalid_errors(value, :attribute, changeset, attribute, error)}
    end
  end

  defp atomic_with_changeset({:atomic, changeset, atomics}, _changeset),
    do: {:atomic, changeset, atomics, []}

  defp atomic_with_changeset({:atomic, atomics}, changeset), do: {:atomic, changeset, atomics, []}
  defp atomic_with_changeset(other, _), do: other

  defp validate_atomically(changeset, condition_expr, error_expr) do
    %{
      changeset
      | atomic_validations: [{condition_expr, error_expr} | changeset.atomic_validations]
    }
  end

  @doc """
  Gets a reference to a field, or the current atomic update expression of that field.
  """
  def atomic_ref(changeset, field) do
    case Keyword.fetch(changeset.atomics, field) do
      {:ok, atomic} ->
        attribute = Ash.Resource.Info.attribute(changeset.resource, field)
        Ash.Expr.expr(type(^atomic, ^attribute.type, ^attribute.constraints))

      :error ->
        case Map.fetch(changeset.attributes, field) do
          {:ok, new_value} ->
            attribute = Ash.Resource.Info.attribute(changeset.resource, field)
            Ash.Expr.expr(type(^new_value, ^attribute.type, ^attribute.constraints))

          :error ->
            expr(^ref(field))
        end
    end
  end

  @doc false
  def atomic_condition(where, changeset, context) do
    Enum.reduce_while(where, {:atomic, true}, fn {module, validation_opts},
                                                 {:atomic, condition} ->
      case module.atomic(
             changeset,
             validation_opts,
             struct(Ash.Resource.Validation.Context, context)
           ) do
        :ok ->
          {:cont, {:atomic, condition}}

        {:error, _} ->
          {:cont, {:atomic, false}}

        [{:atomic, _, expr, _as_error} | rest] ->
          exprs = [expr | Enum.map(rest, &elem(&1, 2))]

          new_expr =
            Enum.reduce(exprs, condition, fn expr, condition ->
              if condition == true do
                expr
              else
                expr(^condition and ^expr)
              end
            end)

          {:cont, {:atomic, new_expr}}

        {:atomic, _, expr, _as_error} ->
          new_expr =
            if condition == true do
              expr
            else
              expr(^condition and ^expr)
            end

          {:cont, {:atomic, new_expr}}

        {:not_atomic, reason} ->
          {:halt, {:not_atomic, reason}}
      end
    end)
  end

  defp atomic_params(changeset, action, params, opts) do
    if opts[:assume_casted?] do
      Enum.reduce_while(params, changeset, fn {key, value}, changeset ->
        cond do
          has_argument?(action, key) ->
            {:cont, %{changeset | arguments: Map.put(changeset.arguments, key, value)}}

          attribute = Ash.Resource.Info.attribute(changeset.resource, key) ->
            {:cont, atomic_update(changeset, attribute.name, {:atomic, value})}

          match?("_" <> _, key) ->
            {:cont, changeset}

          :* in List.wrap(opts[:skip_unknown_inputs]) ->
            {:cont, changeset}

          key in List.wrap(opts[:skip_unknown_inputs]) ->
            {:cont, changeset}

          true ->
            {:cont,
             add_error(
               changeset,
               NoSuchInput.exception(
                 resource: changeset.resource,
                 action: action.name,
                 input: key,
                 inputs: Ash.Resource.Info.action_inputs(changeset.resource, action.name)
               )
             )}
        end
      end)
    else
      Enum.reduce_while(params, changeset, fn {key, value}, changeset ->
        cond do
          has_argument?(action, key) ->
            {:cont, set_argument(changeset, key, value)}

          attribute = Ash.Resource.Info.attribute(changeset.resource, key) ->
            cond do
              attribute.name in action.accept ->
                {:cont, atomic_update(changeset, attribute.name, value)}

              :* in List.wrap(opts[:skip_unknown_inputs]) ->
                {:cont, changeset}

              key in List.wrap(opts[:skip_unknown_inputs]) ->
                {:cont, changeset}

              match?("_" <> _, key) ->
                {:cont, changeset}

              true ->
                {:cont,
                 add_error(
                   changeset,
                   NoSuchInput.exception(
                     resource: changeset.resource,
                     action: action.name,
                     input: key,
                     inputs: Ash.Resource.Info.action_inputs(changeset.resource, action.name)
                   )
                 )}
            end

          match?("_" <> _, key) ->
            {:cont, changeset}

          :* in List.wrap(opts[:skip_unknown_inputs]) ->
            {:cont, changeset}

          key in List.wrap(opts[:skip_unknown_inputs]) ->
            {:cont, changeset}

          true ->
            {:cont,
             add_error(
               changeset,
               NoSuchInput.exception(
                 resource: changeset.resource,
                 action: action.name,
                 input: key,
                 inputs: Ash.Resource.Info.action_inputs(changeset.resource, action.name)
               )
             )}
        end
      end)
    end
  end

  defp set_error_field(expr, field) do
    Ash.Filter.map(expr, fn
      %Ash.Query.Function.Error{arguments: [module, nested_expr]} = func
      when is_map(nested_expr) and not is_struct(nested_expr) ->
        %{func | arguments: [module, Map.put(nested_expr, :field, field)]}

      other ->
        other
    end)
  end

  @manage_types [:append_and_remove, :append, :remove, :direct_control, :create]

  @doc """
  Constructs a changeset for a given action, and validates it.

  Calls `for_create/4`, `for_update/4` or `for_destroy/4` based on the type of action passed in.

  See those functions for more explanation.
  """
  def for_action(initial, action, params \\ %{}, opts \\ []) do
    resource =
      case initial do
        %__MODULE__{resource: resource} -> resource
        %resource{} -> resource
        resource -> resource
      end

    action = get_action_entity(resource, action)

    case action.type do
      :create ->
        for_create(initial, action, params, opts)

      :update ->
        for_update(initial, action, params, opts)

      :destroy ->
        for_destroy(initial, action, params, opts)

      :read ->
        raise ArgumentError,
              "Passed a read action `#{inspect(resource)}.#{action.name}` into `Ash.Changeset.for_action/4`. Use `Ash.Query.for_read/4` instead."
    end
  end

  @for_create_opts [
    require?: [
      type: :boolean,
      default: false,
      doc:
        "If set to `false`, values are only required when the action is run (instead of immediately)."
    ],
    actor: [
      type: :any,
      doc:
        "set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    authorize?: [
      type: :any,
      doc:
        "set authorize?, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    tracer: [
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
      doc:
        "A tracer to use. Will be carried over to the action. For more information see `Ash.Tracer`."
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
      doc: "set the tenant on the changeset"
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
    ],
    context: [
      type: :map,
      doc: "Context to set on the query, changeset, or input"
    ],
    private_arguments: [
      type: :map,
      doc: "Private argument values to set before validations and changes.",
      default: %{}
    ]
  ]

  @doc false
  def for_create_opts, do: @for_create_opts

  @doc """
  Constructs a changeset for a given create action, and validates it.

  Anything that is modified prior to `for_create/4` is validated against the rules of the action, while *anything after it is not*.
  This runs any `change`s contained on your action. To have your logic execute *only* during the action, you can use `after_action/2`
  or `before_action/2`.

  Multitenancy is *not* validated until an action is called. This allows you to avoid specifying a tenant until just before calling
  the domain action.

  ### Params
  `params` may be attributes, relationships, or arguments. You can safely pass user/form input directly into this function.
  Only public attributes and relationships are supported. If you want to change private attributes as well, see the
  Customization section below. `params` are stored directly as given in the `params` field of the changeset, which is used

  ### Opts

  #{Spark.Options.docs(@for_create_opts)}

  ### Customization

  A changeset can be provided as the first argument, instead of a resource, to allow
  setting specific attributes ahead of time.

  For example:

      MyResource
      |> Ash.Changeset.new()
      |> Ash.Changeset.change_attribute(:foo, 1)
      |> Ash.Changeset.for_create(:create, ...opts)

  Once a changeset has been validated by `for_create/4` (or `for_update/4`), it isn't validated again in the action.
  New changes added are validated individually, though. This allows you to create a changeset according
  to a given action, and then add custom changes if necessary.

  ### What does this function do?

  The following steps are run when calling `Ash.Changeset.for_create/4`.

  - Cast input params | This is any arguments in addition to any accepted attributes
  - Set argument defaults
  - Require any missing arguments
  - Validate all provided attributes are accepted
  - Require any accepted attributes that are `allow_nil?` false
  - Set any default values for attributes
  - Run action changes & validations
  - Run validations, or add them in `before_action` hooks if using `d:Ash.Resource.Dsl.actions.create.validate|before_action?`. Any global validations are skipped if the action has `skip_global_validations?` set to `true`.
  """
  def for_create(initial, action, params \\ %{}, opts \\ []) do
    changeset =
      case initial do
        %__MODULE__{action_type: :create} = changeset ->
          changeset

        resource when is_atom(resource) ->
          new(resource)

        other ->
          raise ArgumentError,
            message: """
            Initial must be a changeset with the action type of `:create`, or a resource.

            Got: #{inspect(other)}
            """
      end

    action =
      get_action_entity(changeset.resource, action) ||
        raise_no_action(changeset.resource, action, :create)

    upsert_condition =
      case opts[:upsert_condition] do
        nil -> action && action.upsert_condition
        other -> other
      end

    case action do
      %Ash.Resource.Actions.Update{name: name} ->
        raise ArgumentError,
          message: """
          Action #{inspect(changeset.resource)}.#{name} was passed to `Ash.Changeset.for_create`, but it is an update action.

          Perhaps you meant to call `Ash.Changeset.for_create` instead?
          """

      _ ->
        :ok
    end

    changeset
    |> set_context(%{
      private: %{
        upsert?: opts[:upsert?] || (action && action.upsert?) || false,
        upsert_identity: opts[:upsert_identity] || (action && action.upsert_identity),
        upsert_fields:
          expand_upsert_fields(
            opts[:upsert_fields] || (action && action.upsert_fields),
            changeset.resource
          ),
        upsert_condition: upsert_condition
      }
    })
    |> then(fn
      changeset when upsert_condition != nil -> filter(changeset, upsert_condition)
      changeset -> changeset
    end)
    |> do_for_action(action, params, opts)
  end

  @for_update_opts @for_create_opts

  @doc false
  def for_update_opts, do: @for_update_opts

  @doc """
  Constructs a changeset for a given update action, and validates it.

  Anything that is modified prior to `for_update/4` is validated against the rules of the action, while *anything after it is not*.

  ### What does this function do?

  The following steps are run when calling `Ash.Changeset.for_update/4`.

  - Cast input params | This is any arguments in addition to any accepted attributes
  - Set argument defaults
  - Require any missing arguments
  - Validate all provided attributes are accepted
  - Require any accepted attributes that are `allow_nil?` false
  - Set any default values for attributes
  - Run action changes & validations
  - Run validations, or add them in `before_action` hooks if using `d:Ash.Resource.Dsl.actions.update.validate|before_action?`. Any global validations are skipped if the action has `skip_global_validations?` set to `true`.
  """
  def for_update(initial, action, params \\ %{}, opts \\ []) do
    changeset =
      case initial do
        # We accept :destroy here to support soft deletes
        %__MODULE__{action_type: type} = changeset when type in [:update, :destroy] ->
          changeset

        %mod{} = struct when mod != __MODULE__ ->
          new(struct)

        other ->
          raise ArgumentError,
            message: """
            Initial must be a changeset with the action type of `:update` or `:destroy`, or a record.

            Got: #{inspect(other)}
            """
      end

    do_for_action(changeset, action, params, opts)
  end

  @doc """
  Constructs a changeset for a given destroy action, and validates it.

  ### Opts

  * `:actor` - set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)
  * `:tenant` - set the tenant on the changeset
  * `:private_arguments` - set private arguments on the changeset before validations and changes are run

  Anything that is modified prior to `for_destroy/4` is validated against the rules of the action, while *anything after it is not*.

  Once a changeset has been validated by `for_destroy/4`, it isn't validated again in the action.
  New changes added are validated individually, though. This allows you to create a changeset according
  to a given action, and then add custom changes if necessary.

  ### What does this function do?

  The following steps are run when calling `Ash.Changeset.for_destroy/4`.

  - Cast input params | This is any arguments in addition to any accepted attributes
  - Set argument defaults
  - Require any missing arguments
  - Validate all provided attributes are accepted
  - Require any accepted attributes that are `allow_nil?` false
  - Set any default values for attributes
  - Run action changes & validations
  - Run validations, or add them in `before_action` hooks if using `d:Ash.Resource.Dsl.actions.destroy.validate|before_action?`. Any global validations are skipped if the action has `skip_global_validations?` set to `true`.
  """
  def for_destroy(initial, action_or_name, params \\ %{}, opts \\ []) do
    changeset =
      case initial do
        %__MODULE__{} = changeset ->
          changeset
          |> Map.put(:action_type, :destroy)

        %_{} = struct ->
          struct
          |> new()
          |> Map.put(:action_type, :destroy)

        other ->
          raise ArgumentError,
            message: """
            Initial must be a changeset with the action type of `:destroy`, or a record.

            Got: #{inspect(other)}
            """
      end

    action =
      get_action_entity(changeset.resource, action_or_name) ||
        raise_no_action(changeset.resource, action_or_name, :destroy)

    domain =
      changeset.domain || opts[:domain] || Ash.Resource.Info.domain(changeset.resource) ||
        Ash.Actions.Helpers.maybe_embedded_domain(changeset.resource) ||
        raise ArgumentError,
          message:
            "Could not determine domain for changeset. Provide the `domain` option or configure a domain in the resource directly."

    changeset = %{changeset | domain: domain}

    if changeset.valid? do
      if action do
        if action.soft? do
          do_for_action(%{changeset | action_type: :destroy}, action, params, opts)
        else
          {changeset, opts} =
            Ash.Actions.Helpers.set_context_and_get_opts(
              domain,
              changeset,
              opts
            )

          name =
            fn ->
              "changeset:" <>
                Ash.Resource.Info.trace_name(changeset.resource) <> ":#{action.name}"
            end

          Ash.Tracer.span :changeset,
                          name,
                          opts[:tracer] do
            Ash.Tracer.telemetry_span [:ash, :changeset], fn ->
              %{
                resource_short_name: Ash.Resource.Info.short_name(changeset.resource)
              }
            end do
              metadata = fn ->
                %{
                  resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                  resource: changeset.resource,
                  actor: opts[:actor],
                  tenant: opts[:tenant],
                  action: action.name,
                  authorize?: opts[:authorize?]
                }
              end

              Ash.Tracer.set_metadata(opts[:tracer], :changeset, metadata)

              changeset =
                Enum.reduce(opts[:private_arguments] || %{}, changeset, fn {k, v}, changeset ->
                  Ash.Changeset.set_argument(changeset, k, v)
                end)

              changeset
              |> Map.put(:action, action)
              |> handle_errors(action.error_handler)
              |> set_actor(opts)
              |> set_authorize(opts)
              |> set_tracer(opts)
              |> set_tenant(opts[:tenant] || changeset.tenant)
              |> cast_params(action, params, opts)
              |> set_argument_defaults(action)
              |> require_arguments(action)
              |> validate_attributes_accepted(action)
              |> run_action_changes(
                action,
                opts[:actor],
                opts[:authorize?],
                opts[:tracer],
                metadata
              )
              |> add_validations(opts[:tracer], metadata, opts[:actor])
              |> mark_validated(action.name)
              |> Map.put(:__validated_for_action__, action.name)
            end
          end
        end
      else
        raise_no_action(changeset.resource, action_or_name, :destroy)
      end
    else
      changeset
    end
  end

  @doc """
  Adds multiple atomic changes to the changeset

  See `atomic_update/3` for more information.
  """
  @spec atomic_update(t(), map() | Keyword.t()) :: t()
  def atomic_update(changeset, atomics) when is_list(atomics) or is_map(atomics) do
    Enum.reduce(atomics, changeset, fn {key, value}, changeset ->
      atomic_update(changeset, key, value)
    end)
  end

  @doc """
  Adds an atomic change to the changeset.

  Atomic changes are applied by the data layer, and as such have guarantees that are not
  given by changes that are based on looking at the previous value and updating it. Here
  is an example of a change that is not safe to do concurrently:

  ```elixir
  change fn changeset, _ ->
    Ash.Changeset.change_attribute(changeset, :score, changeset.data.score + 1)
  end
  ```

  If two processes run this concurrently, they will both read the same value of `score`, and
  set the new score to the same value. This means that one of the increments will be lost.
  If you were to instead do this using `atomic_update`, you would get the correct result:

  ```elixir
  Ash.Changeset.atomic_update(changeset, :score, expr(score + 1))
  ```

  There are drawbacks/things to consider, however. The first is that atomic update results
  are not known until after the action is run. The following functional validation would not
  be able to enforce the score being less than 10, because the atomic happens after the validation.

  ```elixir
  validate fn changeset, _ ->
    if Ash.Changeset.get_attribute(changeset, :score) < 10 do
      :ok
    else
      {:error, field: :score, message: "must be less than 10"}
    end
  end
  ```

  If you want to use atomic updates, it is suggested to write module-based validations & changes,
  and implement the appropriate atomic callbacks on those modules. All builtin validations and changes
  implement these callbacks in addition to the standard callbacks. Validations will only be run atomically
  when the entire action is being run atomically or if one of the relevant fields is being updated atomically.
  """
  @spec atomic_update(t(), atom(), {:atomic, Ash.Expr.t()} | Ash.Expr.t()) :: t()
  def atomic_update(changeset, key, {:atomic, value}) do
    %{
      changeset
      | atomics: Keyword.put(changeset.atomics, key, value),
        no_atomic_constraints: [key | changeset.no_atomic_constraints]
    }
  end

  def atomic_update(changeset, key, value) do
    attribute =
      Ash.Resource.Info.attribute(changeset.resource, key) ||
        raise "Unknown attribute `#{inspect(changeset.resource)}.#{inspect(key)}`"

    value =
      Ash.Expr.walk_template(value, fn
        {:_atomic_ref, field} ->
          atomic_ref(changeset, field)

        other ->
          other
      end)

    case Ash.Type.cast_atomic(attribute.type, value, attribute.constraints) do
      {:atomic, value} ->
        value =
          if attribute.primary_key? do
            value
          else
            set_error_field(value, attribute.name)
          end

        %{changeset | atomics: Keyword.put(changeset.atomics, attribute.name, value)}
        |> record_atomic_update_for_atomic_upgrade(attribute.name, value)

      {:ok, value} ->
        allow_nil? =
          if is_nil(changeset.action) do
            true
          else
            attribute.allow_nil? and attribute.name not in changeset.action.require_attributes
          end

        if is_nil(value) and !allow_nil? do
          add_required_attribute_error(changeset, attribute)
        else
          %{
            changeset
            | attributes: Map.put(changeset.attributes, attribute.name, value),
              atomics: Keyword.delete(changeset.atomics, attribute.name)
          }
          |> store_casted_attribute(attribute.name, value, true)
        end

      {:not_atomic, message} ->
        add_error(
          changeset,
          "Cannot atomically update #{inspect(changeset.resource)}.#{attribute.name}: #{message}"
        )
    end
  end

  @doc false
  def handle_allow_nil_atomics(changeset, actor) do
    changeset.atomics
    |> Enum.reduce(changeset, fn {key, value}, changeset ->
      attribute = Ash.Resource.Info.attribute(changeset.resource, key)

      if attribute.primary_key? do
        changeset
      else
        allow_nil? =
          attribute.allow_nil? and attribute.name not in changeset.action.require_attributes

        value =
          if allow_nil? || not Ash.Expr.can_return_nil?(value) do
            value
          else
            expr(
              if is_nil(^value) do
                error(
                  ^Ash.Error.Changes.Required,
                  %{
                    field: ^attribute.name,
                    type: ^:attribute,
                    resource: ^changeset.resource
                  }
                )
              else
                ^value
              end
            )
          end

        %{changeset | atomics: Keyword.put(changeset.atomics, key, value)}
      end
    end)
    |> Ash.Changeset.hydrate_atomic_refs(actor, eager?: true)
  end

  @doc """
  Set the result of the action. This will prevent running the underlying datalayer behavior
  """
  @spec set_result(t(), term) :: t()
  def set_result(changeset, result) do
    set_context(changeset, %{private: %{action_result: result}})
  end

  @doc """
  Turns the special case {:replace, fields}, :replace_all and {:replace_all_except, fields} upsert_fields
  options into a list of fields
  """
  def expand_upsert_fields({:replace, fields}, _) do
    fields
  end

  def expand_upsert_fields(:replace_all, resource) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.map(fn %{name: name} -> name end)
  end

  def expand_upsert_fields({:replace_all_except, except_fields}, resource) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.map(fn %{name: name} -> name end)
    |> Enum.reject(fn name -> name in except_fields end)
  end

  def expand_upsert_fields(fields, _), do: fields

  @spec set_on_upsert(t(), list(atom)) :: Keyword.t()
  def set_on_upsert(changeset, upsert_keys) do
    keys = upsert_keys || Ash.Resource.Info.primary_key(changeset.resource)

    if changeset.context[:private][:upsert_fields] do
      Keyword.new(changeset.context[:private][:upsert_fields], fn key ->
        {key, Ash.Changeset.get_attribute(changeset, key)}
      end)
    else
      explicitly_changing_attributes =
        Enum.map(
          Map.keys(changeset.attributes) -- Map.get(changeset, :defaults, []) -- keys,
          fn key ->
            {key, Ash.Changeset.get_attribute(changeset, key)}
          end
        )

      changeset
      |> upsert_update_defaults()
      |> Keyword.merge(explicitly_changing_attributes)
    end
  end

  defp upsert_update_defaults(changeset) do
    changeset.resource
    |> static_defaults()
    |> Enum.concat(lazy_matching_defaults(changeset.resource))
    |> Enum.concat(lazy_non_matching_defaults(changeset.resource))
  end

  defp static_defaults(resource) do
    resource
    |> Ash.Resource.Info.static_default_attributes(:update)
    |> Enum.map(&{&1.name, &1.update_default})
  end

  defp lazy_non_matching_defaults(resource) do
    resource
    |> Ash.Resource.Info.lazy_non_matching_default_attributes(:update)
    |> Enum.map(&{&1.name, &1.update_default})
  end

  defp lazy_matching_defaults(resource) do
    resource
    |> Ash.Resource.Info.lazy_matching_default_attributes(:update)
    |> Enum.group_by(& &1.update_default)
    |> Enum.flat_map(fn {default_fun, attributes} ->
      default_value =
        case default_fun do
          function when is_function(function) ->
            function.()

          {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
            apply(m, f, a)
        end

      Enum.map(attributes, &{&1.name, default_value})
    end)
  end

  defp do_for_action(changeset, action_or_name, params, opts) do
    domain =
      changeset.domain || opts[:domain] || Ash.Resource.Info.domain(changeset.resource) ||
        Ash.Actions.Helpers.maybe_embedded_domain(changeset.resource) ||
        raise ArgumentError,
          message:
            "Could not determine domain for changeset. Provide the `domain` option or configure a domain in the resource directly."

    changeset = %{changeset | domain: domain}

    if changeset.valid? do
      action = get_action_entity(changeset.resource, action_or_name)

      {changeset, opts} =
        Ash.Actions.Helpers.set_context_and_get_opts(
          domain,
          %{changeset | action: action},
          opts
        )

      if action do
        name =
          fn ->
            "changeset:" <> Ash.Resource.Info.trace_name(changeset.resource) <> ":#{action.name}"
          end

        Ash.Tracer.span :changeset,
                        name,
                        opts[:tracer] do
          Ash.Tracer.telemetry_span [:ash, :changeset], fn ->
            %{
              resource_short_name: Ash.Resource.Info.short_name(changeset.resource)
            }
          end do
            metadata = fn ->
              %{
                resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                resource: changeset.resource,
                actor: opts[:actor],
                tenant: opts[:tenant],
                action: action.name,
                authorize?: opts[:authorize?]
              }
            end

            Ash.Tracer.set_metadata(opts[:tracer], :changeset, metadata)

            changeset =
              Enum.reduce(opts[:private_arguments] || %{}, changeset, fn {k, v}, changeset ->
                Ash.Changeset.set_argument(changeset, k, v)
              end)

            changeset =
              changeset
              |> prepare_changeset_for_action(action, opts)
              |> handle_params(action, params, opts)
              |> run_action_changes(
                action,
                opts[:actor],
                opts[:authorize?],
                opts[:tracer],
                metadata
              )
              |> add_validations(opts[:tracer], metadata, opts[:actor])
              |> mark_validated(action.name)
              |> eager_validate_identities()
              |> Map.put(:__validated_for_action__, action.name)

            if Keyword.get(opts, :require?, true) do
              require_values(changeset, action.type)
            else
              changeset
            end
          end
        end
      else
        raise_no_action(changeset.resource, action_or_name, changeset.action_type)
      end
    else
      action = get_action_entity(changeset.resource, action_or_name)

      {changeset, _opts} =
        Ash.Actions.Helpers.set_context_and_get_opts(
          domain,
          changeset,
          opts
        )

      %{changeset | action: action}
    end
  end

  @doc """
  Checks if an argument is not nil or an attribute is not nil, either in the original data, or that it is not being changed to a `nil` value if it is changing.

  This also accounts for the `accessing_from` context that is set when using `manage_relationship`, so it is aware that a particular value
  *will* be set by `manage_relationship` even if it isn't currently being set.
  """
  def present?(changeset, attribute) do
    arg_or_attribute_value =
      case Ash.Changeset.fetch_argument(changeset, attribute) do
        {:ok, nil} ->
          Ash.Changeset.get_attribute(changeset, attribute)

        :error ->
          Ash.Changeset.get_attribute(changeset, attribute)

        {:ok, value} ->
          {:ok, value}
      end

    arg_or_attribute_value =
      case arg_or_attribute_value do
        %Ash.NotLoaded{} ->
          nil

        %Ash.ForbiddenField{} ->
          nil

        other ->
          other
      end

    not is_nil(arg_or_attribute_value) ||
      belongs_to_attr_of_rel_being_managed?(attribute, changeset, true) ||
      is_belongs_to_rel_being_managed?(attribute, changeset, true)
  end

  @doc """
  Checks if an attribute is not nil, either in the original data, or that it is not being changed to a `nil` value if it is changing.

  This also accounts for the `accessing_from` context that is set when using `manage_relationship`, so it is aware that a particular value
  *will* be set by `manage_relationship` even if it isn't currently being set.
  """
  def attribute_present?(changeset, attribute) do
    attribute_value = Ash.Changeset.get_attribute(changeset, attribute)

    attribute_value =
      case attribute_value do
        %Ash.NotLoaded{} ->
          nil

        %Ash.ForbiddenField{} ->
          nil

        other ->
          other
      end

    not is_nil(attribute_value) ||
      belongs_to_attr_of_rel_being_managed?(attribute, changeset, true) ||
      is_belongs_to_rel_being_managed?(attribute, changeset, true)
  end

  def prepare_changeset_for_action(changeset, action, opts) do
    changeset
    |> Map.put(:action, action)
    |> reset_arguments()
    |> handle_errors(action.error_handler)
    |> set_actor(opts)
    |> set_authorize(opts)
    |> set_tracer(opts)
    |> timeout(changeset.timeout || opts[:timeout])
    |> set_tenant(opts[:tenant] || changeset.tenant || changeset.data.__metadata__[:tenant])
    |> Map.put(:action_type, action.type)
  end

  defp reset_arguments(%{arguments: arguments} = changeset) do
    Enum.reduce(arguments, changeset, fn {key, value}, changeset ->
      set_argument(changeset, key, value)
    end)
  end

  def handle_params(changeset, action, params, handle_params_opts \\ []) do
    if Keyword.get(handle_params_opts, :cast_params?, true) do
      cast_params(changeset, action, params || %{}, handle_params_opts)
    else
      changeset
    end
    |> set_argument_defaults(action)
    |> require_arguments(action)
    |> validate_attributes_accepted(action)
    |> require_values(action.type, false, action.require_attributes)
    |> set_defaults(changeset.action_type, false)
  end

  defp get_action_entity(resource, name) when is_atom(name),
    do: Ash.Resource.Info.action(resource, name)

  defp get_action_entity(_resource, %struct{} = action)
       when struct in [
              Ash.Resource.Actions.Update,
              Ash.Resource.Actions.Create,
              Ash.Resource.Actions.Destroy
            ] do
    action
  end

  defp get_action_entity(_resource, action) do
    raise ArgumentError, "Invalid value provided for action: #{inspect(action)}"
  end

  defp eager_validate_identities(changeset) do
    identities =
      changeset.resource
      |> Ash.Resource.Info.identities()

    case identities do
      [] ->
        changeset

      identities ->
        Enum.reduce(identities, changeset, fn identity, changeset ->
          changeset =
            if identity.eager_check_with do
              validate_identity(changeset, identity, identity.eager_check_with)
            else
              changeset
            end

          if identity.pre_check_with do
            before_action(changeset, &validate_identity(&1, identity, identity.pre_check_with))
          else
            changeset
          end
        end)
    end
  end

  defp validate_identity(
         %{context: %{private: %{upsert?: true, upsert_identity: name}}} = changeset,
         %{name: name},
         _domain
       ) do
    changeset
  end

  defp validate_identity(
         %{action: %{soft?: true}} = changeset,
         identity,
         domain
       ) do
    do_validate_identity(changeset, identity, domain)
  end

  defp validate_identity(
         %{action: %{type: type}} = changeset,
         identity,
         domain
       )
       when type in [:create, :update] do
    do_validate_identity(changeset, identity, domain)
  end

  defp validate_identity(
         %{action: %{type: type}} = changeset,
         identity,
         domain
       )
       when type in [:create, :update] do
    do_validate_identity(changeset, identity, domain)
  end

  defp validate_identity(changeset, _, _), do: changeset

  defp do_validate_identity(changeset, identity, domain) do
    if changeset.context[:private][:upsert_identity] == identity.name do
      changeset
    else
      if changeset.action_type == :create ||
           Enum.any?(identity.keys, &changing_attribute?(changeset, &1)) do
        action = Ash.Resource.Info.primary_action(changeset.resource, :read).name

        if Enum.any?(identity.keys, fn key ->
             Ash.Resource.Info.calculation(changeset.resource, key)
           end) do
          raise ArgumentError, "Cannot pre or eager check an identity based on calculated fields."
        end

        values =
          Enum.map(identity.keys, fn key ->
            case Ash.Changeset.get_attribute(changeset, key) do
              nil ->
                {key, is_nil: true}

              value ->
                {key, value}
            end
          end)

        if identity.nils_distinct? && Enum.any?(values, &(elem(&1, 1) == [is_nil: true])) do
          changeset
        else
          tenant =
            if identity.all_tenants? do
              unless Ash.Resource.Info.multitenancy_global?(changeset.resource) do
                raise ArgumentError,
                  message: """
                  Cannot pre or eager check an identity that has `all_tenants?: true`
                  unless the resource supports global multitenancy.
                  """
              end

              nil
            else
              changeset.tenant
            end

          changeset.resource
          |> Ash.Query.for_read(action, %{},
            tenant: tenant,
            actor: changeset.context[:private][:actor],
            authorize?: changeset.context[:private][:authorize?],
            tracer: changeset.context[:private][:tracer],
            domain: domain
          )
          |> Ash.Query.do_filter(values)
          |> Ash.Query.limit(1)
          |> Ash.Query.set_context(%{private: %{internal?: true}})
          |> Ash.read_one(authorize?: false)
          |> case do
            {:ok, nil} ->
              changeset

            {:ok, _} ->
              error =
                Ash.Error.Changes.InvalidChanges.exception(
                  fields: identity.keys,
                  message: identity.message || "has already been taken"
                )

              add_error(changeset, error)

            {:error, error} ->
              add_error(changeset, error)
          end
        end
      else
        changeset
      end
    end
  end

  defp require_arguments(changeset, action) do
    action.arguments
    |> Enum.filter(&(&1.allow_nil? == false))
    |> Enum.reduce(changeset, fn argument, changeset ->
      case fetch_argument(changeset, argument.name) do
        {:ok, value} when not is_nil(value) ->
          changeset

        _ ->
          if argument.name in changeset.invalid_keys do
            changeset
          else
            add_error(
              changeset,
              Ash.Error.Changes.Required.exception(
                resource: changeset.resource,
                field: argument.name,
                type: :argument
              )
            )
          end
      end
    end)
  end

  defp set_argument_defaults(changeset, action) do
    Enum.reduce(action.arguments, changeset, fn argument, changeset ->
      case fetch_argument(changeset, argument.name) do
        :error ->
          if is_nil(argument.default) do
            changeset
          else
            %{
              changeset
              | arguments: Map.put(changeset.arguments, argument.name, default(:create, argument))
            }
          end

        _ ->
          changeset
      end
    end)
  end

  defp set_actor(changeset, opts) do
    if Keyword.has_key?(opts, :actor) do
      put_context(changeset, :private, %{actor: opts[:actor]})
    else
      changeset
    end
  end

  defp set_authorize(changeset, opts) do
    if Keyword.has_key?(opts, :authorize?) do
      put_context(changeset, :private, %{authorize?: opts[:authorize?]})
    else
      changeset
    end
  end

  defp set_tracer(changeset, opts) do
    if Keyword.has_key?(opts, :tracer) do
      put_context(changeset, :private, %{tracer: opts[:tracer]})
    else
      changeset
    end
  end

  defp raise_no_action(resource, action, type) do
    available_actions =
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.filter(&(&1.type == type))
      |> Enum.map_join("\n", &"    - `#{inspect(&1.name)}`")

    raise ArgumentError,
      message: """
      No such #{type} action on resource #{inspect(resource)}: #{String.slice(inspect(action), 0..50)}

      Example Call:

        Ash.Changeset.for_#{type}(changeset_or_record, :action_name, input, options)

      Available #{type} actions:

      #{available_actions}
      """
  end

  defp mark_validated(changeset, action_name) do
    %{changeset | __validated_for_action__: action_name}
  end

  @doc false
  def validate_multitenancy(changeset) do
    if Ash.Resource.Info.multitenancy_strategy(changeset.resource) &&
         not Ash.Resource.Info.multitenancy_global?(changeset.resource) &&
         is_nil(changeset.tenant) do
      add_error(
        changeset,
        "#{inspect(changeset.resource)} changesets require a tenant to be specified"
      )
    else
      changeset
    end
  end

  defp cast_params(changeset, action, params, opts) do
    changeset = %{
      changeset
      | params: Map.merge(changeset.params, Enum.into(params, %{})),
        casted_arguments: %{},
        casted_attributes: %{}
    }

    skip_unknown_inputs = opts[:skip_unknown_inputs] || []

    Enum.reduce(params, changeset, fn {name, value}, changeset ->
      cond do
        !Ash.Resource.Info.action_input?(changeset.resource, action.name, name) ->
          cond do
            :* in skip_unknown_inputs ->
              changeset

            name in skip_unknown_inputs ->
              changeset

            match?("_" <> _, name) ->
              changeset

            true ->
              add_error(
                changeset,
                NoSuchInput.exception(
                  resource: changeset.resource,
                  action: action.name,
                  input: name,
                  inputs: Ash.Resource.Info.action_inputs(changeset.resource, action.name)
                )
              )
          end

        argument = get_action_argument(action, name) ->
          do_set_argument(changeset, argument.name, value, true)

        attr = Ash.Resource.Info.attribute(changeset.resource, name) ->
          if attr.writable? && attr.name in changeset.action.accept do
            do_change_attribute(changeset, attr.name, value, true)
          else
            cond do
              name in skip_unknown_inputs ->
                changeset

              match?("_" <> _, name) ->
                changeset

              :* in skip_unknown_inputs ->
                changeset

              true ->
                add_error(
                  changeset,
                  NoSuchInput.exception(
                    resource: changeset.resource,
                    action: action.name,
                    input: name,
                    inputs: Ash.Resource.Info.action_inputs(changeset.resource, action.name)
                  )
                )
            end
          end

        true ->
          changeset
      end
    end)
  end

  defp get_action_argument(action, name) when is_atom(name) do
    Enum.find(action.arguments, &(&1.public? && &1.name == name))
  end

  defp get_action_argument(action, name) when is_binary(name) do
    Enum.find(action.arguments, &(to_string(&1.name) == name))
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(to_string(&1.name) == name))
  end

  defp validate_attributes_accepted(changeset, %{accept: nil}), do: changeset

  defp validate_attributes_accepted(changeset, %{accept: accepted_attributes}) do
    changeset.attributes
    |> Map.keys()
    |> Kernel.--(accepted_attributes)
    |> Enum.reduce(changeset, fn key, changeset ->
      add_error(
        changeset,
        InvalidAttribute.exception(
          field: key,
          message: "cannot be changed",
          value: changeset.attributes[key]
        )
      )
    end)
  end

  defp run_action_changes(changeset, %{changes: changes}, actor, authorize?, tracer, metadata) do
    changeset = set_phase(changeset, :validate)
    changes = changes ++ Ash.Resource.Info.changes(changeset.resource, changeset.action_type)

    context = %{
      actor: actor,
      tenant: changeset.tenant,
      authorize?: authorize? || false,
      tracer: tracer
    }

    Enum.reduce(changes, changeset, fn
      %{only_when_valid?: true}, %{valid?: false} = changeset ->
        changeset

      %{always_atomic?: true, change: {module, _}} = change, changeset ->
        if changeset.action.type == :create do
          Ash.Changeset.add_error(
            changeset,
            Ash.Error.Framework.CanNotBeAtomic.exception(
              resource: changeset.resource,
              change: module,
              reason: "Create actions cannot be made atomic"
            )
          )
        else
          case run_atomic_change(changeset, change, context) do
            {:not_atomic, reason} ->
              Ash.Changeset.add_error(
                changeset,
                "Change #{inspect(module)} was configured with `always_atomic?` to `true`, but could not be done atomically: #{reason}"
              )

            changeset ->
              changeset
          end
        end

      %{always_atomic?: true, validation: {module, _}} = change, changeset ->
        if changeset.action.type == :create do
          Ash.Changeset.add_error(
            changeset,
            Ash.Error.Framework.CanNotBeAtomic.exception(
              resource: changeset.resource,
              change: module,
              reason: "Create actions cannot be made atomic"
            )
          )
        else
          case run_atomic_validation(changeset, change, context) do
            {:not_atomic, reason} ->
              Ash.Changeset.add_error(
                changeset,
                "Validation #{change.module} must be run atomically, but it could not be: #{reason}"
              )

            changeset ->
              changeset
          end
        end

      %{change: {module, opts}, where: where} = change, changeset ->
        if module.has_change?() do
          if Enum.all?(where || [], fn {module, opts} ->
               Ash.Tracer.span :validation,
                               fn -> "change condition: #{inspect(module)}" end,
                               tracer do
                 Ash.Tracer.telemetry_span [:ash, :validation], fn ->
                   %{
                     resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                     validation: inspect(module)
                   }
                 end do
                   Ash.Tracer.set_metadata(tracer, :validation, metadata)

                   opts =
                     Ash.Expr.fill_template(
                       opts,
                       actor,
                       changeset.arguments,
                       changeset.context
                     )

                   module.validate(
                     changeset,
                     opts,
                     struct(Ash.Resource.Validation.Context, context)
                   ) == :ok
                 end
               end
             end) do
            Ash.Tracer.span :change, fn -> "change: #{inspect(module)}" end, tracer do
              Ash.Tracer.telemetry_span [:ash, :change], fn ->
                %{
                  resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                  change: inspect(module)
                }
              end do
                {:ok, opts} = module.init(opts)

                Ash.Tracer.set_metadata(tracer, :change, metadata)

                opts =
                  Ash.Expr.fill_template(
                    opts,
                    actor,
                    changeset.arguments,
                    changeset.context
                  )

                module.change(
                  changeset,
                  opts,
                  struct(Ash.Resource.Change.Context, context)
                )
              end
            end
          else
            changeset
          end
        else
          if changeset.action.type == :create do
            Ash.Changeset.add_error(
              changeset,
              Ash.Error.Framework.CanNotBeAtomic.exception(
                resource: changeset.resource,
                change: module,
                reason: "Create actions cannot be made atomic"
              )
            )
          else
            case run_atomic_change(changeset, change, context) do
              {:not_atomic, reason} ->
                Ash.Changeset.add_error(
                  changeset,
                  "Change #{inspect(module)} must be atomic, but could not be done atomically: #{reason}"
                )

              changeset ->
                changeset
            end
          end
        end

      %{validation: _} = validation, changeset ->
        validate(changeset, validation, tracer, metadata, actor)
    end)
    |> clear_phase()
  end

  @doc false
  def hydrate_atomic_refs(changeset, actor, opts \\ []) do
    changeset
    |> add_atomic_validations(actor, opts)
    |> do_hydrate_atomic_refs(actor)
    |> case do
      {:ok, hydrated_changeset} ->
        hydrated_changeset
        |> extract_atomic_eager_errors(actor, opts)
        |> case do
          %Ash.Changeset{} = changeset ->
            %{changeset | atomic_validations: []}

          other ->
            other
        end

      other ->
        other
    end
  end

  defp do_hydrate_atomic_refs(changeset, actor) do
    Enum.reduce_while(
      changeset.atomics,
      {:ok, %{changeset | atomics: []}},
      fn {key, expr}, {:ok, changeset} ->
        expr =
          Ash.Expr.fill_template(
            expr,
            actor,
            changeset.arguments,
            changeset.context,
            changeset
          )

        case Ash.Filter.hydrate_refs(expr, %{resource: changeset.resource, public?: false}) do
          {:ok, expr} ->
            {:cont, {:ok, %{changeset | atomics: Keyword.put(changeset.atomics, key, expr)}}}

          {:error, error} ->
            {:halt,
             {:not_atomic, "Failed to validate expression #{inspect(expr)}: #{inspect(error)}"}}
        end
      end
    )
  end

  @doc false
  def apply_atomic_constraints(changeset, actor, opts \\ []) do
    changeset
    |> do_apply_atomic_constraints()
    |> do_hydrate_atomic_refs(actor)
    |> case do
      {:ok, changeset} ->
        changeset

      {:not_atomic, error} ->
        add_error(changeset, error)
    end
    |> extract_atomic_eager_errors(actor, opts)
  end

  defp do_apply_atomic_constraints(%Ash.Changeset{} = changeset) do
    Enum.reduce(changeset.atomics, %{changeset | atomics: []}, fn {key, value}, changeset ->
      attribute = Ash.Resource.Info.attribute(changeset.resource, key)

      if key in changeset.no_atomic_constraints do
        value =
          if attribute.primary_key? do
            value
          else
            set_error_field(value, attribute.name)
          end

        %{changeset | atomics: Keyword.put(changeset.atomics, key, value)}
      else
        case Ash.Type.apply_atomic_constraints(attribute.type, value, attribute.constraints) do
          {:ok, ^value} ->
            %{changeset | atomics: Keyword.put(changeset.atomics, key, value)}

          {:ok, value} ->
            value = expr(type(^value, ^attribute.type, ^attribute.constraints))

            value =
              if attribute.primary_key? do
                value
              else
                set_error_field(value, attribute.name)
              end

            %{changeset | atomics: Keyword.put(changeset.atomics, key, value)}

          {:error, error} ->
            add_error(changeset, error)
        end
      end
    end)
  end

  defp do_apply_atomic_constraints(value), do: value

  defp extract_atomic_eager_errors(changeset, _actor, opts) do
    if Keyword.get(opts, :eager?, true) do
      Enum.reduce(
        changeset.atomics,
        changeset,
        fn
          {_key,
           %Ash.Query.Function.Error{
             arguments: arguments
           } = error},
          changeset ->
            Enum.reduce_while(arguments, {:ok, []}, fn argument, {:ok, args} ->
              case Ash.Expr.eval(argument,
                     resource: changeset.resource,
                     unknown_on_unknown_refs?: true
                   ) do
                {:ok, value} ->
                  {:cont, {:ok, [value | args]}}

                _ ->
                  {:halt, :error}
              end
            end)
            |> case do
              {:ok, args} ->
                error = %{error | arguments: Enum.reverse(args)}

                case Ash.Expr.eval(error,
                       resource: changeset.resource,
                       unknown_on_unknown_refs?: true
                     ) do
                  {:error, error} ->
                    Ash.Changeset.add_error(changeset, error)

                  _ ->
                    changeset
                end

              _ ->
                changeset
            end

          {_key, _value}, changeset ->
            changeset
        end
      )
    else
      changeset
    end
  end

  @doc false
  def add_atomic_validations(changeset, actor, opts) do
    eager? = Keyword.get(opts, :eager?, true)

    changeset.atomic_validations
    |> Enum.reduce_while(changeset, fn {condition_expr, error_expr}, changeset ->
      condition_expr =
        Ash.Expr.fill_template(
          condition_expr,
          actor,
          changeset.arguments,
          changeset.context,
          changeset
        )

      error_expr =
        Ash.Expr.fill_template(
          error_expr,
          actor,
          changeset.arguments,
          changeset.context,
          changeset
        )

      with {:expr, {:ok, condition_expr}, _expr} <-
             {:expr,
              Ash.Filter.hydrate_refs(condition_expr, %{
                resource: changeset.resource,
                public?: false
              }), condition_expr},
           {:expr, {:ok, error_expr}, _} <-
             {:expr,
              Ash.Filter.hydrate_refs(error_expr, %{resource: changeset.resource, public?: false}),
              error_expr} do
        eager_condition_expr =
          if eager? do
            Ash.Expr.eval(condition_expr,
              resource: changeset.resource,
              unknown_on_unknown_refs?: true
            )
          else
            {:ok, condition_expr}
          end

        eager_error_expr =
          if eager? do
            Ash.Expr.eval(error_expr,
              resource: changeset.resource,
              unknown_on_unknown_refs?: true
            )
          else
            {:ok, error_expr}
          end

        case extract_eager_error(eager_condition_expr, eager_error_expr, eager?) do
          {:ok, error} ->
            {:cont,
             add_error(
               changeset,
               error
             )}

          :error ->
            if changeset.action.type == :update || Map.get(changeset.action, :soft?) do
              [first_pkey_field | _] = Ash.Resource.Info.primary_key(changeset.resource)

              full_atomic_update =
                expr(
                  if ^condition_expr do
                    ^error_expr
                  else
                    ^atomic_ref(changeset, first_pkey_field)
                  end
                )

              case Ash.Filter.hydrate_refs(full_atomic_update, %{
                     resource: changeset.resource,
                     public: false
                   }) do
                {:ok, full_atomic_update} ->
                  {:cont,
                   atomic_update(
                     changeset,
                     first_pkey_field,
                     full_atomic_update
                   )}

                {:error, error} ->
                  {:halt,
                   {:not_atomic,
                    "Failed to validate expression #{inspect(full_atomic_update)}: #{inspect(error)}"}}
              end
            else
              {:cont,
               filter(
                 changeset,
                 expr(
                   if ^condition_expr do
                     ^error_expr
                   else
                     true
                   end
                 )
               )}
            end
        end
      else
        {:expr, {:error, error}, expr} ->
          {:halt,
           {:not_atomic, "Failed to validate expression #{inspect(expr)}: #{inspect(error)}"}}
      end
    end)
  end

  defp extract_eager_error({:ok, true}, {:error, %{class: :invalid} = error}, true) do
    {:ok, error}
  end

  defp extract_eager_error(_, _, _), do: :error

  @doc false
  def set_defaults(changeset, action_type, lazy? \\ false)

  def set_defaults(changeset, :create, lazy?) do
    with_static_defaults =
      changeset.resource
      |> Ash.Resource.Info.static_default_attributes(:create)
      |> Enum.reduce(changeset, fn attribute, changeset ->
        if changing_attribute?(changeset, attribute.name) do
          changeset
        else
          changeset
          |> force_change_attribute(attribute.name, default(:create, attribute))
          |> Map.update!(:defaults, fn defaults ->
            [attribute.name | defaults]
          end)
        end
      end)
      |> Map.update!(:defaults, &Enum.uniq/1)

    if lazy? do
      set_lazy_defaults(with_static_defaults, :create)
    else
      with_static_defaults
    end
    |> Map.update!(:defaults, &Enum.uniq/1)
  end

  def set_defaults(changeset, :update, lazy?) do
    with_static_defaults =
      changeset.resource
      |> Ash.Resource.Info.static_default_attributes(:update)
      |> Enum.reduce(changeset, fn attribute, changeset ->
        if changing_attribute?(changeset, attribute.name) do
          changeset
        else
          changeset
          |> force_change_attribute(attribute.name, default(:update, attribute))
          |> Map.update!(:defaults, fn defaults ->
            [attribute.name | defaults]
          end)
        end
      end)

    if lazy? do
      set_lazy_defaults(with_static_defaults, :update)
    else
      with_static_defaults
    end
    |> Map.update!(:defaults, &Enum.uniq/1)
  end

  def set_defaults(changeset, _, _) do
    changeset
  end

  defp set_lazy_defaults(changeset, type) do
    changeset
    |> set_lazy_non_matching_defaults(type)
    |> set_lazy_matching_defaults(type)
  end

  defp set_lazy_non_matching_defaults(changeset, type) do
    changeset.resource
    |> Ash.Resource.Info.lazy_non_matching_default_attributes(type)
    |> Enum.reduce(changeset, fn attribute, changeset ->
      if changing_attribute?(changeset, attribute.name) do
        changeset
      else
        changeset
        |> force_change_attribute(attribute.name, default(type, attribute))
        |> Map.update!(:defaults, fn defaults ->
          [attribute.name | defaults]
        end)
      end
    end)
  end

  defp set_lazy_matching_defaults(changeset, type) do
    changeset.resource
    |> Ash.Resource.Info.lazy_matching_default_attributes(type)
    |> Enum.group_by(fn attribute ->
      case type do
        :create ->
          attribute.default

        :update ->
          attribute.update_default
      end
    end)
    |> Enum.reduce(changeset, fn {default_fun, attributes}, changeset ->
      default_value =
        case default_fun do
          function when is_function(function) ->
            function.()

          {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
            apply(m, f, a)
        end

      Enum.reduce(attributes, changeset, fn attribute, changeset ->
        if changing_attribute?(changeset, attribute.name) do
          changeset
        else
          changeset
          |> force_change_attribute(attribute.name, default_value)
          |> Map.update!(:defaults, fn defaults ->
            [attribute.name | defaults]
          end)
        end
      end)
    end)
  end

  defp default(:create, %{default: {mod, func, args}}), do: apply(mod, func, args)
  defp default(:create, %{default: function}) when is_function(function, 0), do: function.()
  defp default(:create, %{default: value}), do: value

  defp default(:update, %{update_default: {mod, func, args}}), do: apply(mod, func, args)

  defp default(:update, %{update_default: function}) when is_function(function, 0),
    do: function.()

  defp default(:update, %{update_default: value}), do: value

  defp add_validations(changeset, tracer, metadata, actor) do
    if changeset.action.skip_global_validations? do
      changeset
    else
      changeset.resource
      # We use the `changeset.action_type` to support soft deletes
      # Because a delete is an `update` with an action type of `update`
      |> Ash.Resource.Info.validations(changeset.action_type)
      |> then(fn validations ->
        if changeset.action.delay_global_validations? do
          Enum.map(validations, &%{&1 | before_action?: true})
        else
          validations
        end
      end)
      |> Enum.reduce(changeset, &validate(&2, &1, tracer, metadata, actor))
    end
  end

  defp validate(changeset, validation, tracer, metadata, actor) do
    if validation.module.has_validate?() &&
         Enum.all?(validation.where, fn {module, _} ->
           module.has_validate?()
         end) do
      if validation.before_action? do
        before_action(
          changeset,
          fn changeset ->
            if validation.only_when_valid? and not changeset.valid? do
              changeset
            else
              do_validation(changeset, validation, tracer, metadata, actor)
            end
          end,
          append?: true
        )
      else
        if validation.only_when_valid? and not changeset.valid? do
          changeset
        else
          do_validation(changeset, validation, tracer, metadata, actor)
        end
      end
    else
      if changeset.action.type == :create do
        Ash.Changeset.add_error(
          changeset,
          Ash.Error.Framework.CanNotBeAtomic.exception(
            resource: changeset.resource,
            change: validation.module,
            reason: "Create actions cannot be made atomic"
          )
        )
      else
        context = %{
          actor: changeset.context[:private][:actor],
          tenant: changeset.tenant,
          authorize?: changeset.context[:private][:authorize?] || false,
          tracer: changeset.context[:private][:tracer]
        }

        case run_atomic_validation(changeset, validation, context) do
          {:not_atomic, reason} ->
            Ash.Changeset.add_error(
              changeset,
              "Validation #{validation.module} must be run atomically, but it could not be: #{reason}"
            )

          changeset ->
            changeset
        end
      end
    end
  end

  defp do_validation(changeset, validation, tracer, metadata, actor) do
    context = %{
      actor: changeset.context[:private][:actor],
      tenant: changeset.tenant,
      authorize?: changeset.context[:private][:authorize?] || false,
      tracer: changeset.context[:private][:tracer]
    }

    if Enum.all?(validation.where || [], fn {module, opts} ->
         opts =
           Ash.Expr.fill_template(
             opts,
             actor,
             changeset.arguments,
             changeset.context
           )

         case module.init(opts) do
           {:ok, opts} ->
             module.validate(changeset, opts, struct(Ash.Resource.Validation.Context, context)) ==
               :ok

           _ ->
             false
         end
       end) do
      Ash.Tracer.span :validation, fn -> "validate: #{inspect(validation.module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :validation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            validation: inspect(validation.module)
          }
        end do
          Ash.Tracer.set_metadata(tracer, :validation, metadata)

          opts =
            Ash.Expr.fill_template(
              validation.opts,
              actor,
              changeset.arguments,
              changeset.context
            )

          with {:ok, opts} <- validation.module.init(opts),
               :ok <-
                 validation.module.validate(
                   changeset,
                   opts,
                   struct(
                     Ash.Resource.Validation.Context,
                     Map.put(context, :message, validation.message)
                   )
                 ) do
            changeset
          else
            :ok ->
              changeset

            {:error, error} when is_binary(error) ->
              Ash.Changeset.add_error(changeset, validation.message || error)

            {:error, error} when is_exception(error) ->
              if validation.message do
                error = override_validation_message(error, validation.message)
                Ash.Changeset.add_error(changeset, error)
              else
                Ash.Changeset.add_error(changeset, error)
              end

            {:error, errors} when is_list(errors) ->
              if validation.message do
                errors =
                  Enum.map(errors, fn error ->
                    override_validation_message(error, validation.message)
                  end)

                Ash.Changeset.add_error(changeset, errors)
              else
                Ash.Changeset.add_error(changeset, errors)
              end

            {:error, error} ->
              error =
                if Keyword.keyword?(error) do
                  Keyword.put(error, :message, validation.message || error[:message])
                else
                  validation.message || error
                end

              Ash.Changeset.add_error(changeset, error)
          end
        end
      end
    else
      changeset
    end
  end

  @doc false
  def override_validation_message(error, message) do
    case error do
      %{field: field} = error when not is_nil(field) ->
        error
        |> Map.take([:field, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
        |> Keyword.put(:value, Map.get(error, :value))
        |> InvalidAttribute.exception()

      %{fields: fields} when fields not in [nil, []] ->
        error
        |> Map.take([:fields, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
        |> Keyword.put(:value, Map.get(error, :value))
        |> InvalidChanges.exception()

      _ ->
        message
    end
  end

  @doc false
  def require_values(changeset, action_type, private_and_belongs_to? \\ false, attrs \\ nil)

  def require_values(changeset, :create, private_and_belongs_to?, attrs) do
    attributes =
      attrs ||
        attributes_to_require(changeset.resource, changeset.action, private_and_belongs_to?)

    attributes
    |> Enum.map(fn
      attribute when is_struct(attribute, Ash.Resource.Attribute) ->
        attribute

      name when is_atom(name) ->
        Ash.Resource.Info.attribute(changeset.resource, name)
    end)
    |> then(fn attributes ->
      if private_and_belongs_to? do
        attributes
      else
        Enum.reject(attributes, &belongs_to_attr_of_rel_being_managed?(&1.name, changeset))
      end
    end)
    |> Enum.reduce(changeset, fn required_attribute, changeset ->
      if changing_attribute?(changeset, required_attribute.name) do
        if is_nil(get_attribute(changeset, required_attribute.name)) do
          if required_attribute.name in changeset.invalid_keys do
            changeset
          else
            add_required_attribute_error(changeset, required_attribute)
          end
        else
          changeset
        end
      else
        if is_nil(required_attribute.default) ||
             required_attribute.name in changeset.action.require_attributes do
          if required_attribute.name in changeset.invalid_keys do
            changeset
          else
            add_required_attribute_error(changeset, required_attribute)
          end
        else
          changeset
        end
      end
    end)
  end

  def require_values(changeset, :update, private_and_belongs_to?, attrs) do
    attributes =
      attrs ||
        attributes_to_require(changeset.resource, changeset.action, private_and_belongs_to?)

    attributes
    |> Enum.map(fn
      attribute when is_struct(attribute, Ash.Resource.Attribute) ->
        attribute

      name when is_atom(name) ->
        Ash.Resource.Info.attribute(changeset.resource, name)
    end)
    |> then(fn attributes ->
      if private_and_belongs_to? do
        attributes
      else
        Enum.reject(attributes, &belongs_to_attr_of_rel_being_managed?(&1.name, changeset))
      end
    end)
    |> Enum.reduce(changeset, fn required_attribute, changeset ->
      setting? =
        Map.has_key?(changeset.attributes, required_attribute.name) ||
          Keyword.has_key?(changeset.atomics, required_attribute.name) ||
          Map.has_key?(changeset.casted_attributes, required_attribute.name)

      if setting? do
        if is_nil(get_attribute(changeset, required_attribute.name)) do
          if required_attribute.name in changeset.invalid_keys do
            changeset
          else
            add_required_attribute_error(changeset, required_attribute)
          end
        else
          changeset
        end
      else
        changeset
      end
    end)
  end

  def require_values(changeset, _, _, _), do: changeset

  defp add_required_attribute_error(changeset, required_attribute) do
    changeset.resource
    |> Ash.Resource.Info.relationships()
    |> Enum.find(&(&1.type == :belongs_to && &1.source_attribute == required_attribute.name))
    |> case do
      nil ->
        add_error(
          changeset,
          Required.exception(
            resource: changeset.resource,
            field: required_attribute.name,
            type: :attribute
          )
        )

      %{name: name} ->
        if required_attribute.name in changeset.action.accept do
          add_error(
            changeset,
            Required.exception(
              resource: changeset.resource,
              field: required_attribute.name,
              type: :attribute
            )
          )
        else
          add_error(
            changeset,
            Required.exception(
              resource: changeset.resource,
              field: name,
              type: :relationship
            )
          )
        end
    end
  end

  defp is_belongs_to_rel_being_managed?(attribute, changeset, only_if_relating?) do
    Enum.any?(changeset.relationships, fn
      {key, [{rels, _}]} ->
        relationship = Ash.Resource.Info.relationship(changeset.resource, key)

        relationship.type == :belongs_to && relationship.name == attribute &&
          (not only_if_relating? || rels != [])

      {_key, list} when is_list(list) ->
        false
    end)
  end

  defp belongs_to_attr_of_rel_being_managed?(attribute, changeset, only_if_relating? \\ false) do
    do_belongs_to_attr_of_rel_being_managed?(changeset, attribute, only_if_relating?) ||
      belongs_to_attr_of_being_managed_through?(changeset, attribute, only_if_relating?)
  end

  defp do_belongs_to_attr_of_rel_being_managed?(changeset, attribute, only_if_relating?) do
    Enum.any?(changeset.relationships, fn
      {key, [{rels, _}]} ->
        relationship = Ash.Resource.Info.relationship(changeset.resource, key)

        relationship.type == :belongs_to && relationship.source_attribute == attribute &&
          (not only_if_relating? || rels != [])

      {_key, list} when is_list(list) ->
        false
    end)
  end

  defp belongs_to_attr_of_being_managed_through?(
         %{context: %{accessing_from: %{unrelating?: true}}},
         _attribute,
         true
       ) do
    false
  end

  defp belongs_to_attr_of_being_managed_through?(
         %{context: %{accessing_from: %{source: source, name: relationship}}},
         attribute,
         _
       ) do
    case Ash.Resource.Info.relationship(source, relationship) do
      %{type: :belongs_to} -> false
      relationship -> relationship.destination_attribute == attribute
    end
  end

  defp belongs_to_attr_of_being_managed_through?(_, _, _), do: false

  # Attributes that are private and/or are the source field of a belongs_to relationship
  # are typically not set by input, so they aren't required until the actual action
  # is run.
  defp attributes_to_require(resource, _action, true = _final?) do
    Ash.Resource.Info.attributes_to_require(resource)
  end

  defp attributes_to_require(resource, action, false = _final?) do
    Ash.Resource.Info.attributes_to_require(resource, action.name)
  end

  @doc """
  Wraps a function in the before/after action hooks of a changeset.

  The function takes a changeset and if it returns
  `{:ok, result}`, the result will be passed through the after
  action hooks.
  """
  @spec with_hooks(
          t(),
          (t() ->
             {:ok, term, %{notifications: list(Ash.Notifier.Notification.t())}}
             | {:error, term}),
          Keyword.t()
        ) ::
          {:ok, term, t(), %{notifications: list(Ash.Notifier.Notification.t())}} | {:error, term}
  def with_hooks(changeset, func, opts \\ [])

  def with_hooks(changeset, _func, _opts) when changeset.valid? == false do
    {:error, changeset.errors}
  end

  def with_hooks(changeset, func, opts) do
    if opts[:transaction?] && Ash.DataLayer.data_layer_can?(changeset.resource, :transact) do
      transaction_hooks(changeset, fn changeset ->
        resources =
          changeset.resource
          |> List.wrap()
          |> Enum.concat(changeset.action.touches_resources)
          |> Enum.uniq()

        notify? =
          if Process.get(:ash_started_transaction?) do
            false
          else
            Process.put(:ash_started_transaction?, true)
            true
          end

        resources = Enum.reject(resources, &Ash.DataLayer.in_transaction?/1)

        try do
          resources
          |> Ash.DataLayer.transaction(
            fn ->
              case run_around_actions(changeset, func) do
                {:error, error} ->
                  if opts[:rollback_on_error?] do
                    Ash.DataLayer.rollback(
                      changeset.resource,
                      error
                    )
                  else
                    {:error, error}
                  end

                other ->
                  other
              end
            end,
            changeset.timeout || :infinity,
            Map.put(
              opts[:transaction_metadata],
              :data_layer_context,
              changeset.context[:data_layer] || %{}
            )
          )
          |> case do
            {:ok, {:ok, value, changeset, instructions}} ->
              {:ok, value, changeset, Map.put(instructions, :gather_notifications?, notify?)}

            {:ok, {:error, error}} ->
              {:error, error}

            {:error, error} ->
              {:error, error}
          end
        after
          if notify? do
            Process.delete(:ash_started_transaction?)
          end
        end
      end)
    else
      if changeset.timeout do
        Ash.ProcessHelpers.task_with_timeout(
          fn ->
            transaction_hooks(changeset, fn changeset ->
              run_around_actions(changeset, func)
            end)
          end,
          changeset.resource,
          changeset.timeout,
          fn -> "#{inspect(changeset.resource)}.#{changeset.action.name}" end,
          opts[:tracer]
        )
      else
        transaction_hooks(changeset, fn changeset ->
          run_around_actions(changeset, func)
        end)
      end
    end
    |> case do
      {:ok, value, changeset, instructions} ->
        if opts[:return_notifications?] do
          {:ok, value, changeset, instructions}
        else
          if Process.get(:ash_started_transaction?) do
            current_notifications = List.wrap(Process.get(:ash_notifications, []))

            Process.put(
              :ash_notifications,
              current_notifications ++ List.wrap(instructions[:notifications])
            )

            {:ok, value, changeset, Map.put(instructions, :notifications, [])}
          else
            notifications =
              instructions[:notifications] || []

            notifications =
              if instructions[:gather_notifications?] do
                Enum.concat(List.wrap(Process.delete(:ash_notifications) || []), notifications)
              else
                notifications
              end

            {:ok, value, changeset,
             Map.put(instructions, :notifications, Ash.Notifier.notify(notifications))}
          end
        end

      other ->
        other
    end
  end

  defp warn_on_transaction_hooks(_, [], _), do: :ok

  defp warn_on_transaction_hooks(changeset, _, type) do
    if Application.get_env(:ash, :warn_on_transaction_hooks?) != false &&
         changeset.context[:warn_on_transaction_hooks?] != false &&
         Ash.DataLayer.in_transaction?(changeset.resource) &&
         (changeset.before_transaction != [] or changeset.around_transaction != []) do
      message =
        if type in ["before_transaction", "around_transaction"] do
          "already"
        else
          "still"
        end

      Logger.warning("""
      One or more `#{type}` hooks on `#{inspect(changeset.resource)}.#{changeset.action.name}` are being executed,
      but there is an ongoing transaction #{message} happening.

      This means that you may be running an action in a transaction that you did not design with the intent of running in a surrounding transaction.
      You should either

      1. If you are testing, and your data layer runs in a transaction/sandbox mode, set `config :ash, warn_on_transaction_hooks?: false` in `config/test.exs`
      2. Create another action that is safe to use in a surrounding transaction, and use that instead of this one
      3. Silence this warning using `set_context(%{warn_on_transaction_hooks?: false})` in the action definition
      4. If building a changeset manually, do #2 except programmatically, `Ash.Changeset.set_context(changeset, %{warn_on_transaction_hooks?: false})`
      """)
    end
  end

  defp transaction_hooks(changeset, func) do
    warn_on_transaction_hooks(changeset, changeset.around_transaction, "around_transaction")

    run_around_transaction_hooks(changeset, fn changeset ->
      warn_on_transaction_hooks(changeset, changeset.before_transaction, "before_transaction")

      changeset_result =
        try do
          {:changeset, run_before_transaction_hooks(changeset)}
        rescue
          exception ->
            {:raise, exception, __STACKTRACE__}
        catch
          :exit, reason ->
            {:exit, reason}
        end

      case changeset_result do
        {:changeset, %{valid?: true} = changeset} ->
          result =
            try do
              func.(clear_phase(changeset))
            rescue
              exception ->
                {:raise, exception, __STACKTRACE__}
            catch
              :exit, reason ->
                {:exit, reason}
            end

          case result do
            {:exit, reason} ->
              error = Ash.Error.to_ash_error(reason)

              case run_after_transactions({:error, error}, changeset) do
                {:ok, result} ->
                  {:ok, result, %{}}

                {:error, new_error} when new_error == error ->
                  exit(reason)

                {:error, new_error} ->
                  exit(new_error)
              end

            {:raise, exception, stacktrace} ->
              case run_after_transactions({:error, exception}, changeset) do
                {:ok, result} ->
                  {:ok, result, changeset, %{}}

                {:error, error} ->
                  reraise error, stacktrace
              end

            {:ok, result, changeset, notifications} ->
              case run_after_transactions({:ok, result}, changeset) do
                {:ok, result} ->
                  {:ok, result, changeset, notifications}

                {:error, error} ->
                  {:error, error}
              end

            {:ok, result, notifications} ->
              case run_after_transactions({:ok, result}, changeset) do
                {:ok, result} ->
                  {:ok, result, changeset, notifications}

                {:error, error} ->
                  {:error, error}
              end

            {:error, error} ->
              case run_after_transactions({:error, error}, changeset) do
                {:ok, result} ->
                  {:ok, result, changeset, %{}}

                {:error, error} ->
                  {:error, error}
              end
          end

        {:changeset, changeset} ->
          case run_after_transactions(
                 {:error, Ash.Error.to_error_class(changeset.errors)},
                 changeset
               ) do
            {:ok, result} ->
              {:ok, result, changeset, %{}}

            {:error, error} ->
              {:error, error}
          end

        {:exit, reason} ->
          error = Ash.Error.to_ash_error(reason)

          case run_after_transactions({:error, error}, changeset) do
            {:ok, result} ->
              {:ok, result, %{}}

            {:error, new_error} when new_error == error ->
              exit(reason)

            {:error, new_error} ->
              exit(new_error)
          end

        {:raise, exception, stacktrace} ->
          case run_after_transactions({:error, exception}, changeset) do
            {:ok, result} ->
              {:ok, result, changeset, %{}}

            {:error, error} ->
              reraise error, stacktrace
          end
      end
    end)
  end

  defp run_around_transaction_hooks(%{around_transaction: []} = changeset, func),
    do: func.(changeset)

  defp run_around_transaction_hooks(%{around_transaction: [around | rest]} = changeset, func) do
    changeset
    |> set_phase(:around_transaction)
    |> around.(fn changeset ->
      run_around_transaction_hooks(%{changeset | around_transaction: rest}, func)
    end)
  end

  def run_before_transaction_hooks(changeset) do
    Enum.reduce_while(
      changeset.before_transaction,
      set_phase(changeset, :before_transaction),
      fn before_transaction, changeset ->
        metadata = fn ->
          %{
            domain: changeset.domain,
            resource: changeset.resource,
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            actor: changeset.context[:private][:actor],
            tenant: changeset.context[:private][:tenant],
            action: changeset.action && changeset.action.name,
            authorize?: changeset.context[:private][:authorize?]
          }
        end

        tracer = changeset.context[:private][:tracer]

        result =
          Ash.Tracer.span :before_transaction,
                          "before_transaction",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :before_transaction, metadata)

            Ash.Tracer.telemetry_span [:ash, :before_transaction], metadata do
              before_transaction.(changeset)
            end
          end

        case result do
          {:error, error} ->
            {:halt, {:error, error}}

          changeset ->
            cont =
              if changeset.valid? do
                :cont
              else
                :halt
              end

            {cont, changeset}
        end
      end
    )
  end

  @doc false
  def run_before_actions(%{before_action: []} = changeset), do: {changeset, %{notifications: []}}

  def run_before_actions(%{valid?: false} = changeset), do: changeset

  def run_before_actions(changeset) do
    can_do_atomic? = data_layer_can_do_atomic_for_changest?(changeset)

    Enum.reduce_while(
      changeset.before_action,
      {changeset, %{notifications: []}},
      fn before_action, {changeset, instructions} ->
        metadata = fn ->
          %{
            domain: changeset.domain,
            resource: changeset.resource,
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            actor: changeset.context[:private][:actor],
            tenant: changeset.context[:private][:actor],
            action: changeset.action && changeset.action.name,
            authorize?: changeset.context[:private][:authorize?]
          }
        end

        tracer = changeset.context[:private][:tracer]

        result =
          Ash.Tracer.span :before_action,
                          "before_action",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :before_action, metadata)

            Ash.Tracer.telemetry_span [:ash, :before_action], metadata do
              before_action.(changeset)
            end
          end

        case result do
          {:error, error} ->
            {:halt, {:error, error}}

          {changeset, %{notifications: notifications}} ->
            cont =
              if changeset.valid? do
                :cont
              else
                :halt
              end

            {cont,
             {changeset,
              %{
                instructions
                | notifications: List.wrap(instructions.notifications) ++ List.wrap(notifications)
              }}}

          changeset ->
            cont =
              if changeset.valid? do
                :cont
              else
                :halt
              end

            {cont, {changeset, instructions}}
        end
      end
    )
    |> case do
      {:error, error} ->
        {:error, error}

      {%{atomics: atomics} = changeset, _} when atomics != [] and not can_do_atomic? ->
        Ash.Changeset.add_error(
          changeset,
          Ash.Error.Invalid.AtomicsNotSupported.exception(
            resource: changeset.resource,
            action_type: changeset.action_type
          )
        )

      {%{valid?: true} = changeset, instructions} ->
        {Ash.Changeset.hydrate_atomic_refs(changeset, changeset.context[:private][:actor]),
         instructions}

      {changeset, instructions} ->
        {changeset, instructions}
    end
  end

  @doc false
  def run_after_transactions(result, changeset) do
    warn_on_transaction_hooks(changeset, changeset.after_transaction, "after_transaction")

    changeset = set_phase(changeset, :after_transaction)

    changeset.after_transaction
    |> Enum.reduce(
      result,
      fn after_transaction, result ->
        tracer = changeset.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: changeset.domain,
            resource: changeset.resource,
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            actor: changeset.context[:private][:actor],
            tenant: changeset.context[:private][:actor],
            action: changeset.action && changeset.action.name,
            authorize?: changeset.context[:private][:authorize?]
          }
        end

        Ash.Tracer.span :after_transaction,
                        "after_transaction",
                        tracer do
          Ash.Tracer.set_metadata(tracer, :after_transaction, metadata)

          Ash.Tracer.telemetry_span [:ash, :after_transaction], metadata do
            after_transaction.(changeset, result)
          end
        end
      end
    )
    |> case do
      {:ok, new_result} ->
        {:ok, new_result}

      {:error, error} ->
        {:error, error}
    end
  end

  defp run_around_actions(%{around_action: []} = changeset, func) do
    changeset =
      changeset
      |> put_context(:private, %{in_before_action?: true})
      |> set_phase(:before_action)

    result =
      if changeset.atomics != [] && !data_layer_can_do_atomic_for_changest?(changeset) do
        {:error,
         Ash.Error.Invalid.AtomicsNotSupported.exception(
           resource: changeset.resource,
           action_type: changeset.action_type
         )}
      else
        run_before_actions(changeset)
      end

    case result do
      {:error, error} ->
        {:error, error}

      {changeset, %{notifications: before_action_notifications}} ->
        changed? =
          Ash.Changeset.changing_attributes?(changeset) or
            not Enum.empty?(changeset.atomics)

        changeset =
          Ash.Changeset.put_context(changeset, :changed?, changed?)

        changeset
        |> clear_phase()
        |> func.()
        |> case do
          {:ok, result, instructions} ->
            run_after_actions(
              result,
              changeset,
              List.wrap(instructions[:notifications]) ++ List.wrap(before_action_notifications)
            )

          {:ok, result} ->
            run_after_actions(result, changeset, before_action_notifications)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp run_around_actions(
         %{around_action: [around | rest]} = changeset,
         func
       ) do
    changeset
    |> set_phase(:around_action)
    |> around.(fn changeset ->
      run_around_actions(%{changeset | around_action: rest}, func)
    end)
  end

  @doc false
  def run_after_actions(result, changeset, before_action_notifications) do
    changeset = set_phase(changeset, :after_action)

    Enum.reduce_while(
      changeset.after_action,
      {:ok, result, changeset, %{notifications: before_action_notifications}},
      fn after_action, {:ok, result, changeset, %{notifications: notifications} = acc} ->
        tracer = changeset.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: changeset.domain,
            resource: changeset.resource,
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            actor: changeset.context[:private][:actor],
            tenant: changeset.context[:private][:actor],
            action: changeset.action && changeset.action.name,
            authorize?: changeset.context[:private][:authorize?]
          }
        end

        result =
          Ash.Tracer.span :after_action,
                          "after_action",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :after_action, metadata)

            Ash.Tracer.telemetry_span [:ash, :after_action], metadata do
              after_action.(changeset, result)
            end
          end

        case result do
          {:ok, new_result, new_notifications} ->
            all_notifications =
              Enum.map(
                List.wrap(notifications) ++ List.wrap(new_notifications),
                fn notification ->
                  %{
                    notification
                    | resource: notification.resource || changeset.resource,
                      action:
                        notification.action ||
                          Ash.Resource.Info.action(
                            changeset.resource,
                            changeset.action,
                            changeset.action_type
                          ),
                      data: notification.data || new_result,
                      changeset: notification.changeset || changeset,
                      actor: notification.actor || changeset.context[:private][:actor]
                  }
                end
              )

            {:cont,
             {:ok, new_result, clear_phase(changeset), %{acc | notifications: all_notifications}}}

          {:ok, new_result} ->
            {:cont, {:ok, new_result, clear_phase(changeset), acc}}

          {:error, error} ->
            {:halt, {:error, error}}

          other ->
            raise """
            Invalid return value from after_action hook. Expected one of:

            * {:ok, result}
            * {:ok, result, notifications}
            * {:error, error}

            Got:

            #{inspect(other)}
            """
        end
      end
    )
  end

  defp data_layer_can_do_atomic_for_changest?(changeset) do
    ability = if changeset.action_type == :update, do: :update, else: :upsert
    Ash.DataLayer.data_layer_can?(changeset.resource, {:atomic, ability})
  end

  @doc "Gets the value of an argument provided to the changeset."
  @spec get_argument(t, atom) :: term
  def get_argument(changeset, argument) when is_atom(argument) do
    if Map.has_key?(changeset.arguments, argument) do
      Map.get(changeset.arguments, argument)
    else
      Map.get(changeset.arguments, to_string(argument))
    end
  end

  def get_argument(changeset, argument) when is_binary(argument) do
    changeset.arguments
    |> Enum.find(fn {key, _} ->
      to_string(key) == argument
    end)
    |> case do
      {_key, value} ->
        value

      _ ->
        nil
    end
  end

  @doc "Fetches the value of an argument provided to the changeset or `:error`."
  @spec fetch_argument(t, atom) :: {:ok, term} | :error
  def fetch_argument(changeset, argument) when is_atom(argument) do
    case Map.fetch(changeset.arguments, argument) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case Map.fetch(changeset.arguments, to_string(argument)) do
          {:ok, value} -> {:ok, value}
          :error -> :error
        end
    end
  end

  def fetch_argument(changeset, argument) when is_binary(argument) do
    changeset.arguments
    |> Enum.find(fn {key, _} ->
      to_string(key) == argument
    end)
    |> case do
      {_key, value} ->
        {:ok, value}

      _ ->
        :error
    end
  end

  @doc "Gets the changing value or the original value of an attribute."
  @spec get_attribute(t, atom) :: term
  def get_attribute(changeset, attribute) do
    case fetch_change(changeset, attribute) do
      {:ok, value} ->
        value

      :error ->
        get_data(changeset, attribute)
    end
  end

  @doc "Gets the value of an argument provided to the changeset, falling back to `Ash.Changeset.get_attribute/2` if nothing was provided."
  @spec get_argument_or_attribute(t, atom) :: term
  def get_argument_or_attribute(changeset, attribute) do
    case fetch_argument(changeset, attribute) do
      {:ok, value} -> value
      :error -> get_attribute(changeset, attribute)
    end
  end

  @doc "Gets the new value for an attribute, or `:error` if it is not being changed."
  @spec fetch_change(t, atom) :: {:ok, any} | :error
  def fetch_change(changeset, attribute) do
    Map.fetch(changeset.attributes, attribute)
  end

  @doc "Gets the value of an argument provided to the changeset, falling back to `Ash.Changeset.fetch_change/2` if nothing was provided."
  @spec fetch_argument_or_change(t, atom) :: {:ok, any} | :error
  def fetch_argument_or_change(changeset, attribute) do
    case fetch_argument(changeset, attribute) do
      {:ok, value} -> {:ok, value}
      :error -> fetch_change(changeset, attribute)
    end
  end

  @doc "Gets the original value for an attribute"
  @spec get_data(t, atom) :: term
  def get_data(changeset, attribute) do
    Map.get(changeset.data, attribute)
  end

  @doc """
  Puts a key/value in the changeset context that can be used later.

  Do not use the `private` key in your custom context, as that is reserved for internal use.
  """
  @spec put_context(t(), atom, term) :: t()
  def put_context(changeset, key, value) do
    set_context(changeset, %{key => value})
  end

  @spec set_tenant(t(), Ash.ToTenant.t()) :: t()
  def set_tenant(changeset, tenant) do
    %{changeset | tenant: tenant, to_tenant: Ash.ToTenant.to_tenant(tenant, changeset.resource)}
  end

  @spec timeout(t(), nil | pos_integer, nil | pos_integer) :: t()
  def timeout(changeset, timeout, default \\ nil) do
    %{changeset | timeout: timeout || default}
  end

  @doc """
  Deep merges the provided map into the changeset context that can be used later.

  Do not use the `private` key in your custom context, as that is reserved for internal use.
  """
  @spec set_context(t(), map | nil) :: t()
  def set_context(changeset, nil), do: changeset

  def set_context(changeset, map) do
    %{changeset | context: Ash.Helpers.deep_merge_maps(changeset.context, map)}
    |> store_context_changes(map)
  end

  defp store_context_changes(%{phase: :pending} = changeset, map) do
    %{changeset | context_changes: Ash.Helpers.deep_merge_maps(changeset.context_changes, map)}
  end

  defp store_context_changes(changeset, _), do: changeset

  @type manage_relationship_type ::
          :append_and_remove | :append | :remove | :direct_control | :create

  @spec manage_relationship_opts(manage_relationship_type()) :: Keyword.t()
  def manage_relationship_opts(:append_and_remove) do
    [
      on_lookup: :relate,
      on_no_match: :error,
      on_match: :ignore,
      on_missing: :unrelate
    ]
  end

  def manage_relationship_opts(:append) do
    [
      on_lookup: :relate,
      on_no_match: :error,
      on_match: :ignore,
      on_missing: :ignore
    ]
  end

  def manage_relationship_opts(:remove) do
    [
      on_no_match: :error,
      on_match: :unrelate,
      on_missing: :ignore
    ]
  end

  def manage_relationship_opts(:create) do
    [
      on_no_match: :create,
      on_match: :ignore
    ]
  end

  def manage_relationship_opts(:direct_control) do
    [
      on_lookup: :ignore,
      on_no_match: :create,
      on_match: :update,
      on_missing: :destroy
    ]
  end

  @manage_opts [
    type: [
      type: {:one_of, @manage_types},
      doc: """
      If the `type` is specified, the default values of each option is modified to match that `type` of operation.

      This allows for specifying certain operations much more succinctly. The defaults that are modified are listed below:

      - `:append_and_remove`

            [
              on_lookup: :relate,
              on_no_match: :error,
              on_match: :ignore,
              on_missing: :unrelate
            ]

      - `:append`

            [
              on_lookup: :relate,
              on_no_match: :error,
              on_match: :ignore,
              on_missing: :ignore
            ]

      - `:remove`

            [
              on_no_match: :error,
              on_match: :unrelate,
              on_missing: :ignore
            ]

      - `:direct_control`

            [
              on_lookup: :ignore,
              on_no_match: :create,
              on_match: :update,
              on_missing: :destroy
            ]

      - `:create`

            [
              on_no_match: :create,
              on_match: :ignore
            ]
      """
    ],
    authorize?: [
      type: :boolean,
      default: true,
      doc:
        "Authorize reads and changes to the destination records, if the primary change is being authorized as well."
    ],
    eager_validate_with: [
      type: :atom,
      default: false,
      doc:
        "Validates that any referenced entities exist *before* the action is being performed, using the provided domain for the read."
    ],
    on_no_match: [
      type: :any,
      default: :ignore,
      doc: """
      Instructions for handling records where no matching record existed in the relationship.

      * `:ignore` (default) - those inputs are ignored
      * `:match` - For `has_one` and `belongs_to` only, any input is treated as a match for an existing value. For `has_many` and `many_to_many`, this is the same as `:ignore`.
      * `:create` - the records are created using the destination's primary create action
      * `{:create, :action_name}` - the records are created using the specified action on the destination resource
      * `{:create, :action_name, :join_table_action_name, [:list, :of, :join_table, :params]}` - Same as `{:create, :action_name}` but takes
          the list of params specified out and applies them when creating the join record, with the provided join_table_action_name.
      * `:error`  - an error is returned indicating that a record would have been created
        *  If `on_lookup` is set, and the data contained a primary key or identity, then the error is a `NotFound` error
        * Otherwise, an `InvalidRelationship` error is returned
      """
    ],
    value_is_key: [
      type: :atom,
      doc: """
      Configures what key to use when a single value is provided.

      This is useful when you use things like a list of strings i.e `friend_emails` to manage the relationship, instead of a list of maps.

      By default, we assume it is the primary key of the destination resource, unless it is a composite primary key.
      """
    ],
    identity_priority: [
      type: {:list, :atom},
      doc: """
      The list, in priority order, of identities to use when looking up records for `on_lookup`, and matching records with `on_match`.

      Use `:_primary_key` to prioritize checking a match with the primary key.
      All identities, along with `:_primary_key` are checked regardless, this only allows ensuring that some are checked first.
      Defaults to the list provided by `use_identities`, so you typically won't need this option.
      """
    ],
    use_identities: [
      type: {:list, :atom},
      doc: """
      A list of identities that may be used to look up and compare records. Use `:_primary_key` to include the primary key. By default, only `[:_primary_key]` is used.
      """
    ],
    on_lookup: [
      type: :any,
      default: :ignore,
      doc: """
      Before creating a record (because no match was found in the relationship), the record can be looked up and related.

      * `:ignore` (default) - Does not look for existing entries (matches in the current relationship are still considered updates)
      * `:relate` - Same as calling `{:relate, primary_action_name}`
      * `{:relate, :action_name}` - the records are looked up by primary key/the first identity that is found (using the primary read action), and related. The action should be:
          * `many_to_many` - a create action on the join resource
          * `has_many` - an update action on the destination resource
          * `has_one` - an update action on the destination resource
          * `belongs_to` - an update action on the source resource
      * `{:relate, :action_name, :read_action_name}` - Same as the above, but customizes the read action called to search for matches.
      * `:relate_and_update` - Same as `:relate`, but the remaining parameters from the lookup are passed into the action that is used to change the relationship key
      * `{:relate_and_update, :action_name}` - Same as the above, but customizes the action used. The action should be:
          * `many_to_many` - a create action on the join resource
          * `has_many` - an update action on the destination resource
          * `has_one` - an update action on the destination resource
          * `belongs_to` - an update action on the source resource
      * `{:relate_and_update, :action_name, :read_action_name}` - Same as the above, but customizes the read action called to search for matches.
      * `{:relate_and_update, :action_name, :read_action_name, [:list, :of, :join_table, :params]}` - Same as the above, but uses the provided list of parameters when creating
          the join row (only relevant for many to many relationships). Use `:*` to *only* update the join record, and pass all parameters to its action
      """
    ],
    on_match: [
      type: :any,
      default: :ignore,
      doc: """
      Instructions for handling records where a matching record existed in the relationship already.

      * `:ignore` (default) - those inputs are ignored
      * `:update` - the record is updated using the destination's primary update action
      * `{:update, :action_name}` - the record is updated using the specified action on the destination resource
      * `{:update, :action_name, :join_table_action_name, [:list, :of, :params]}` - Same as `{:update, :action_name}` but takes
          the list of params specified out and applies them as an update to the join record (only valid for many to many)
      * `:update_join` - update only the join record (only valid for many to many)
      * `{:update_join, :join_table_action_name}` - use the specified update action on a join resource
      * `{:update_join, :join_table_action_name, [:list, :of, :params]}` - pass specified params from input into a join resource update action
      * `{:destroy, :action_name}` - the record is destroyed using the specified action on the destination resource. The action should be:
        * `many_to_many` - a destroy action on the join record
        * `has_many` - a destroy action on the destination resource
        * `has_one` - a destroy action on the destination resource
        * `belongs_to` - a destroy action on the destination resource
      * `:error`  - an error is returned indicating that a record would have been updated
      * `:no_match` - follows the `on_no_match` instructions with these records
      * `:missing` - follows the `on_missing` instructions with these records
      * `:unrelate` - the related item is not destroyed, but the data is "unrelated", making this behave like `remove_from_relationship/3`. The action should be:
        * `many_to_many` - the join resource row is destroyed
        * `has_many` - the `destination_attribute` (on the related record) is set to `nil`
        * `has_one` - the `destination_attribute` (on the related record) is set to `nil`
        * `belongs_to` - the `source_attribute` (on this record) is set to `nil`
      * `{:unrelate, :action_name}` - the record is unrelated using the provided update action. The action should be:
        * `many_to_many` - a destroy action on the join resource
        * `has_many` - an update action on the destination resource
        * `has_one` - an update action on the destination resource
        * `belongs_to` - an update action on the source resource
      """
    ],
    on_missing: [
      type: :any,
      default: :ignore,
      doc: """
      Instructions for handling records that existed in the current relationship but not in the input.

      * `:ignore` (default) - those inputs are ignored
      * `:destroy` - the record is destroyed using the destination's primary destroy action
      * `{:destroy, :action_name}` - the record is destroyed using the specified action on the destination resource
      * `{:destroy, :action_name, :join_resource_action_name}` - the record is destroyed using the specified action on the destination resource,
        but first the join resource is destroyed with its specified action
      * `:error`  - an error is returned indicating that a record would have been updated
      * `:unrelate` - the related item is not destroyed, but the data is "unrelated", making this behave like `remove_from_relationship/3`. The action should be:
        * `many_to_many` - the join resource row is destroyed
        * `has_many` - the `destination_attribute` (on the related record) is set to `nil`
        * `has_one` - the `destination_attribute` (on the related record) is set to `nil`
        * `belongs_to` - the `source_attribute` (on this record) is set to `nil`
      * `{:unrelate, :action_name}` - the record is unrelated using the provided update action. The action should be:
        * `many_to_many` - a destroy action on the join resource
        * `has_many` - an update action on the destination resource
        * `has_one` - an update action on the destination resource
        * `belongs_to` - an update action on the source resource
      """
    ],
    error_path: [
      type: :any,
      doc: """
      By default, errors added to the changeset will use the path `[:relationship_name]`, or `[:relationship_name, <index>]`.
      If you want to modify this path, you can specify `error_path`, e.g if had a `change` on an action that takes an argument
      and uses that argument data to call `manage_relationship`, you may want any generated errors to appear under the name of that
      argument, so you could specify `error_path: :argument_name` when calling `manage_relationship`.
      """
    ],
    join_keys: [
      type: {:list, :atom},
      doc: """
      For many to many relationships specifies the parameters to pick from the input and pass into a join resource action.
      Applicable in cases like `on_no_match: :create`, `on_match: :update` and `on_lookup: :relate`.
      Can be overwritten by a full form instruction tuple which contains join parameters at the end.
      """
    ],
    meta: [
      type: :any,
      doc: """
      Freeform data that will be retained along with the options, which can be used to track/manage the changes
      that are added to the `relationships` key. Use the `meta[:order]` option to specify the order in which multiple
      calls to `manage_relationship` should be executed.
      """
    ],
    ignore?: [
      type: :any,
      default: false,
      doc: """
      This tells Ash to ignore the provided inputs when actually running the action. This can be useful for
      building up a set of instructions that you intend to handle manually.
      """
    ]
  ]

  @doc false
  def manage_relationship_schema, do: @manage_opts

  manage_opts = @manage_opts

  defmodule ManageRelationshipOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: manage_opts
  end

  @doc """
  Manages the related records by creating, updating, or destroying them as necessary.

  Keep in mind that the default values for all `on_*` are `:ignore`, meaning nothing will happen
  unless you provide instructions.

  The input provided to `manage_relationship` should be a map, in the case of to_one relationships, or a list of maps
  in the case of to_many relationships. The following steps are followed for each input provided:

  - The input is checked against the currently related records to find any matches. The primary key and unique identities are used to find matches.
  - For any input that had a match in the current relationship, the `:on_match` behavior is triggered
  - For any input that does not have a match:
    - if there is `on_lookup` behavior:
      - we try to find the record in the data layer.
      - if the record is found, the on_lookup behavior is triggered
      - if the record is not found, the `on_no_match` behavior is triggered
    - if there is no `on_lookup` behavior:
      - the `on_no_match` behavior is triggered
  - finally, for any records present in the *current relationship* that had no match *in the input*, the `on_missing` behavior is triggered

  ## Options

  #{Spark.Options.docs(@manage_opts)}

  Each call to this function adds new records that will be handled according to their options. For example,
  if you tracked "tags to add" and "tags to remove" in separate fields, you could input them like so:

  ```elixir
  changeset
  |> Ash.Changeset.manage_relationship(
    :tags,
    [%{name: "backend"}],
    on_lookup: :relate, #relate that tag if it exists in the database
    on_no_match: :error # error if a tag with that name doesn't exist
  )
  |> Ash.Changeset.manage_relationship(
    :tags,
    [%{name: "frontend"}],
    on_no_match: :error, # error if a tag with that name doesn't exist in the relationship
    on_match: :unrelate # if a tag with that name is related, unrelate it
  )
  ```

  When calling this multiple times with the `on_missing` option set, the list of records that are considered missing are checked
  after each set of inputs is processed. For example, if you manage the relationship once with `on_missing: :unrelate`, the records
  missing from your input will be removed, and *then* your next call to `manage_relationship` will be resolved (with those records unrelated).
  For this reason, it is suggested that you don't call this function multiple times with an `on_missing` instruction, as you may be
  surprised by the result.

  If you want the input to update existing entities, you need to ensure that the primary key (or unique identity) is provided as
  part of the input. See the example below:

      changeset
      |> Ash.Changeset.manage_relationship(
        :comments,
        [%{rating: 10, contents: "foo"}],
        on_no_match: {:create, :create_action},
        on_missing: :ignore
      )
      |> Ash.Changeset.manage_relationship(
        :comments,
        [%{id: 10, rating: 10, contents: "foo"}],
        on_match: {:update, :update_action},
        on_no_match: {:create, :create_action})

  This is a simple way to manage a relationship. If you need custom behavior, you can customize the action that is
  called, which allows you to add arguments/changes. However, at some point you may want to forego this function
  and make the changes yourself. For example:

      input = [%{id: 10, rating: 10, contents: "foo"}]

      changeset
      |> Ash.Changeset.after_action(fn _changeset, result ->
        # An example of updating comments based on a result of other changes
        for comment <- input do
          comment = Ash.get(Comment, comment.id)

          comment
          |> Map.update(:rating, 0, &(&1 * result.rating_weight))
          |> Ash.update!()
        end

        {:ok, result}
      end)

  ## Using records as input

  Records can be supplied as the input values. If you do:

  * if it would be looked up due to `on_lookup`, the record is used as-is
  * if it would be created due to `on_no_match`, the record is used as-is
  * Instead of specifying `join_keys`, those keys must go in `__metadata__.join_keys`. If `join_keys` is specified in the options, it is ignored.

  For example:

  ```elixir
  post1 =
    changeset
    |> Ash.create!()
    |> Ash.Resource.put_metadata(:join_keys, %{type: "a"})

  post1 =
    changeset2
    |> Ash.create!()
    |> Ash.Resource.put_metadata(:join_keys, %{type: "b"})

  author = Ash.create!(author_changeset)

  Ash.Changeset.manage_relationship(
    author,
    :posts,
    [post1, post2],
    on_lookup: :relate
  )
  ```
  """
  def manage_relationship(changeset, relationship, input, opts \\ [])

  def manage_relationship(changeset, relationship, "", opts) do
    manage_relationship(changeset, relationship, nil, opts)
  end

  def manage_relationship(changeset, relationship, input, opts) do
    opts =
      if opts[:type] == :replace do
        Logger.warning(
          "`type: :replace` has been renamed to `:append_and_remove` in 2.0, and it will be removed in 2.1"
        )

        Keyword.put(opts, :type, :append_and_remove)
      else
        opts
      end

    inputs_was_list? = is_list(input)

    opts =
      if opts[:type] do
        defaults = manage_relationship_opts(opts[:type])
        Keyword.merge(defaults, opts)
      else
        opts
      end

    opts =
      opts
      |> ManageRelationshipOpts.validate!()

    keyword_opts =
      ManageRelationshipOpts.to_options(opts)

    opts =
      if opts.meta && Keyword.has_key?(opts.meta, :inputs_was_list?) do
        opts
      else
        %{opts | meta: Keyword.put(opts.meta || [], :inputs_was_list?, inputs_was_list?)}
      end

    case Ash.Resource.Info.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            relationship: relationship
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "relationship is not editable"
          )

        add_error(changeset, error)

      %{manual: manual} = relationship when not is_nil(manual) ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "cannot manage a manual relationship"
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship when is_list(input) and length(input) > 1 ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "cannot manage a #{type} relationship with a list of records"
          )

        add_error(changeset, error)

      relationship ->
        key =
          opts.value_is_key ||
            changeset.resource
            |> Ash.Resource.Info.related(relationship.name)
            |> Ash.Resource.Info.primary_key()
            |> case do
              [key] ->
                key

              _ ->
                nil
            end

        if relationship.cardinality == :many && is_map(input) && !is_struct(input) do
          case map_input_to_list(input) do
            {:ok, input} ->
              input =
                if key do
                  Enum.map(input, fn input ->
                    if is_map(input) || is_list(input) do
                      input
                    else
                      %{key => input}
                    end
                  end)
                else
                  input
                end

              manage_relationship(changeset, relationship.name, input, keyword_opts)

            :error ->
              manage_relationship(changeset, relationship.name, List.wrap(input), keyword_opts)
          end
        else
          input =
            if key do
              input
              |> List.wrap()
              |> Enum.map(fn input ->
                if is_map(input) || Keyword.keyword?(input) do
                  input
                else
                  %{key => input}
                end
              end)
            else
              input
            end

          if Enum.any?(
               List.wrap(input),
               &(is_struct(&1) && Ash.Resource.Info.resource?(&1.__struct__) &&
                   &1.__struct__ != relationship.destination)
             ) do
            add_error(
              changeset,
              InvalidRelationship.exception(
                relationship: relationship.name,
                message: "cannot provide structs that don't match the destination"
              )
            )
          else
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, [])
              |> Map.update!(relationship.name, &(&1 ++ [{input, keyword_opts}]))

            changeset = %{changeset | relationships: relationships}

            if opts.eager_validate_with do
              eager_validate_relationship_input(
                relationship,
                input,
                changeset,
                opts.eager_validate_with,
                opts.error_path || opts.meta[:id] || relationship.name,
                keyword_opts
              )
            else
              changeset
            end
          end
        end
    end
  end

  defp eager_validate_relationship_input(
         _relationship,
         [],
         _changeset,
         _domain,
         _error_path,
         _opts
       ),
       do: :ok

  defp eager_validate_relationship_input(relationship, input, changeset, domain, error_path, opts) do
    pkeys = Ash.Actions.ManagedRelationships.pkeys(relationship, opts)

    pkeys =
      Enum.map(pkeys, fn pkey ->
        Enum.map(pkey, fn key ->
          Ash.Resource.Info.attribute(relationship.destination, key)
        end)
      end)

    search =
      Enum.reduce(input, false, fn item, expr ->
        filter =
          Enum.find_value(pkeys, fn pkey ->
            this_filter =
              pkey
              |> Enum.reject(&(&1.name == relationship.destination_attribute))
              |> Enum.all?(fn key ->
                case fetch_identity_field(
                       item,
                       changeset.data,
                       key,
                       relationship
                     ) do
                  {:ok, _value} ->
                    true

                  :error ->
                    false
                end
              end)
              |> case do
                true ->
                  case Map.take(
                         item,
                         Enum.map(pkey, & &1.name) ++ Enum.map(pkey, &to_string(&1.name))
                       ) do
                    empty when empty == %{} -> nil
                    filter -> filter
                  end

                false ->
                  nil
              end

            if Enum.any?(pkey, &(&1.name == relationship.destination_attribute)) &&
                 relationship.type in [:has_many, :has_one] do
              destination_value = Map.get(changeset.data, relationship.source_attribute)

              expr(
                ^this_filter and
                  (is_nil(^ref(relationship.destination_attribute)) or
                     ^ref(relationship.destination_attribute) == ^destination_value)
              )
            else
              this_filter
            end
          end)

        if filter && filter != %{} do
          expr(^expr or ^filter)
        else
          expr
        end
      end)

    results =
      if search == false do
        {:ok, []}
      else
        action =
          relationship.read_action ||
            Ash.Resource.Info.primary_action!(relationship.destination, :read).name

        relationship.destination
        |> Ash.Query.for_read(action, %{},
          actor: changeset.context[:private][:actor],
          authorize?: changeset.context[:private][:authorize?],
          tenant: changeset.tenant
        )
        |> Ash.Query.limit(Enum.count(input))
        |> Ash.Query.do_filter(search)
        |> domain.read()
      end

    case results do
      {:error, error} ->
        {:error, error}

      {:ok, results} ->
        case Enum.find(input, fn item ->
               no_pkey_all_matches(results, pkeys, fn result, key ->
                 case fetch_identity_field(item, changeset.data, key, relationship) do
                   {:ok, value} ->
                     Ash.Type.equal?(
                       key.type,
                       value,
                       Map.get(result, key.name)
                     )

                   :error ->
                     false
                 end
               end)
             end) do
          nil ->
            :ok

          item ->
            pkey_search =
              Enum.find_value(pkeys, fn pkey ->
                if Enum.all?(pkey, fn key ->
                     Map.has_key?(item, key.name) || Map.has_key?(item, to_string(key.name))
                   end) do
                  Map.take(item, pkey ++ Enum.map(pkey, &to_string(&1.name)))
                end
              end)

            {:error,
             Ash.Error.Query.NotFound.exception(
               primary_key: pkey_search,
               resource: relationship.destination
             )}
        end
    end
    |> case do
      :ok ->
        changeset

      {:error, error} ->
        add_error(changeset, Ash.Error.set_path(error, error_path))
    end
  end

  defp no_pkey_all_matches(results, pkeys, func) do
    !Enum.any?(results, fn result ->
      Enum.any?(pkeys, fn pkey ->
        Enum.all?(pkey, fn key ->
          func.(result, key)
        end)
      end)
    end)
  end

  defp fetch_identity_field(item, data, attribute, relationship) do
    if attribute.name == relationship.destination_attribute &&
         relationship.type in [:has_many, :has_one] do
      {:ok, Map.get(data, relationship.source_attribute)}
    else
      string_attribute = to_string(attribute.name)

      if Map.has_key?(item, attribute.name) || Map.has_key?(item, string_attribute) do
        input_value = Map.get(item, attribute.name) || Map.get(item, string_attribute)

        case Ash.Type.cast_input(attribute.type, input_value, attribute.constraints) do
          {:ok, casted_input_value} ->
            {:ok, casted_input_value}

          _ ->
            :error
        end
      else
        :error
      end
    end
  end

  defp map_input_to_list(input) when input == %{} do
    :error
  end

  defp map_input_to_list(input) do
    input
    |> Enum.reduce_while({:ok, []}, fn
      {key, value}, {:ok, acc} when is_integer(key) ->
        {:cont, {:ok, [{key, value} | acc]}}

      {key, value}, {:ok, acc} when is_binary(key) ->
        case Integer.parse(key) do
          {int, ""} ->
            {:cont, {:ok, [{int, value} | acc]}}

          _ ->
            {:halt, :error}
        end

      _, _ ->
        {:halt, :error}
    end)
    |> case do
      {:ok, value} ->
        {:ok,
         value
         |> Enum.sort_by(&elem(&1, 0))
         |> Enum.map(&elem(&1, 1))}

      :error ->
        :error
    end
  end

  @doc "Returns true if any attributes on the resource are being changed."
  @spec changing_attributes?(t()) :: boolean
  def changing_attributes?(changeset) do
    changeset.resource
    |> Ash.Resource.Info.attributes()
    |> Enum.any?(&changing_attribute?(changeset, &1.name))
  end

  @doc "Returns true if an attribute exists in the changes"
  @spec changing_attribute?(t(), atom) :: boolean
  def changing_attribute?(changeset, attribute) do
    Map.has_key?(changeset.attributes, attribute) ||
      Keyword.has_key?(changeset.atomics, attribute)
  end

  @doc "Returns true if a relationship exists in the changes"
  @spec changing_relationship?(t(), atom) :: boolean
  def changing_relationship?(changeset, relationship) do
    Map.has_key?(changeset.relationships, relationship)
  end

  @doc "Change an attribute only if is not currently being changed"
  @spec change_new_attribute(t(), atom, term) :: t()
  def change_new_attribute(changeset, attribute, value) do
    maybe_already_validated_error!(changeset, :force_change_new_attribute)

    if changing_attribute?(changeset, attribute) do
      changeset
    else
      change_attribute(changeset, attribute, value)
    end
  end

  @doc """
  Change an attribute if is not currently being changed, by calling the provided function.

  Use this if you want to only perform some expensive calculation for an attribute value
  only if there isn't already a change for that attribute.
  """
  @spec change_new_attribute_lazy(t(), atom, (-> any)) :: t()
  def change_new_attribute_lazy(changeset, attribute, func) do
    maybe_already_validated_error!(changeset, :force_change_new_attribute_lazy)

    if changing_attribute?(changeset, attribute) do
      changeset
    else
      change_attribute(changeset, attribute, func.())
    end
  end

  @doc """
  Updates an existing attribute change by applying a function to it.

  This is useful for applying some kind of normalization to the attribute.

  ```elixir
  Ash.Changeset.update_change(changeset, :serial, &String.downcase/1)
  ```

  The update function gets called with the value already cast to the correct type.

  ```elixir
  changeset
  |> Ash.Changeset.change_attribute(:integer_attribute, "3")
  |> Ash.Changeset.update_change(:integer_attribute, fn x -> x + 1 end)
  ```

  ## Invalid value handling

  If `update_change` is called with a changeset that has not been validated yet, the update
  function must handle potentially invalid and `nil` values.

  To only deal with valid values, you can call `update_change` in a `before_action` hook.
  """
  @spec update_change(t(), atom, (any -> any)) :: t()
  def update_change(changeset, attribute, fun) do
    case fetch_change(changeset, attribute) do
      {:ok, change} ->
        force_change_attribute(changeset, attribute, fun.(change))

      :error ->
        changeset
    end
  end

  @doc """
  Add an argument to the changeset, which will be provided to the action.
  """
  def set_argument(changeset, argument, value) do
    maybe_already_validated_error!(changeset, :force_set_argument)
    do_set_argument(changeset, argument, value)
  end

  @doc """
  Add an argument to the changeset, which will be provided to the action.

  Does not show a warning when used in before/after action hooks.
  """
  def force_set_argument(changeset, argument, value) do
    do_set_argument(changeset, argument, value)
  end

  defp do_set_argument(changeset, argument, value, store_casted? \\ false) do
    if changeset.action do
      argument =
        Enum.find(
          changeset.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      if argument do
        with value <- Ash.Type.Helpers.handle_indexed_maps(argument.type, value),
             constraints <-
               Ash.Type.include_source(argument.type, changeset, argument.constraints),
             {:ok, casted} <-
               Ash.Type.cast_input(argument.type, value, constraints),
             {:constrained, {:ok, casted}, _last_val} when not is_nil(casted) <-
               {:constrained, Ash.Type.apply_constraints(argument.type, casted, constraints),
                casted} do
          %{changeset | arguments: Map.put(changeset.arguments, argument.name, casted)}
          |> store_casted_argument(argument.name, casted, store_casted?)
        else
          {:constrained, {:ok, nil}, _} ->
            %{changeset | arguments: Map.put(changeset.arguments, argument.name, nil)}
            |> store_casted_argument(argument.name, nil, store_casted?)

          {:constrained, {:error, error}, last_val} ->
            add_invalid_errors(value, :argument, changeset, argument, error)
            |> store_casted_argument(argument.name, last_val, store_casted?)

          {:error, error} ->
            add_invalid_errors(value, :argument, changeset, argument, error)
        end
      else
        %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
        |> store_casted_argument(argument, value, store_casted?)
      end
    else
      %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
      |> store_casted_argument(argument, value, store_casted?)
    end
  end

  defp store_casted_argument(changeset, name, value, true) do
    %{
      changeset
      | casted_arguments: Map.put(changeset.casted_arguments, name, value)
    }
  end

  defp store_casted_argument(changeset, _name, _value, _store_casted?) do
    changeset
  end

  @doc """
  Remove an argument from the changeset
  """
  def delete_argument(changeset, argument_or_arguments) do
    maybe_already_validated_error!(changeset)

    argument_or_arguments
    |> List.wrap()
    |> Enum.reduce(changeset, fn argument, changeset ->
      %{changeset | arguments: Map.delete(changeset.arguments, argument)}
    end)
  end

  @doc """
  Merge a map of arguments to the arguments list.
  """
  def set_arguments(changeset, map) do
    Enum.reduce(map, changeset, fn {key, value}, changeset ->
      set_argument(changeset, key, value)
    end)
  end

  @doc """
  Merge a map of arguments to the arguments list.

  Does not show a warning when used in before/after action hooks.
  """
  def force_set_arguments(changeset, map) do
    Enum.reduce(map, changeset, fn {key, value}, changeset ->
      force_set_argument(changeset, key, value)
    end)
  end

  @doc """
  Force change an attribute if it is not currently being changed.

  See `change_new_attribute/3` for more.
  """
  @spec force_change_new_attribute(t(), atom, term) :: t()
  def force_change_new_attribute(changeset, attribute, value) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      force_change_attribute(changeset, attribute, value)
    end
  end

  @doc """
  Force change an attribute if it is not currently being changed, by calling the provided function.

  See `change_new_attribute_lazy/3` for more.
  """
  @spec force_change_new_attribute_lazy(t(), atom, (-> any)) :: t()
  def force_change_new_attribute_lazy(changeset, attribute, func) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      force_change_attribute(changeset, attribute, func.())
    end
  end

  @doc "Calls `change_attribute/3` for each key/value pair provided."
  @spec change_attributes(t(), map | Keyword.t()) :: t()
  def change_attributes(changeset, changes) do
    maybe_already_validated_error!(changeset, :force_change_attributes)

    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      change_attribute(changeset, key, value)
    end)
  end

  @doc "Adds a change to the changeset, unless the value matches the existing value."
  @spec change_attribute(t(), atom, any) :: t()
  def change_attribute(changeset, attribute, value) do
    maybe_already_validated_error!(changeset, :force_change_attribute)
    do_change_attribute(changeset, attribute, value)
  end

  defp do_change_attribute(changeset, attribute, value, store_casted? \\ false) do
    case Ash.Resource.Info.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            attribute: attribute
          )

        add_error(changeset, error)

      %{writable?: false} = attribute ->
        add_invalid_errors(value, :attribute, changeset, attribute, "Attribute is not writable")

      attribute ->
        with value <- Ash.Type.Helpers.handle_indexed_maps(attribute.type, value),
             constraints <-
               Ash.Type.include_source(attribute.type, changeset, attribute.constraints),
             {{:ok, prepared}, _} <-
               {prepare_change(changeset, attribute, value, constraints), value},
             {{:ok, casted}, _} <-
               {Ash.Type.cast_input(
                  attribute.type,
                  prepared,
                  constraints
                ), prepared},
             {{:ok, casted}, _} <-
               {handle_change(
                  changeset,
                  attribute,
                  casted,
                  constraints
                ), casted},
             {{:ok, casted}, _} <-
               {Ash.Type.apply_constraints(attribute.type, casted, constraints), casted} do
          data_value =
            if changeset.action_type != :create do
              case changeset.data do
                %Ash.Changeset.OriginalDataNotAvailable{} ->
                  nil

                data ->
                  Map.get(data, attribute.name)
              end
            end

          changeset = remove_default(changeset, attribute.name)

          cond do
            changeset.action_type == :create ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }
              |> store_casted_attribute(attribute.name, casted, store_casted?)

            is_nil(data_value) and is_nil(casted) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }
              |> store_casted_attribute(attribute.name, nil, store_casted?)

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }
              |> store_casted_attribute(attribute.name, casted, store_casted?)

            true ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }
              |> store_casted_attribute(attribute.name, casted, store_casted?)
          end
        else
          {{:error, error_or_errors}, last_val} ->
            add_invalid_errors(value, :attribute, changeset, attribute, error_or_errors)
            |> store_casted_attribute(attribute.name, last_val, store_casted?)

          {:error, error_or_errors} ->
            add_invalid_errors(value, :attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  defp store_casted_attribute(changeset, name, value, true) do
    %{changeset | casted_attributes: Map.put(changeset.casted_attributes, name, value)}
  end

  defp store_casted_attribute(changeset, _name, _value, _store_casted?) do
    changeset
  end

  @doc """
  The same as `change_attribute`, but annotates that the attribute is currently holding a default value.

  This information can be used in changes to see if a value was explicitly set or if it was set by being the default.
  Additionally, this is used in `upsert` actions to not overwrite existing values with the default.
  """
  @spec change_default_attribute(t(), atom, any) :: t()
  def change_default_attribute(changeset, attribute, value) do
    maybe_already_validated_error!(changeset)

    case Ash.Resource.Info.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            attribute: attribute
          )

        add_error(changeset, error)

      attribute ->
        changeset
        |> change_attribute(attribute.name, value)
        |> Map.update!(:defaults, fn defaults ->
          Enum.uniq([attribute.name | defaults])
        end)
    end
  end

  @doc "Calls `force_change_attribute/3` for each key/value pair provided."
  @spec force_change_attributes(t(), map | Keyword.t()) :: t()
  def force_change_attributes(changeset, changes) do
    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      force_change_attribute(changeset, key, value)
    end)
  end

  @doc "Changes an attribute even if it isn't writable"
  @spec force_change_attribute(t(), atom, any) :: t()
  def force_change_attribute(changeset, attribute, value) do
    case Ash.Resource.Info.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            attribute: attribute
          )

        add_error(changeset, error)

      attribute when is_nil(value) ->
        changeset = remove_default(changeset, attribute.name)

        %{changeset | attributes: Map.put(changeset.attributes, attribute.name, nil)}
        |> record_attribute_change_for_atomic_upgrade(attribute.name, nil)

      attribute ->
        with value <- Ash.Type.Helpers.handle_indexed_maps(attribute.type, value),
             constraints <-
               Ash.Type.include_source(attribute.type, changeset, attribute.constraints),
             {:ok, prepared} <-
               prepare_change(changeset, attribute, value, constraints),
             {:ok, casted} <-
               Ash.Type.cast_input(attribute.type, prepared, constraints),
             {:ok, casted} <- handle_change(changeset, attribute, casted, constraints),
             {:ok, casted} <-
               Ash.Type.apply_constraints(attribute.type, casted, constraints) do
          data_value =
            if changeset.action_type != :create do
              case changeset.data do
                %Ash.Changeset.OriginalDataNotAvailable{} ->
                  nil

                data ->
                  Map.get(data, attribute.name)
              end
            end

          changeset = remove_default(changeset, attribute.name)

          cond do
            changeset.action_type == :create ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted)
              }
              |> record_attribute_change_for_atomic_upgrade(attribute.name, casted)

            is_nil(data_value) and is_nil(casted) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name)
              }
              |> record_attribute_change_for_atomic_upgrade(attribute.name, casted)

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name)
              }
              |> record_attribute_change_for_atomic_upgrade(attribute.name, casted)

            true ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted)
              }
              |> record_attribute_change_for_atomic_upgrade(attribute.name, casted)
          end
        else
          :error ->
            add_invalid_errors(value, :attribute, changeset, attribute)

          {:error, error_or_errors} ->
            add_invalid_errors(value, :attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  defp record_attribute_change_for_atomic_upgrade(changeset, name, casted) do
    if changeset.phase == :pending do
      %{changeset | attribute_changes: Map.put(changeset.attribute_changes, name, casted)}
    else
      changeset
    end
  end

  defp record_atomic_update_for_atomic_upgrade(changeset, name, value) do
    if changeset.phase == :pending do
      %{changeset | atomic_changes: Keyword.put(changeset.atomic_changes, name, value)}
    else
      changeset
    end
  end

  @doc """
  Adds a before_action hook to the changeset.

  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
  """
  @spec before_action(
          changeset :: t(),
          fun :: before_action_fun(),
          opts :: Keyword.t()
        ) ::
          t()
  def before_action(changeset, func, opts \\ []) do
    changeset = maybe_dirty_hook(changeset, :before_action)

    if opts[:prepend?] do
      %{changeset | before_action: [func | changeset.before_action]}
    else
      %{changeset | before_action: changeset.before_action ++ [func]}
    end
  end

  @doc """
  Adds a before_transaction hook to the changeset.

  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
  """
  @spec before_transaction(
          t(),
          before_transaction_fun(),
          Keyword.t()
        ) :: t()
  def before_transaction(changeset, func, opts \\ []) do
    changeset = maybe_dirty_hook(changeset, :before_transaction)

    if opts[:prepend?] do
      %{changeset | before_transaction: [func | changeset.before_transaction]}
    else
      %{changeset | before_transaction: changeset.before_transaction ++ [func]}
    end
  end

  @doc """
  Adds an after_action hook to the changeset.

  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
  """
  @spec after_action(
          t(),
          after_action_fun(),
          Keyword.t()
        ) :: t()
  def after_action(changeset, func, opts \\ []) do
    changeset = maybe_dirty_hook(changeset, :after_action)

    if opts[:prepend?] do
      if changeset.phase == :pending do
        %{
          changeset
          | after_action: [func | changeset.after_action],
            atomic_after_action: [func | changeset.atomic_after_action]
        }
      else
        %{changeset | after_action: [func | changeset.after_action]}
      end
    else
      if changeset.phase == :pending do
        %{
          changeset
          | after_action: changeset.after_action ++ [func],
            atomic_after_action: changeset.atomic_after_action ++ [func]
        }
      else
        %{changeset | after_action: changeset.after_action ++ [func]}
      end
    end
  end

  @doc """
  Adds an after_transaction hook to the changeset. Cannot be called within other hooks.

  `after_transaction` hooks differ from `after_action` hooks in that they are run
  on success *and* failure of the action or some previous hook.

  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
  """
  @spec after_transaction(
          t(),
          after_transaction_fun(),
          Keyword.t()
        ) :: t()
  def after_transaction(changeset, func, opts \\ []) do
    changeset = maybe_dirty_hook(changeset, :after_transaction)

    if changeset.phase in [:pending, :validate] do
      if opts[:prepend?] do
        %{changeset | after_transaction: [func | changeset.after_transaction]}
      else
        %{changeset | after_transaction: changeset.after_transaction ++ [func]}
      end
    else
      Ash.Changeset.add_error(
        changeset,
        "Cannot add after_transaction hooks inside of other hooks, or in atomic hooks. Current phase: #{inspect(changeset.phase)}."
      )
    end
  end

  @doc """
  Adds an around_action hook to the changeset.

  Your function will get the changeset, and a callback that must be called with a changeset (that may be modified).
  The callback will return `{:ok, result, changeset, instructions}` or `{:error, error}`. You can modify these values, but the
  return value must be one of those types. Instructions contains the notifications in its `notifications` key, i.e
  `%{notifications: [%Ash.Resource.Notification{}, ...]}`.

  The around_action calls happen first, and then (after they each resolve their callbacks) the `before_action`
  hooks are called, followed by the action itself occurring at the data layer and then the `after_action` hooks being run.
  Then, the code that appeared *after* the callbacks were called is then run.

  For example:
  ```elixir
  changeset
  |> Ash.Changeset.around_action(fn changeset, callback ->
    IO.puts("first around: before")
    result = callback.(changeset)
    IO.puts("first around: after")

    result
  end)
  |> Ash.Changeset.around_action(fn changeset, callback ->
    IO.puts("second around: before")
    result = callback.(changeset)
    IO.puts("second around: after")

    result
  end)
  |> Ash.Changeset.before_action(fn changeset ->
    IO.puts("first before")
    changeset
  end, append?: true)
  |> Ash.Changeset.before_action(fn changeset ->
    IO.puts("second before")
    changeset
  end, append?: true)
  |> Ash.Changeset.after_action(fn changeset, result ->
    IO.puts("first after")
    {:ok, result}
  end)
  |> Ash.Changeset.after_action(fn changeset, result ->
    IO.puts("second after")
    {:ok, result}
  end)
  ```

  This would print:
  ```
  first around: before
  second around: before
  first before
  second before
               <-- action happens here
  first after
  second after
  second around: after <-- Notice that because of the callbacks, the after of the around hooks is reversed from the before
  first around: after
  ```

  Warning: using this without understanding how it works can cause big problems.
  You *must* call the callback function that is provided to your hook, and the return value must
  contain the same structure that was given to you, i.e `{:ok, result_of_action, instructions}`.

  You can almost always get the same effect by using `before_action`, setting some context on the changeset
  and reading it out in an `after_action` hook.
  """

  @spec around_action(t(), around_action_fun()) :: t()
  def around_action(changeset, func) do
    changeset = maybe_dirty_hook(changeset, :around_action)
    %{changeset | around_action: changeset.around_action ++ [func]}
  end

  @doc """
  Adds an around_transaction hook to the changeset.

  Your function will get the changeset, and a callback that must be called with a changeset (that may be modified).
  The callback will return `{:ok, result}` or `{:error, error}`. You can modify these values, but the return value
  must be one of those types.

  The around_transaction calls happen first, and then (after they each resolve their callbacks) the `before_transaction`
  hooks are called, followed by the action hooks and then the `after_transaction` hooks being run.
  Then, the code that appeared *after* the callbacks were called is then run.

  For example:

  ```elixir
  changeset
  |> Ash.Changeset.around_transaction(fn changeset, callback ->
    IO.puts("first around: before")
    result = callback.(changeset)
    IO.puts("first around: after")

    result
  end)
  |> Ash.Changeset.around_transaction(fn changeset, callback ->
    IO.puts("second around: before")
    result = callback.(changeset)
    IO.puts("second around: after")

    result
  end)
  |> Ash.Changeset.before_transaction(fn changeset ->
    IO.puts("first before")
    changeset
  end, append?: true)
  |> Ash.Changeset.before_transaction(fn changeset ->
    IO.puts("second before")
    changeset
  end, append?: true)
  |> Ash.Changeset.after_transaction(fn changeset, result ->
    IO.puts("first after")
    result
  end)
  |> Ash.Changeset.after_transaction(fn changeset, result ->
    IO.puts("second after")
    result
  end)
  ```

  This would print:

  ```
  first around: before
  second around: before
  first before
  second before
               <-- action hooks happens here
  first after
  second after
  second around: after <-- Notice that because of the callbacks, the after of the around hooks is reversed from the before
  first around: after
  ```

  Warning: using this without understanding how it works can cause big problems.
  You *must* call the callback function that is provided to your hook, and the return value must
  contain the same structure that was given to you, i.e `{:ok, result_of_action}`.

  You can almost always get the same effect by using `before_transaction`, setting some context on the changeset
  and reading it out in an `after_transaction` hook.
  """

  @spec around_transaction(t(), around_transaction_fun()) :: t()
  def around_transaction(changeset, func) do
    changeset = maybe_dirty_hook(changeset, :around_transaction)
    %{changeset | around_transaction: changeset.around_transaction ++ [func]}
  end

  defp maybe_dirty_hook(changeset, type) do
    if changeset.phase == :pending do
      %{changeset | dirty_hooks: Enum.uniq([type | changeset.dirty_hooks])}
    else
      changeset
    end
  end

  @doc """
  Returns the original data with attribute changes merged, if the changeset is valid.

  Options:

  * force? - applies current attributes even if the changeset is not valid
  """
  @spec apply_attributes(t(), opts :: Keyword.t()) :: {:ok, Ash.Resource.record()} | {:error, t()}
  def apply_attributes(changeset, opts \\ [])

  def apply_attributes(%{valid?: true} = changeset, _opts) do
    changeset = set_defaults(changeset, changeset.action_type, true)

    {:ok,
     Enum.reduce(changeset.attributes, changeset.data, fn {attribute, value}, data ->
       Map.put(data, attribute, value)
     end)}
  end

  def apply_attributes(changeset, opts) do
    if opts[:force?] do
      apply_attributes(%{changeset | valid?: true}, opts)
    else
      {:error, changeset}
    end
  end

  defp remove_default(changeset, attribute) do
    %{changeset | defaults: changeset.defaults -- [attribute]}
  end

  @doc "Clears an attribute or relationship change off of the changeset."
  def clear_change(changeset, field) do
    cond do
      attr = Ash.Resource.Info.attribute(changeset.resource, field) ->
        %{
          changeset
          | attributes: Map.delete(changeset.attributes, attr.name),
            atomics: Keyword.delete(changeset.atomics, attr.name)
        }

      rel = Ash.Resource.Info.relationship(changeset.resource, field) ->
        %{changeset | relationships: Map.delete(changeset.relationships, rel.name)}

      true ->
        changeset
    end
  end

  @doc """
  Sets a custom error handler on the changeset.

  The error handler should be a two argument function or an mfa, in which case the first two arguments will be set
  to the changeset and the error, w/ the supplied arguments following those.

  Any errors generated are passed to `handle_errors`, which can return any of the following:

  * `:ignore` - the error is discarded, and the changeset is not marked as invalid
  * `changeset` - a new (or the same) changeset. The error is not added (you'll want to add an error yourself), but the changeset *is* marked as invalid.
  * `{changeset, error}` - a new (or the same) error and changeset. The error is added to the changeset, and the changeset is marked as invalid.
  * `anything_else` - is treated as a new, transformed version of the error. The result is added as an error to the changeset, and the changeset is marked as invalid.
  """
  @spec handle_errors(
          t(),
          (t(), error :: term -> :ignore | t() | (error :: term) | {error :: term, t()})
          | {module, atom, [term]}
        ) :: t()
  def handle_errors(changeset, {m, f, a}) do
    %{changeset | handle_errors: &apply(m, f, [&1, &2 | a])}
  end

  def handle_errors(changeset, func) do
    %{changeset | handle_errors: func}
  end

  @doc """
  Adds a filter for a record being updated or destroyed.

  Used by optimistic locking. See `Ash.Resource.Change.Builtins.optimistic_lock/1` for more.
  """
  @spec filter(t(), Ash.Expr.t()) :: t()
  def filter(changeset, expr) when expr in [nil, %{}, []] do
    changeset
  end

  def filter(changeset, expression) do
    if Ash.DataLayer.data_layer_can?(changeset.resource, :changeset_filter) do
      expression = Ash.Filter.parse!(changeset.resource, expression)

      actor = changeset.context[:private][:actor]

      if is_nil(actor) && Ash.Expr.template_references_actor?(expression) do
        add_error(changeset, Ash.Error.Changes.ActionRequiresActor.exception([]))
      else
        expression =
          Ash.Expr.fill_template(
            expression,
            actor,
            changeset.arguments,
            changeset.context
          )

        with {:ok, expression} <-
               Ash.Filter.hydrate_refs(expression, %{
                 resource: changeset.resource,
                 public?: false
               }),
             {:ok, full_expression} <-
               Ash.Filter.add_to_filter(
                 changeset.filter,
                 expression
               ) do
          %{changeset | filter: full_expression}
          |> record_added_filter(expression)
        else
          {:error, error} ->
            Ash.Changeset.add_error(changeset, error)
        end
      end
    else
      IO.warn(
        "changeset.filter is not supported by the #{inspect(Ash.DataLayer.data_layer(changeset.resource))} data layer"
      )

      changeset
    end
  end

  defp record_added_filter(%{phase: :pending} = changeset, expression) do
    case Ash.Filter.add_to_filter(changeset.added_filter, expression) do
      {:ok, new_added_filter} ->
        %{changeset | added_filter: new_added_filter}

      {:error, error} ->
        Ash.Changeset.add_error(changeset, Ash.Error.to_ash_error(error))
    end
  end

  defp record_added_filter(changeset, _), do: changeset

  @doc """
  Adds an error to the changesets errors list, and marks the change as `valid?: false`.

  ## Error Data

  The given `errors` argument can be a string, a keyword list, a struct, or a list of any of the three.

  If `errors` is a keyword list, or a list of keyword lists, the following keys are supported in the keyword list:

  - `field` (atom) - the field that the error is for. This is required, unless `fields` is given.
  - `fields` (list of atoms) - the fields that the error is for. This is required, unless `field` is given.
  - `message` (string) - the error message
  - `value` (any) - (optional) the field value that caused the error
  """
  @spec add_error(t(), error_info() | [error_info()], Keyword.t()) :: t()
  @spec add_error(t(), term | String.t() | list(term | String.t())) :: t()
  def add_error(changeset, errors, path \\ [])

  def add_error(changeset, errors, path) when is_list(errors) do
    if Keyword.keyword?(errors) do
      errors
      |> to_change_error()
      |> Ash.Error.set_path(path)
      |> handle_error(changeset)
    else
      Enum.reduce(errors, changeset, &add_error(&2, &1, path))
    end
  end

  def add_error(changeset, error, path) when is_binary(error) do
    add_error(
      changeset,
      InvalidChanges.exception(message: error),
      path
    )
  end

  def add_error(changeset, %__MODULE__{errors: errors}, path) do
    add_error(changeset, errors, path)
  end

  def add_error(changeset, error, path) do
    error
    |> Ash.Error.set_path(path)
    |> handle_error(changeset)
  end

  defp handle_error(error, %{handle_errors: nil} = changeset) do
    %{changeset | valid?: false, errors: [error | changeset.errors]}
  end

  defp handle_error(error, changeset) do
    changeset
    |> changeset.handle_errors.(error)
    |> case do
      :ignore ->
        changeset

      {:ignore, changeset} ->
        changeset

      %__MODULE__{} = changeset ->
        %{changeset | valid?: false}

      {changeset, error} ->
        %{changeset | valid?: false, errors: [error | changeset.errors]}

      error ->
        %{changeset | valid?: false, errors: [error | changeset.errors]}
    end
  end

  defp to_change_error(keyword) do
    error =
      if keyword[:field] do
        InvalidAttribute.exception(
          field: keyword[:field],
          message: keyword[:message],
          value: keyword[:value],
          vars: keyword
        )
      else
        InvalidChanges.exception(
          fields: keyword[:fields] || [],
          message: keyword[:message],
          value: keyword[:value],
          vars: keyword
        )
      end

    if keyword[:path] do
      Ash.Error.set_path(error, keyword[:path])
    else
      error
    end
  end

  defp prepare_change(%{action_type: :create}, _attribute, value, _constraints), do: {:ok, value}

  defp prepare_change(changeset, attribute, value, constraints) do
    old_value = Map.get(changeset.data, attribute.name)
    Ash.Type.prepare_change(attribute.type, old_value, value, constraints)
  end

  defp handle_change(%{action_type: :create}, _attribute, value, _constraints), do: {:ok, value}

  defp handle_change(changeset, attribute, value, constraints) do
    old_value = Map.get(changeset.data, attribute.name)
    Ash.Type.handle_change(attribute.type, old_value, value, constraints)
  end

  defp add_invalid_errors(value, type, changeset, attribute, message \\ nil) do
    changeset = %{changeset | invalid_keys: MapSet.put(changeset.invalid_keys, attribute.name)}

    message
    |> Ash.Helpers.flatten_preserving_keywords()
    |> Enum.reduce(changeset, fn message, changeset ->
      if is_exception(message) do
        error =
          message
          |> Ash.Error.to_ash_error()

        errors =
          case error do
            %class{errors: errors}
            when class in [
                   Ash.Error.Invalid,
                   Ash.Error.Unknown,
                   Ash.Error.Forbidden,
                   Ash.Error.Framework
                 ] ->
              errors

            error ->
              [error]
          end

        Enum.reduce(errors, changeset, fn error, changeset ->
          add_error(changeset, Ash.Error.set_path(error, attribute.name))
        end)
      else
        opts = Ash.Type.Helpers.error_to_exception_opts(message, attribute)

        exception =
          case type do
            :attribute -> InvalidAttribute
            :argument -> InvalidArgument
          end

        Enum.reduce(opts, changeset, fn opts, changeset ->
          error =
            exception.exception(
              value: value,
              field: Keyword.get(opts, :field),
              message: Keyword.get(opts, :message),
              vars: opts
            )

          error =
            if opts[:path] do
              Ash.Error.set_path(error, opts[:path])
            else
              error
            end

          add_error(changeset, error)
        end)
      end
    end)
  end

  defp set_phase(changeset, phase) when changeset.phase == phase, do: changeset

  defp set_phase(changeset, phase) when phase in @phases, do: %{changeset | phase: phase}
  defp clear_phase(changeset), do: %{changeset | phase: :pending}
end

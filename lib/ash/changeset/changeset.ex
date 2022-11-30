defmodule Ash.Changeset do
  @moduledoc """
  Changesets are used to create and update data in Ash.

  Create a changeset with `new/1` or `new/2`, and alter the attributes
  and relationships using the functions provided in this module.  Nothing in this module
  actually incurs changes in a data layer. To commit a changeset, see `c:Ash.Api.create/2`
  and `c:Ash.Api.update/2`.

  See the action DSL documentation for more.
  """

  defstruct [
    :data,
    :action_type,
    :action,
    :resource,
    :api,
    :tenant,
    :__validated_for_action__,
    :handle_errors,
    :timeout,
    select: nil,
    params: %{},
    action_failed?: false,
    arguments: %{},
    context: %{},
    defaults: [],
    after_action: [],
    around_action: [],
    before_action: [],
    errors: [],
    valid?: true,
    attributes: %{},
    relationships: %{},
    change_dependencies: []
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
          concat("tenant: ", to_doc(changeset.tenant, opts))
        else
          empty()
        end

      api =
        if changeset.api do
          concat("api: ", to_doc(changeset.api, opts))
        else
          empty()
        end

      container_doc(
        "#Ash.Changeset<",
        [
          api,
          concat("action_type: ", inspect(changeset.action_type)),
          concat("action: ", inspect(changeset.action && changeset.action.name)),
          tenant,
          concat("attributes: ", to_doc(changeset.attributes, opts)),
          concat("relationships: ", to_doc(changeset.relationships, opts)),
          arguments(changeset, opts),
          concat("errors: ", to_doc(changeset.errors, opts)),
          concat("data: ", to_doc(changeset.data, opts)),
          context,
          concat("valid?: ", to_doc(changeset.valid?, opts))
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
              if argument.sensitive? do
                {argument.name, "**redacted**"}
              else
                {argument.name, Ash.Changeset.get_argument(changeset, argument.name)}
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

  @type t :: %__MODULE__{data: Ash.Resource.record()}

  alias Ash.Error.{
    Changes.InvalidArgument,
    Changes.InvalidAttribute,
    Changes.InvalidChanges,
    Changes.InvalidRelationship,
    Changes.NoSuchAttribute,
    Changes.NoSuchRelationship,
    Changes.Required,
    Invalid.NoSuchResource,
    Invalid.TimeoutNotSupported
  }

  require Ash.Query
  require Ash.Tracer
  require Logger

  defmacrop maybe_already_validated_error!(changeset, alternative \\ nil) do
    {function, arity} = __CALLER__.function

    if alternative do
      quote do
        changeset = unquote(changeset)

        if changeset.__validated_for_action__ && !changeset.context[:private][:in_before_action?] do
          IO.warn("""
          Changeset has already been validated for action #{inspect(changeset.__validated_for_action__)}.

          In the future, this will become an error.

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

          In the future, this will become an error.

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
  Returns a new changeset over a resource. Prefer `for_action` or `for_create`, etc. over this function if possible.

  Warning: You almost always want to use `for_action/4` (or `for_create`, etc...)

  You can use this to start a changeset and make a few changes prior to calling `for_action`. For example:

  ```elixir
  Resource
  |> Ash.Changeset.new()
  |> Ash.Changeset.change_attribute(:name, "foobar")
  |> Ash.Changeset.for_action(...)
  ```
  """
  @spec new(Ash.Resource.t() | Ash.Resource.record(), params :: map) :: t
  def new(resource, params \\ %{})

  def new(%resource{} = record, params) do
    tenant =
      record
      |> Map.get(:__metadata__, %{})
      |> Map.get(:tenant, nil)

    context = Ash.Resource.Info.default_context(resource) || %{}

    if Ash.Resource.Info.resource?(resource) do
      %__MODULE__{resource: resource, data: record, action_type: :update}
      |> change_attributes(params)
      |> set_context(context)
      |> set_tenant(tenant)
    else
      %__MODULE__{
        resource: resource,
        action_type: :update,
        data: struct(resource)
      }
      |> add_error(NoSuchResource.exception(resource: resource))
      |> set_tenant(tenant)
      |> set_context(context)
    end
  end

  def new(resource, params) do
    if Ash.Resource.Info.resource?(resource) do
      %__MODULE__{
        resource: resource,
        action_type: :create,
        data: struct(resource)
      }
      |> change_attributes(params)
    else
      %__MODULE__{resource: resource, action_type: :create, data: struct(resource)}
      |> add_error(NoSuchResource.exception(resource: resource))
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
      %{changeset | select: Enum.uniq(List.wrap(fields))}
    else
      %{changeset | select: Enum.uniq(List.wrap(fields) ++ (changeset.select || []))}
    end
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
      to_select =
        changeset.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

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

          attribute && (attribute.private? || attribute.primary_key?)
        end
    end
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

    action = Ash.Resource.Info.action(resource, action)

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
        "If set to `true`, values are only required when the action is run (instead of immediately)."
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
      type: :atom,
      doc:
        "A tracer to use. Will be carried over to the action. For more information see `Ash.Tracer`."
    ],
    tenant: [
      type: :any,
      doc: "set the tenant on the changeset"
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
  the api action.

  ### Params
  `params` may be attributes, relationships, or arguments. You can safely pass user/form input directly into this function.
  Only public attributes and relationships are supported. If you want to change private attributes as well, see the
  Customization section below. `params` are stored directly as given in the `params` field of the changeset, which is used

  ### Opts

  #{Spark.OptionsHelpers.docs(@for_create_opts)}

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

    changeset
    |> set_context(%{
      private: %{upsert?: opts[:upsert?] || false, upsert_identity: opts[:upsert_identity]}
    })
    |> do_for_action(action, params, opts)
  end

  @doc """
  Constructs a changeset for a given update action, and validates it.

  Anything that is modified prior to `for_update/4` is validated against the rules of the action, while *anything after it is not*.

  See `for_create/4` for more information
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

  Anything that is modified prior to `for_destroy/4` is validated against the rules of the action, while *anything after it is not*.

  Once a changeset has been validated by `for_destroy/4`, it isn't validated again in the action.
  New changes added are validated individually, though. This allows you to create a changeset according
  to a given action, and then add custom changes if necessary.
  """
  def for_destroy(initial, action_name, params \\ %{}, opts \\ []) do
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

    if changeset.valid? do
      action = Ash.Resource.Info.action(changeset.resource, action_name, changeset.action_type)

      if action do
        if action.soft? do
          do_for_action(%{changeset | action_type: :destroy}, action.name, params, opts)
        else
          {changeset, opts} =
            Ash.Actions.Helpers.add_process_context(changeset.api, changeset, opts)

          name =
            "changeset:" <> Ash.Resource.Info.trace_name(changeset.resource) <> ":#{action.name}"

          Ash.Tracer.span :changeset,
                          name,
                          opts[:tracer] do
            Ash.Tracer.telemetry_span [:ash, :changeset], %{
              resource_short_name: Ash.Resource.Info.short_name(changeset.resource)
            } do
              metadata = %{
                resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                resource: changeset.resource,
                actor: opts[:actor],
                tenant: opts[:tenant],
                action: action.name,
                authorize?: opts[:authorize?]
              }

              Ash.Tracer.set_metadata(opts[:tracer], :changeset, metadata)

              changeset
              |> Map.put(:action, action)
              |> reset_arguments()
              |> handle_errors(action.error_handler)
              |> set_actor(opts)
              |> set_authorize(opts)
              |> set_tracer(opts)
              |> set_tenant(opts[:tenant] || changeset.tenant)
              |> cast_params(action, params)
              |> set_argument_defaults(action)
              |> require_arguments(action)
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
        raise_no_action(changeset.resource, action_name, :destroy)
      end
    else
      changeset
    end
  end

  defp reset_arguments(%{arguments: arguments} = changeset) when is_map(arguments) do
    Enum.reduce(arguments, changeset, fn {key, value}, changeset ->
      set_argument(changeset, key, value)
    end)
  end

  defp reset_arguments(changeset), do: changeset

  @spec set_on_upsert(t(), list(atom)) :: Keyword.t()
  def set_on_upsert(changeset, upsert_keys) do
    keys = upsert_keys || Ash.Resource.Info.primary_key(changeset.resource)

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

  defp upsert_update_defaults(changeset) do
    attributes =
      changeset.resource
      |> Ash.Resource.Info.attributes()
      |> Enum.reject(&is_nil(&1.update_default))

    attributes
    |> static_defaults()
    |> Enum.concat(lazy_matching_defaults(attributes))
    |> Enum.concat(lazy_non_matching_defaults(attributes))
  end

  defp static_defaults(attributes) do
    attributes
    |> Enum.reject(&get_default_fun(&1))
    |> Enum.map(&{&1.name, &1.update_default})
  end

  defp lazy_non_matching_defaults(attributes) do
    attributes
    |> Enum.filter(&(!&1.match_other_defaults? && get_default_fun(&1)))
    |> Enum.map(&{&1.name, &1.update_default})
  end

  defp lazy_matching_defaults(attributes) do
    attributes
    |> Enum.filter(&(&1.match_other_defaults? && get_default_fun(&1)))
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

  defp get_default_fun(attribute) do
    if is_function(attribute.update_default) or match?({_, _, _}, attribute.update_default) do
      attribute.update_default
    end
  end

  defp do_for_action(changeset, action_name, params, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(changeset.api, changeset, opts)

    if changeset.valid? do
      action = Ash.Resource.Info.action(changeset.resource, action_name, changeset.action_type)

      if action do
        name =
          "changeset:" <> Ash.Resource.Info.trace_name(changeset.resource) <> ":#{action.name}"

        Ash.Tracer.span :changeset,
                        name,
                        opts[:tracer] do
          Ash.Tracer.telemetry_span [:ash, :changeset], %{
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource)
          } do
            metadata = %{
              resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
              resource: changeset.resource,
              actor: opts[:actor],
              tenant: opts[:tenant],
              action: action.name,
              authorize?: opts[:authorize?]
            }

            Ash.Tracer.set_metadata(opts[:tracer], :changeset, metadata)

            changeset =
              changeset
              |> Map.put(:action, action)
              |> reset_arguments()
              |> handle_errors(action.error_handler)
              |> set_actor(opts)
              |> set_authorize(opts)
              |> set_tracer(opts)
              |> timeout(changeset.timeout || opts[:timeout])
              |> set_tenant(
                opts[:tenant] || changeset.tenant || changeset.data.__metadata__[:tenant]
              )
              |> cast_params(action, params || %{})
              |> set_argument_defaults(action)
              |> require_arguments(action)
              |> validate_attributes_accepted(action)
              |> require_values(action.type, false, action.require_attributes)
              |> set_defaults(changeset.action_type, false)
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
        raise_no_action(changeset.resource, action_name, changeset.action_type)
      end
    else
      changeset
    end
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
         _api
       ) do
    changeset
  end

  defp validate_identity(
         %{action: %{soft?: true}} = changeset,
         identity,
         api
       ) do
    do_validate_identity(changeset, identity, api)
  end

  defp validate_identity(
         %{action: %{type: type}} = changeset,
         identity,
         api
       )
       when type in [:create, :update] do
    do_validate_identity(changeset, identity, api)
  end

  defp validate_identity(
         %{action: %{type: type}} = changeset,
         identity,
         api
       )
       when type in [:create, :update] do
    do_validate_identity(changeset, identity, api)
  end

  defp validate_identity(changeset, _, _), do: changeset

  defp do_validate_identity(changeset, identity, api) do
    if changeset.context[:private][:upsert_identity] == identity.name do
      changeset
    else
      if Enum.any?(identity.keys, &changing_attribute?(changeset, &1)) do
        action = Ash.Resource.Info.primary_action(changeset.resource, :read).name

        values =
          Enum.map(identity.keys, fn key ->
            {key, Ash.Changeset.get_attribute(changeset, key)}
          end)

        changeset.resource
        |> Ash.Query.for_read(action, %{},
          tenant: changeset.tenant,
          actor: changeset.context[:private][:actor],
          authorize?: changeset.context[:private][:authorize?],
          tracer: changeset.context[:private][:tracer]
        )
        |> Ash.Query.do_filter(values)
        |> Ash.Query.limit(1)
        |> api.read_one()
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
          add_error(
            changeset,
            Ash.Error.Changes.Required.exception(
              resource: changeset.resource,
              field: argument.name,
              type: :argument
            )
          )
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

  defp cast_params(changeset, action, params) do
    changeset = %{
      changeset
      | params: Map.merge(changeset.params || %{}, Enum.into(params || %{}, %{}))
    }

    Enum.reduce(params, changeset, fn {name, value}, changeset ->
      cond do
        has_argument?(action, name) ->
          set_argument(changeset, name, value)

        attr = Ash.Resource.Info.public_attribute(changeset.resource, name) ->
          if attr.writable? do
            change_attribute(changeset, attr.name, value)
          else
            changeset
          end

        true ->
          changeset
      end
    end)
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.private? == false && &1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(&1.private? == false && to_string(&1.name) == name))
  end

  defp validate_attributes_accepted(changeset, %{accept: nil}), do: changeset

  defp validate_attributes_accepted(changeset, %{accept: accepted_attributes}) do
    changeset.attributes
    |> Enum.reject(fn {key, _value} ->
      key in accepted_attributes
    end)
    |> Enum.reduce(changeset, fn {key, _}, changeset ->
      add_error(
        changeset,
        InvalidAttribute.exception(field: key, message: "cannot be changed")
      )
    end)
  end

  defp run_action_changes(changeset, %{changes: changes}, actor, authorize?, tracer, metadata) do
    changes = changes ++ Ash.Resource.Info.changes(changeset.resource, changeset.action_type)

    Enum.reduce(changes, changeset, fn
      %{only_when_valid?: true}, %{valid?: false} = changeset ->
        changeset

      %{change: {module, opts}, where: where}, changeset ->
        if Enum.all?(where || [], fn {module, opts} ->
             Ash.Tracer.span :validation, "change condition: #{inspect(module)}", tracer do
               Ash.Tracer.telemetry_span [:ash, :validation], %{
                 resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
                 validation: inspect(module)
               } do
                 Ash.Tracer.set_metadata(opts[:tracer], :validation, metadata)

                 opts =
                   Ash.Filter.build_filter_from_template(
                     opts,
                     actor,
                     changeset.arguments,
                     changeset.context
                   )

                 module.validate(changeset, opts) == :ok
               end
             end
           end) do
          Ash.Tracer.span :change, "change: #{inspect(module)}", tracer do
            Ash.Tracer.telemetry_span [:ash, :change], %{
              resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
              change: inspect(module)
            } do
              {:ok, opts} = module.init(opts)

              Ash.Tracer.set_metadata(opts[:tracer], :change, metadata)

              opts =
                Ash.Filter.build_filter_from_template(
                  opts,
                  actor,
                  changeset.arguments,
                  changeset.context
                )

              module.change(changeset, opts, %{
                actor: actor,
                authorize?: authorize? || false,
                tracer: tracer
              })
            end
          end
        else
          changeset
        end

      %{validation: _} = validation, changeset ->
        validate(changeset, validation, tracer, metadata, actor)
    end)
  end

  @doc false
  def set_defaults(changeset, action_type, lazy? \\ false)

  def set_defaults(changeset, :create, lazy?) do
    attributes = Ash.Resource.Info.attributes(changeset.resource)

    with_static_defaults =
      attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.default) &&
          not (is_function(attribute.default) or match?({_, _, _}, attribute.default))
      end)
      |> Enum.reduce(changeset, fn attribute, changeset ->
        changeset
        |> force_change_new_attribute_lazy(attribute.name, fn ->
          default(:create, attribute)
        end)
        |> Map.update!(:defaults, fn defaults ->
          [attribute.name | defaults]
        end)
      end)
      |> Map.update!(:defaults, &Enum.uniq/1)

    if lazy? do
      set_lazy_defaults(with_static_defaults, attributes, :create)
    else
      with_static_defaults
    end
    |> Map.update!(:defaults, &Enum.uniq/1)
  end

  def set_defaults(changeset, :update, lazy?) do
    attributes = Ash.Resource.Info.attributes(changeset.resource)

    with_static_defaults =
      attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.update_default) &&
          not (is_function(attribute.update_default) or
                 match?({_, _, _}, attribute.update_default))
      end)
      |> Enum.reduce(changeset, fn attribute, changeset ->
        changeset
        |> force_change_new_attribute_lazy(attribute.name, fn ->
          default(:update, attribute)
        end)
        |> Map.update!(:defaults, fn defaults ->
          [attribute.name | defaults]
        end)
      end)

    if lazy? do
      set_lazy_defaults(with_static_defaults, attributes, :update)
    else
      with_static_defaults
    end
    |> Map.update!(:defaults, &Enum.uniq/1)
  end

  def set_defaults(changeset, _, _) do
    changeset
  end

  defp set_lazy_defaults(changeset, attributes, type) do
    changeset
    |> set_lazy_non_matching_defaults(attributes, type)
    |> set_lazy_matching_defaults(attributes, type)
  end

  defp set_lazy_non_matching_defaults(changeset, attributes, type) do
    attributes
    |> Enum.filter(fn attribute ->
      !attribute.match_other_defaults? && get_default_fun(attribute, type)
    end)
    |> Enum.reduce(changeset, fn attribute, changeset ->
      changeset
      |> force_change_new_attribute_lazy(attribute.name, fn ->
        default(type, attribute)
      end)
      |> Map.update!(:defaults, fn defaults ->
        [attribute.name | defaults]
      end)
    end)
  end

  defp get_default_fun(attribute, :create) do
    if is_function(attribute.default) or match?({_, _, _}, attribute.default) do
      attribute.default
    end
  end

  defp get_default_fun(attribute, :update) do
    if is_function(attribute.update_default) or match?({_, _, _}, attribute.update_default) do
      attribute.update_default
    end
  end

  defp set_lazy_matching_defaults(changeset, attributes, type) do
    attributes
    |> Enum.filter(fn attribute ->
      attribute.match_other_defaults? && get_default_fun(attribute, type)
    end)
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
        changeset
        |> force_change_new_attribute(attribute.name, default_value)
        |> Map.update!(:defaults, fn defaults ->
          [attribute.name | defaults]
        end)
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
    changeset.resource
    # We use the `changeset.action_type` to support soft deletes
    # Because a delete is an `update` with an action type of `update`
    |> Ash.Resource.Info.validations(changeset.action_type)
    |> Enum.reduce(changeset, &validate(&2, &1, tracer, metadata, actor))
  end

  defp validate(changeset, validation, tracer, metadata, actor) do
    if validation.before_action? do
      before_action(changeset, fn changeset ->
        if validation.only_when_valid? and not changeset.valid? do
          changeset
        else
          do_validation(changeset, validation, tracer, metadata, actor)
        end
      end)
    else
      if validation.only_when_valid? and not changeset.valid? do
        changeset
      else
        do_validation(changeset, validation, tracer, metadata, actor)
      end
    end
  end

  defp do_validation(changeset, validation, tracer, metadata, actor) do
    if Enum.all?(validation.where || [], fn {module, opts} ->
         opts =
           Ash.Filter.build_filter_from_template(
             opts,
             actor,
             changeset.arguments,
             changeset.context
           )

         module.validate(changeset, opts) == :ok
       end) do
      Ash.Tracer.span :validation, "validate: #{inspect(validation.module)}", tracer do
        Ash.Tracer.telemetry_span [:ash, :validation], %{
          resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
          validation: inspect(validation.module)
        } do
          Ash.Tracer.set_metadata(tracer, :validation, metadata)

          opts =
            Ash.Filter.build_filter_from_template(
              validation.opts,
              actor,
              changeset.arguments,
              changeset.context
            )

          case validation.module.validate(changeset, opts) do
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

  defp override_validation_message(error, message) do
    case error do
      %{field: field} when not is_nil(field) ->
        error
        |> Map.take([:field, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
        |> InvalidAttribute.exception()

      %{fields: fields} when fields not in [nil, []] ->
        error
        |> Map.take([:fields, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
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

    Enum.reduce(attributes, changeset, fn required_attribute, changeset ->
      if is_atom(required_attribute) do
        if is_nil(get_attribute(changeset, required_attribute)) do
          add_error(
            changeset,
            Required.exception(
              resource: changeset.resource,
              field: required_attribute,
              type: :attribute
            )
          )
        else
          changeset
        end
      else
        if private_and_belongs_to? || changing_attribute?(changeset, required_attribute.name) do
          if is_nil(get_attribute(changeset, required_attribute.name)) do
            add_error(
              changeset,
              Required.exception(
                resource: changeset.resource,
                field: required_attribute.name,
                type: :attribute
              )
            )
          else
            changeset
          end
        else
          if is_nil(required_attribute.default) do
            add_error(
              changeset,
              Required.exception(
                resource: changeset.resource,
                field: required_attribute.name,
                type: :attribute
              )
            )
          else
            changeset
          end
        end
      end
    end)
  end

  def require_values(changeset, :update, private_and_belongs_to?, attrs) do
    attributes =
      attrs ||
        attributes_to_require(changeset.resource, changeset.action, private_and_belongs_to?)

    Enum.reduce(attributes, changeset, fn required_attribute, changeset ->
      if is_atom(required_attribute) do
        if is_nil(get_attribute(changeset, required_attribute)) do
          add_error(
            changeset,
            Required.exception(
              resource: changeset.resource,
              field: required_attribute,
              type: :attribute
            )
          )
        else
          changeset
        end
      else
        if changing_attribute?(changeset, required_attribute.name) do
          if is_nil(get_attribute(changeset, required_attribute.name)) do
            add_error(
              changeset,
              Required.exception(
                resource: changeset.resource,
                field: required_attribute.name,
                type: :attribute
              )
            )
          else
            changeset
          end
        else
          changeset
        end
      end
    end)
  end

  def require_values(changeset, _, _, _), do: changeset

  # Attributes that are private and/or are the source field of a belongs_to relationship
  # are typically not set by input, so they aren't required until the actual action
  # is run.
  defp attributes_to_require(resource, %{type: :create, accept: accept}, true) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reject(&(&1.allow_nil? || &1.generated?))
    |> Enum.filter(&(&1.name in accept))
  end

  defp attributes_to_require(resource, %{type: :create, accept: accept} = action, false) do
    resource
    |> do_attributes_to_require(action)
    |> Enum.filter(&(&1.name in accept))
  end

  defp attributes_to_require(resource, _action, true = _private_and_belongs_to?) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reject(&(&1.allow_nil? || &1.generated?))
  end

  defp attributes_to_require(resource, action, false = _private_and_belongs_to?) do
    do_attributes_to_require(resource, action)
  end

  defp do_attributes_to_require(resource, action) do
    belongs_to =
      resource
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(&(&1.type == :belongs_to))
      |> Enum.map(& &1.source_attribute)

    action =
      case action do
        action when is_atom(action) ->
          Ash.Resource.Info.action(resource, action)

        _ ->
          action
      end

    allow_nil_input =
      case action do
        %{allow_nil_input: allow_nil_input} ->
          allow_nil_input

        _ ->
          []
      end

    masked_argument_names = Enum.map(action.arguments, & &1.name)

    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reject(
      &(&1.allow_nil? || &1.private? || !&1.writable? || &1.generated? ||
          &1.name in masked_argument_names || &1.name in belongs_to ||
          &1.name in allow_nil_input)
    )
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
             | {:error, term})
        ) ::
          {:ok, term, t(), %{notifications: list(Ash.Notifier.Notification.t())}} | {:error, term}
  def with_hooks(%{valid?: false} = changeset, _func) do
    {:error, changeset.errors}
  end

  def with_hooks(changeset, func) do
    if changeset.valid? do
      run_around_actions(changeset, func)
    else
      {:error, changeset.errors}
    end
  end

  defp run_around_actions(%{around_action: []} = changeset, func) do
    changeset = put_context(changeset, :private, %{in_before_action?: true})

    result =
      Enum.reduce_while(
        changeset.before_action,
        {changeset, %{notifications: []}},
        fn before_action, {changeset, instructions} ->
          metadata = %{
            api: changeset.api,
            resource: changeset.resource,
            resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
            actor: changeset.context[:private][:actor],
            tenant: changeset.context[:private][:actor],
            action: changeset.action && changeset.action.name,
            authorize?: changeset.context[:private][:authorize?]
          }

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
                  | notifications: instructions.notifications ++ List.wrap(notifications)
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

    case result do
      {:error, error} ->
        {:error, error}

      {changeset, %{notifications: before_action_notifications}} ->
        case func.(changeset) do
          {:ok, result, instructions} ->
            run_after_actions(
              result,
              instructions[:new_changeset] || changeset,
              List.wrap(instructions[:notifications]) ++ before_action_notifications
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
    around.(changeset, fn changeset ->
      run_around_actions(%{changeset | around_action: rest}, func)
    end)
  end

  defp run_after_actions(result, changeset, before_action_notifications) do
    Enum.reduce_while(
      changeset.after_action,
      {:ok, result, changeset, %{notifications: before_action_notifications}},
      fn after_action, {:ok, result, changeset, %{notifications: notifications} = acc} ->
        tracer = changeset.context[:private][:tracer]

        metadata = %{
          api: changeset.api,
          resource: changeset.resource,
          resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
          actor: changeset.context[:private][:actor],
          tenant: changeset.context[:private][:actor],
          action: changeset.action && changeset.action.name,
          authorize?: changeset.context[:private][:authorize?]
        }

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
              Enum.map(notifications ++ new_notifications, fn notification ->
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
              end)

            {:cont, {:ok, new_result, changeset, %{acc | notifications: all_notifications}}}

          {:ok, new_result} ->
            {:cont, {:ok, new_result, changeset, acc}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end
    )
  end

  @doc "Gets the value of an argument provided to the changeset"
  @spec get_argument(t, atom) :: term
  def get_argument(changeset, argument) when is_atom(argument) do
    if Map.has_key?(changeset.arguments, argument) do
      Map.get(changeset.arguments, argument)
    else
      Map.get(changeset.arguments, to_string(argument))
    end
  end

  @doc "fetches the value of an argument provided to the changeset or `:error`"
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

  @doc "Gets the changing value or the original value of an attribute"
  @spec get_attribute(t, atom) :: term
  def get_attribute(changeset, attribute) do
    case fetch_change(changeset, attribute) do
      {:ok, value} ->
        value

      :error ->
        get_data(changeset, attribute)
    end
  end

  @doc "Gets the value of an argument provided to the changeset, falling back to `Ash.Changeset.get_attribute/2` if nothing was provided"
  @spec get_argument_or_attribute(t, atom) :: term
  def get_argument_or_attribute(changeset, attribute) do
    case fetch_argument(changeset, attribute) do
      {:ok, value} -> value
      :error -> get_attribute(changeset, attribute)
    end
  end

  @doc "Gets the new value for an attribute, or `:error` if it is not being changed"
  @spec fetch_change(t, atom) :: {:ok, any} | :error
  def fetch_change(changeset, attribute) do
    Map.fetch(changeset.attributes, attribute)
  end

  @doc "Gets the value of an argument provided to the changeset, falling back to `Ash.Changeset.fetch_change/2` if nothing was provided"
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
  Puts a key/value in the changeset context that can be used later

  Do not use the `private` key in your custom context, as that is reserved for internal use.
  """
  @spec put_context(t(), atom, term) :: t()
  def put_context(changeset, key, value) do
    set_context(changeset, %{key => value})
  end

  @spec set_tenant(t(), String.t()) :: t()
  def set_tenant(changeset, tenant) do
    %{changeset | tenant: tenant}
  end

  @spec timeout(t(), nil | pos_integer, nil | pos_integer) :: t()
  def timeout(changeset, timeout, default \\ nil) do
    if Ash.DataLayer.data_layer_can?(changeset.resource, :timeout) || is_nil(timeout) do
      %{changeset | timeout: timeout || default}
    else
      add_error(changeset, TimeoutNotSupported.exception(resource: changeset.resource))
    end
  end

  @doc """
  Deep merges the provided map into the changeset context that can be used later

  Do not use the `private` key in your custom context, as that is reserved for internal use.
  """
  @spec set_context(t(), map | nil) :: t()
  def set_context(changeset, nil), do: changeset

  def set_context(changeset, map) do
    %{changeset | context: Ash.Helpers.deep_merge_maps(changeset.context, map)}
  end

  @type manage_relationship_type ::
          :append_and_remove | :append | :remove | :direct_control | :create

  @spec manage_relationship_opts(manage_relationship_type()) :: Keyword.t()
  def manage_relationship_opts(:replace) do
    Logger.warn(
      "type: :replace has been renamed to `:append_and_remove` in 2.0, and it will be removed in 2.1"
    )

    manage_relationship_opts(:append_and_remove)
  end

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

      This allows for specifying certain operations much more succinctly. The defaults that are modified are listed below

      ## `:append_and_remove`
        [
          on_lookup: :relate,
          on_no_match: :error,
          on_match: :ignore,
          on_missing: :unrelate
        ]

      ## `:append`
        [
          on_lookup: :relate,
          on_no_match: :error,
          on_match: :ignore,
          on_missing: :ignore
        ]

      ## `:remove`
        [
          on_no_match: :error,
          on_match: :unrelate,
          on_missing: :ignore
        ]

      ## `:direct_control`
        [
          on_lookup: :ignore,
          on_no_match: :create,
          on_match: :update,
          on_missing: :destroy
        ]

      ## `:create`
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
        "Validates that any referenced entities exist *before* the action is being performed, using the provided api for the read."
    ],
    on_no_match: [
      type: :any,
      default: :ignore,
      doc: """
      instructions for handling records where no matching record existed in the relationship
          * `:ignore`(default) - those inputs are ignored
          * `:match` - For "has_one" and "belongs_to" only, any input is treated as a match for an existing value. For has_many and many_to_many, this is the same as :ignore.
          * `:create` - the records are created using the destination's primary create action
          * `{:create, :action_name}` - the records are created using the specified action on the destination resource
          * `{:create, :action_name, :join_table_action_name, [:list, :of, :join_table, :params]}` - Same as `{:create, :action_name}` but takes
              the list of params specified out and applies them when creating the join record, with the provided join_table_action_name.
          * `:error`  - an eror is returned indicating that a record would have been created
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
      default: [:_primary_key],
      doc: """
      The list, in priority order, of identities to use when looking up records for `on_lookup`, and matching records with `on_match`.

      Use `:_primary_key` to prioritize checking a match with the primary key.
      All identities, along with `:_primary_key` are checked regardless, this only allows ensuring that some are checked first.
      """
    ],
    use_identities: [
      type: {:list, :atom},
      doc: """
      A list of identities that may be used to look up and compare records.
      Use `:_primary_key` to include the primary key. This does not determine the order
      that they are checked, use `identity_priority` for that. By default, all are used.
      """
    ],
    on_lookup: [
      type: :any,
      default: :ignore,
      doc: """
      Before creating a record(because no match was found in the relationship), the record can be looked up and related.
          * `:ignore`(default) - Does not look for existing entries (matches in the current relationship are still considered updates)
          * `:relate` - Same as calling `{:relate, primary_action_name}`
          * `{:relate, :action_name}` - the records are looked up by primary key/the first identity that is found (using the primary read action), and related. The action should be:
              * many_to_many - a create action on the join resource
              * has_many - an update action on the destination resource
              * has_one - an update action on the destination resource
              * belongs_to - an update action on the source resource
          * `{:relate, :action_name, :read_action_name}` - Same as the above, but customizes the read action called to search for matches.
          * `:relate_and_update` - Same as `:relate`, but the remaining parameters from the lookup are passed into the action that is used to change the relationship key
          * `{:relate_and_update, :action_name}` - Same as the above, but customizes the action used. The action should be:
              * many_to_many - a create action on the join resource
              * has_many - an update action on the destination resource
              * has_one - an update action on the destination resource
              * belongs_to - an update action on the source resource
          * `{:relate_and_update, :action_name, :read_action_name}` - Same as the above, but customizes the read action called to search for matches.
          * `{:relate_and_update, :action_name, :read_action_name, [:list, :of, :join_table, :params]}` - Same as the above, but uses the provided list of parameters when creating
             the join row (only relevant for many to many relationships). Use `:all` to *only* update the join record, and pass all parameters to its action
      """
    ],
    on_match: [
      type: :any,
      default: :ignore,
      doc: """
      instructions for handling records where a matching record existed in the relationship already
          * `:ignore`(default) - those inputs are ignored
          * `:update` - the record is updated using the destination's primary update action
          * `{:update, :action_name}` - the record is updated using the specified action on the destination resource
          * `{:update, :action_name, :join_table_action_name, [:list, :of, :params]}` - Same as `{:update, :action_name}` but takes
              the list of params specified out and applies them as an update to the join record (only valid for many to many).
          * `{:destroy, :action_name}` - the record is destroyed using the specified action on the destination resource. The action should be:
            * many_to_many - a destroy action on the join record
            * has_many - a destroy action on the destination resource
            * has_one - a destroy action on the destination resource
            * belongs_to - a destroy action on the destination resource
          * `:error`  - an eror is returned indicating that a record would have been updated
          * `:no_match` - ignores the primary key match and follows the on_no_match instructions with these records instead.
          * `:unrelate` - the related item is not destroyed, but the data is "unrelated", making this behave like `remove_from_relationship/3`. The action should be:
            * many_to_many - the join resource row is destroyed
            * has_many - the destination_attribute (on the related record) is set to `nil`
            * has_one - the destination_attribute (on the related record) is set to `nil`
            * belongs_to - the source_attribute (on this record) is set to `nil`
          * `{:unrelate, :action_name}` - the record is unrelated using the provided update action. The action should be:
            * many_to_many - a destroy action on the join resource
            * has_many - an update action on the destination resource
            * has_one - an update action on the destination resource
            * belongs_to - an update action on the source resource
      """
    ],
    on_missing: [
      type: :any,
      default: :ignore,
      doc: """
      instructions for handling records that existed in the current relationship but not in the input
          * `:ignore`(default) - those inputs are ignored
          * `:destroy` - the record is destroyed using the destination's primary destroy action
          * `{:destroy, :action_name}` - the record is destroyed using the specified action on the destination resource
          * `{:destroy, :action_name, :join_resource_action_name, [:join, :keys]}` - the record is destroyed using the specified action on the destination resource,
            but first the join resource is destroyed with its specified action
          * `:error`  - an error is returned indicating that a record would have been updated
          * `:unrelate` - the related item is not destroyed, but the data is "unrelated", making this behave like `remove_from_relationship/3`. The action should be:
            * many_to_many - the join resource row is destroyed
            * has_many - the destination_attribute (on the related record) is set to `nil`
            * has_one - the destination_attribute (on the related record) is set to `nil`
            * belongs_to - the source_attribute (on this record) is set to `nil`
          * `{:unrelate, :action_name}` - the record is unrelated using the provided update action. The action should be:
            * many_to_many - a destroy action on the join resource
            * has_many - an update action on the destination resource
            * has_one - an update action on the destination resource
            * belongs_to - an update action on the source resource
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
    meta: [
      type: :any,
      doc:
        "Freeform data that will be retained along with the options, which can be used to track/manage the changes that are added to the `relationships` key."
    ],
    ignore?: [
      type: :any,
      default: false,
      doc: """
      This tells Ash to ignore the provided inputs when actually running the action. This can be useful for
      building up a set of instructions that you intend to handle manually
      """
    ]
  ]

  @doc false
  def manage_relationship_schema, do: @manage_opts

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

  #{Spark.OptionsHelpers.docs(@manage_opts)}

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
          comment = MyApi.get(Comment, comment.id)

          comment
          |> Map.update(:rating, 0, &(&1 * result.rating_weight))
          |> MyApi.update!()
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
    |> Api.create!()
    |> Ash.Resource.put_metadata(:join_keys, %{type: "a"})

  post1 =
    changeset2
    |> Api.create!()
    |> Ash.Resource.put_metadata(:join_keys, %{type: "b"})

  author = Api.create!(author_changeset)

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
        Logger.warn(
          "type: :replace has been renamed to `:append_and_remove` in 2.0, and it will be removed in 2.1"
        )

        Keyword.put(opts, :type, :append_and_remove)
      else
        opts
      end

    inputs_was_list? = is_list(input)

    manage_opts =
      if opts[:type] do
        defaults = manage_relationship_opts(opts[:type])

        Enum.reduce(defaults, @manage_opts, fn {key, value}, manage_opts ->
          Spark.OptionsHelpers.set_default!(manage_opts, key, value)
        end)
      else
        @manage_opts
      end

    opts = Spark.OptionsHelpers.validate!(opts, manage_opts)

    opts =
      if Keyword.has_key?(opts[:meta] || [], :inputs_was_list?) do
        opts
      else
        Keyword.update(
          opts,
          :meta,
          [inputs_was_list?: inputs_was_list?],
          &Keyword.put(&1, :inputs_was_list?, inputs_was_list?)
        )
      end

    case Ash.Resource.Info.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      %{manual: manual} = relationship when not is_nil(manual) ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot manage a manual relationship"
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship when is_list(input) and length(input) > 1 ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot manage a #{type} relationship with a list of records"
          )

        add_error(changeset, error)

      relationship ->
        key =
          opts[:value_is_key] ||
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

              manage_relationship(changeset, relationship.name, input, opts)

            :error ->
              manage_relationship(changeset, relationship.name, List.wrap(input), opts)
          end
        else
          input =
            if key do
              input
              |> List.wrap()
              |> Enum.map(fn input ->
                if is_map(input) || is_list(input) do
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
               &(is_struct(&1) && &1.__struct__ != relationship.destination)
             ) do
            add_error(
              changeset,
              InvalidRelationship.exception(
                relationship: relationship.name,
                message: "Cannot provide structs that don't match the destination"
              )
            )
          else
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, [])
              |> Map.update!(relationship.name, &(&1 ++ [{input, opts}]))

            changeset = %{changeset | relationships: relationships}

            if opts[:eager_validate_with] do
              eager_validate_relationship_input(
                relationship,
                input,
                changeset,
                opts[:eager_validate_with],
                opts[:error_path] || opts[:meta][:id] || relationship.name,
                opts
              )
            else
              changeset
            end
          end
        end
    end
  end

  defp eager_validate_relationship_input(_relationship, [], _changeset, _api, _error_path, _opts),
    do: :ok

  defp eager_validate_relationship_input(relationship, input, changeset, api, error_path, opts) do
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

              Ash.Query.expr(
                ^this_filter and
                  (is_nil(ref(^relationship.destination_attribute, [])) or
                     ref(^relationship.destination_attribute, []) == ^destination_value)
              )
            else
              this_filter
            end
          end)

        if filter && filter != %{} do
          Ash.Query.expr(^expr or ^filter)
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
        |> Ash.Query.filter(^search)
        |> api.read()
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

        case Ash.Type.cast_input(attribute.type, input_value, attribute.name) do
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

  @doc """
  Appends a record or a list of records to a relationship.

  Alias for:
  ```elixir
  manage_relationship(changeset, relationship, input,
    on_lookup: :relate, # If a record is not in the relationship, and can be found, relate it
    on_no_match: :error, # If a record is not found in the relationship or the database, we error
    on_match: :ignore, # If a record is found in the relationship we don't change it
    on_missing: :ignore, # If a record is not found in the input, we ignore it
  )
  ```

  Provide `opts` to customize/override the behavior.
  """
  @spec append_to_relationship(
          t,
          atom,
          Ash.Resource.record() | map | term | [Ash.Resource.record() | map | term],
          Keyword.t()
        ) ::
          t()
  @deprecated "Use manage_relationship/4 instead"
  def append_to_relationship(changeset, relationship, record_or_records, opts \\ []) do
    manage_relationship(
      changeset,
      relationship,
      record_or_records,
      Keyword.merge(
        [
          on_lookup: :relate,
          on_no_match: :error,
          on_match: :ignore,
          on_missing: :ignore,
          authorize?: false
        ],
        opts
      )
    )
  end

  @doc """
  Removes a record or a list of records to a relationship.

  Alias for:
  ```elixir
    manage_relationship(changeset, relationship, record_or_records,
      on_no_match: :error, # If a record is not found in the relationship, we error
      on_match: :unrelate, # If a record is found in the relationship we unrelate it
      on_missing: :ignore, # If a record is not found in the relationship
      authorize?: false
    )
  ```
  """
  @deprecated "Use manage_relationship/4 instead"
  @spec remove_from_relationship(
          t,
          atom,
          Ash.Resource.record() | map | term | [Ash.Resource.record() | map | term],
          Keyword.t()
        ) ::
          t()
  def remove_from_relationship(changeset, relationship, record_or_records, opts \\ []) do
    manage_relationship(
      changeset,
      relationship,
      record_or_records,
      Keyword.merge(
        [
          on_no_match: :error,
          on_match: :unrelate,
          on_missing: :ignore,
          authorize?: false
        ],
        opts
      )
    )
  end

  @doc """
  Alias for:
  ```elixir
  manage_relationship(
    changeset,
    relationship,
    record_or_records,
    on_lookup: :relate, # If a record is not found in the relationship, but is found in the database, relate it and apply the input as an update
    on_no_match: :error, # If a record is not found in the relationship or the database, we error
    on_match: :ignore, # If a record is found in the relationship we make no changes to it
    on_missing: :unrelate, # If a record is not found in the relationship, we unrelate it
    authorize?: false
  )
  ```
  """
  @spec replace_relationship(
          t(),
          atom(),
          Ash.Resource.record() | map | term | [Ash.Resource.record() | map | term] | nil,
          Keyword.t()
        ) :: t()
  @deprecated "Use manage_relationship/4 instead"
  def replace_relationship(changeset, relationship, record_or_records, opts \\ []) do
    manage_relationship(
      changeset,
      relationship,
      record_or_records,
      Keyword.put(opts, :type, :append_and_remove)
    )
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
    Map.has_key?(changeset.attributes, attribute)
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
  Change an attribute if is not currently being changed, by calling the provided function

  Use this if you want to only perform some expensive calculation for an attribute value
  only if there isn't already a change for that attribute
  """
  @spec change_new_attribute_lazy(t(), atom, (() -> any)) :: t()
  def change_new_attribute_lazy(changeset, attribute, func) do
    maybe_already_validated_error!(changeset, :force_change_new_attribute_lazy)

    if changing_attribute?(changeset, attribute) do
      changeset
    else
      change_attribute(changeset, attribute, func.())
    end
  end

  @doc """
  Add an argument to the changeset, which will be provided to the action
  """
  def set_argument(changeset, argument, value) do
    maybe_already_validated_error!(changeset, :set_argument)

    if changeset.action do
      argument =
        Enum.find(
          changeset.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      if argument do
        with {:ok, casted} <- cast_input(argument.type, value, argument.constraints, changeset),
             {:constrained, {:ok, casted}, argument} when not is_nil(casted) <-
               {:constrained,
                Ash.Type.apply_constraints(argument.type, casted, argument.constraints),
                argument} do
          %{changeset | arguments: Map.put(changeset.arguments, argument.name, casted)}
        else
          {:constrained, {:ok, nil}, _argument} ->
            %{changeset | arguments: Map.put(changeset.arguments, argument.name, nil)}

          {:constrained, {:error, error}, argument} ->
            changeset = %{
              changeset
              | arguments: Map.put(changeset.arguments, argument.name, value)
            }

            add_invalid_errors(:argument, changeset, argument, error)

          {:error, error} ->
            changeset = %{
              changeset
              | arguments: Map.put(changeset.arguments, argument.name, value)
            }

            add_invalid_errors(:argument, changeset, argument, error)
        end
      else
        %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
      end
    else
      %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
    end
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
  Merge a map of arguments to the arguments list
  """
  def set_arguments(changeset, map) do
    maybe_already_validated_error!(changeset)

    Enum.reduce(map, changeset, fn {key, value}, changeset ->
      set_argument(changeset, key, value)
    end)
  end

  @doc """
  Force change an attribute if it is not currently being changed

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
  Force change an attribute if it is not currently being changed, by calling the provided function

  See `change_new_attribute_lazy/3` for more.
  """
  @spec force_change_new_attribute_lazy(t(), atom, (() -> any)) :: t()
  def force_change_new_attribute_lazy(changeset, attribute, func) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      force_change_attribute(changeset, attribute, func.())
    end
  end

  @doc "Calls `change_attribute/3` for each key/value pair provided"
  @spec change_attributes(t(), map | Keyword.t()) :: t()
  def change_attributes(changeset, changes) do
    maybe_already_validated_error!(changeset, :force_change_attributes)

    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      change_attribute(changeset, key, value)
    end)
  end

  @doc "Adds a change to the changeset, unless the value matches the existing value"
  @spec change_attribute(t(), atom, any) :: t()
  def change_attribute(changeset, attribute, value) do
    maybe_already_validated_error!(changeset, :change_attribute)

    case Ash.Resource.Info.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            name: attribute
          )

        add_error(changeset, error)

      %{writable?: false} = attribute ->
        changeset = %{
          changeset
          | attributes: Map.put(changeset.attributes, attribute.name, value)
        }

        add_invalid_errors(:attribute, changeset, attribute, "Attribute is not writable")

      attribute ->
        with value <- handle_indexed_maps(attribute.type, value),
             {:ok, prepared} <-
               prepare_change(changeset, attribute, value, attribute.constraints),
             {:ok, casted} <-
               cast_input(attribute.type, prepared, attribute.constraints, changeset, true),
             {:ok, casted} <-
               handle_change(changeset, attribute, casted, attribute.constraints),
             {:ok, casted} <-
               Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)
          changeset = remove_default(changeset, attribute.name)

          cond do
            changeset.action_type == :create ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }

            is_nil(data_value) and is_nil(casted) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }

            true ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }
          end
        else
          {{:error, error_or_errors}, last_val} ->
            changeset = %{
              changeset
              | attributes: Map.put(changeset.attributes, attribute.name, last_val),
                defaults: changeset.defaults -- [attribute.name]
            }

            add_invalid_errors(:attribute, changeset, attribute, error_or_errors)

          :error ->
            changeset = %{
              changeset
              | attributes: Map.put(changeset.attributes, attribute.name, value),
                defaults: changeset.defaults -- [attribute.name]
            }

            add_invalid_errors(:attribute, changeset, attribute)

          {:error, error_or_errors} ->
            changeset = %{
              changeset
              | attributes: Map.put(changeset.attributes, attribute.name, value),
                defaults: changeset.defaults -- [attribute.name]
            }

            add_invalid_errors(:attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  @doc """
  The same as `change_attribute`, but annotates that the attribute is currently holding a default value.

  This information can be used in changes to see if a value was explicitly set or if it was set by being the default.
  Additionally, this is used in `upsert` actions to not overwrite existing values with the default
  """
  @spec change_default_attribute(t(), atom, any) :: t()
  def change_default_attribute(changeset, attribute, value) do
    maybe_already_validated_error!(changeset)

    case Ash.Resource.Info.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            name: attribute
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

  @doc false
  def cast_input(type, term, constraints, changeset, return_value? \\ false)

  def cast_input(type, value, constraints, changeset, return_value?) do
    value = handle_indexed_maps(type, value)
    constraints = Ash.Type.constraints(changeset, type, constraints)

    case Ash.Type.cast_input(type, value, constraints) do
      {:ok, value} ->
        {:ok, value}

      {:error, error} ->
        if return_value? do
          {{:error, error}, value}
        else
          {:error, error}
        end
    end
  end

  @doc false
  def handle_indexed_maps({:array, type}, term) when is_map(term) and term != %{} do
    term
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
        value
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(&elem(&1, 1))
        |> Enum.map(&handle_indexed_maps(type, &1))

      :error ->
        term
    end
  end

  def handle_indexed_maps(_, value), do: value

  @doc "Calls `force_change_attribute/3` for each key/value pair provided"
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
            name: attribute
          )

        add_error(changeset, error)

      attribute when is_nil(value) ->
        %{changeset | attributes: Map.put(changeset.attributes, attribute.name, nil)}

      attribute ->
        with value <- handle_indexed_maps(attribute.type, value),
             {:ok, prepared} <-
               prepare_change(changeset, attribute, value, attribute.constraints),
             {:ok, casted} <-
               cast_input(attribute.type, prepared, attribute.constraints, changeset),
             {:ok, casted} <- handle_change(changeset, attribute, casted, attribute.constraints),
             {:ok, casted} <-
               Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)

          changeset = remove_default(changeset, attribute.name)

          cond do
            changeset.action_type == :create ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }

            is_nil(data_value) and is_nil(casted) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              %{
                changeset
                | attributes: Map.delete(changeset.attributes, attribute.name),
                  defaults: changeset.defaults -- [attribute.name]
              }

            true ->
              %{
                changeset
                | attributes: Map.put(changeset.attributes, attribute.name, casted),
                  defaults: changeset.defaults -- [attribute.name]
              }
          end
        else
          :error ->
            changeset = %{
              changeset
              | attributes: Map.put(changeset.attributes, attribute.name, value)
            }

            add_invalid_errors(:attribute, changeset, attribute)

          {:error, error_or_errors} ->
            changeset = %{
              changeset
              | attributes: Map.put(changeset.attributes, attribute.name, value)
            }

            add_invalid_errors(:attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  @doc """
  Adds a before_action hook to the changeset.

  Provide the option `append?: true` to place the hook after all
  other hooks instead of before.
  """
  @spec before_action(
          t(),
          (t() -> t() | {t(), %{notifications: list(Ash.Notifier.Notification.t())}}),
          Keyword.t()
        ) ::
          t()
  def before_action(changeset, func, opts \\ []) do
    if opts[:append?] do
      %{changeset | before_action: changeset.before_action ++ [func]}
    else
      %{changeset | before_action: [func | changeset.before_action]}
    end
  end

  @doc """
  Adds an after_action hook to the changeset.


  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
  """
  @spec after_action(
          t(),
          (t(), Ash.Resource.record() ->
             {:ok, Ash.Resource.record()}
             | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
             | {:error, term}),
          Keyword.t()
        ) :: t()
  def after_action(changeset, func, opts \\ []) do
    if opts[:prepend?] do
      %{changeset | after_action: [func | changeset.after_action]}
    else
      %{changeset | after_action: changeset.after_action ++ [func]}
    end
  end

  @doc """
  Adds an around_action hook to the changeset.

  Your function will get the changeset, and a callback that must be called with a changeset (that may be modified).
  The callback will return `{:ok, result, instructions}` or `{:error, error}`. You can modify these values, but the
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
  end)
  |> Ash.Changeset.around_action(fn changeset, callback ->
    IO.puts("second around: before")
    result = callback.(changeset)
    IO.puts("second around: after")
  end)
  |> Ash.Changeset.before_action(fn changeset ->
    IO.puts("first before")
    changeset
  end)
  |> Ash.Changeset.before_action(fn changeset ->
    IO.puts("second before")
    changeset
  end)
  |> Ash.Changeset.after_action(fn changeset, result ->
    IO.puts("first after")
    {:ok, result}
  end)
  |> Ash.Changeset.after_action(fn changeset ->
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
  @type around_result ::
          {:ok, Ash.Resource.record(), t(), %{notifications: list(Ash.Notifier.Notification.t())}}
          | {:error, Ash.Error.t()}

  @type around_callback :: (t() -> around_result)
  @spec around_action(t(), (t(), around_callback() -> around_result)) :: t()
  def around_action(changeset, func) do
    %{changeset | around_action: changeset.around_action ++ [func]}
  end

  @doc """
  Returns the original data with attribute changes merged, if the changeset is valid.

  Options:

  * force? - applies current attributes even if the changeset is not valid
  """
  @spec apply_attributes(t(), opts :: Keyword.t()) :: {:ok, Ash.Resource.record()} | {:error, t()}
  def apply_attributes(changeset, opts \\ [])

  def apply_attributes(%{valid?: true} = changeset, _opts) do
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

  @doc "Clears an attribute or relationship change off of the changeset"
  def clear_change(changeset, field) do
    cond do
      attr = Ash.Resource.Info.attribute(changeset.resource, field) ->
        %{changeset | attributes: Map.delete(changeset.attributes, attr.name)}

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

  @doc "Adds an error to the changesets errors list, and marks the change as `valid?: false`"
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
          vars: keyword
        )
      else
        InvalidChanges.exception(
          fields: keyword[:fields] || [],
          message: keyword[:message],
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

  defp add_invalid_errors(type, changeset, attribute, message \\ nil) do
    messages =
      if Keyword.keyword?(message) do
        [message]
      else
        List.wrap(message)
      end

    Enum.reduce(messages, changeset, fn message, changeset ->
      opts = error_to_exception_opts(message, attribute)

      exception =
        case type do
          :attribute -> InvalidAttribute
          :argument -> InvalidArgument
        end

      Enum.reduce(opts, changeset, fn opts, changeset ->
        error =
          exception.exception(
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
    end)
  end

  @doc false
  def error_to_exception_opts(message, attribute) do
    case message do
      keyword when is_list(keyword) ->
        fields =
          case List.wrap(keyword[:fields]) do
            [] ->
              List.wrap(keyword[:field])

            fields ->
              fields
          end

        fields
        |> case do
          [] ->
            [
              keyword
              |> Keyword.put(
                :message,
                add_index(keyword[:message], keyword)
              )
              |> Keyword.put(:field, attribute.name)
            ]

          fields ->
            Enum.map(
              fields,
              &Keyword.merge(message,
                field: attribute.name,
                message: add_index(add_field(keyword[:message], "#{&1}"), keyword)
              )
            )
        end

      message when is_binary(message) ->
        [[field: attribute.name, message: message]]

      _ ->
        [[field: attribute.name]]
    end
  end

  defp add_field(message, field) do
    "at field #{field} " <> (message || "")
  end

  defp add_index(message, opts) do
    if opts[:index] do
      "at index #{opts[:index]} " <> (message || "")
    else
      message
    end
  end
end

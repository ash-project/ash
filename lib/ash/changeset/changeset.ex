defmodule Ash.Changeset do
  @moduledoc """
  Changesets are used to create and update data in Ash.

  Create a changeset with `new/1` or `new/2`, and alter the attributes
  and relationships using the functions provided in this module.  Nothing in this module
  actually incurs changes in a data layer. To commit a changeset, see `c:Ash.Api.create/2`
  and `c:Ash.Api.update/2`.

  ## Primary Keys

  For relationship manipulation using `append_to_relationship/3`, `remove_from_relationship/3`
  and `replace_relationship/3` there are three types that can be used for primary keys:

  1.) An instance of the resource in question.

  2.) If the primary key is just a single field, i.e `:id`, then a single value, i.e `1`

  3.) A map of keys to values representing the primary key, i.e `%{id: 1}` or `%{id: 1, org_id: 2}`

  ## Join Attributes

  For many to many relationships, the attributes on a join relationship may be set while relating items
  by passing a tuple of the primary key and the changes to be applied. This is done via upserts, so
  update validations on the join resource are *not* applied, but create validations are.

  For example:

  ```elixir
  Ash.Changeset.replace_relationship(changeset, :linked_tickets, [
    {1, %{link_type: "blocking"}},
    {a_ticket, %{link_type: "caused_by"}},
    {%{id: 2}, %{link_type: "related_to"}}
  ])
  ```

  ## Manage relationship vs append/replace/remove

  ### Manage relationship
  `Ash.Changeset.manage_relationship/4` is for creating/updating/destroying related items. A simple example
  is for creating a comment and adding it to a post.

  ```elixir
  post
  |> Ash.Changeset.manage_relationship(
    :comments,
    [%{body: "this post is great!"}],
    on_destroy: :ignore,
    on_update: :error
    )
  |> MyApp.MyApi.update!(actor: current_user)
  ```

  We configured it to ignore any "destroys", meaning "don't worry about the comments that are related but not in this list."
  We also configured it to error on updates, meaning "this shouldn't change any existing comments"
  We left `on_create` as the default, which will call the primary create action on the destination.

  User input should not be passed directly into this function. See `manage_relationship/4` for more.

  *By default, these changes on the destination resources follow the authorization rules, if any, on that resource*

  ### Append/Replace/Remove

  `Ash.Changeset.replace_relationship/3`, `Ash.Changeset.append_to_relationship/3` and `Ash.Changeset.remove_from_relationship/3`
  are simply about managing what data is/isn't related. A simple example might be updating the *tags* of a post, where all the tags
  already exist, we simply want to edit the information.

  *These changes on the destination resources *do not* follow authorization rules of the destination resource.
  For example, updating a `has_many` relationship could involve changing the destination field to point at a different record.*

  User input should not be passed directly into this function. See `manage_relationship/4` for more.

  Instead add an `append_to_relationship`, `remove_from_relationship` or `replace_relationship` to the action itself.

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
    params: %{},
    action_failed?: false,
    arguments: %{},
    context: %{},
    after_action: [],
    before_action: [],
    errors: [],
    valid?: true,
    attributes: %{},
    relationships: %{},
    change_dependencies: [],
    requests: []
  ]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(changeset, opts) do
      context =
        if changeset.context == %{} do
          empty()
        else
          concat("context: ", to_doc(changeset.context, opts))
        end

      container_doc(
        "#Ash.Changeset<",
        [
          concat("action_type: ", inspect(changeset.action_type)),
          concat("action: ", inspect(changeset.action && changeset.action.name)),
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
        action = Ash.Resource.action(changeset.resource, changeset.action, changeset.action_type)

        if is_nil(action) || Enum.empty?(action.arguments) do
          empty()
        else
          arg_string =
            action.arguments
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

  @type t :: %__MODULE__{}

  alias Ash.Error.{
    Changes.InvalidArgument,
    Changes.InvalidAttribute,
    Changes.InvalidChanges,
    Changes.InvalidRelationship,
    Changes.NoSuchAttribute,
    Changes.NoSuchRelationship,
    Changes.Required,
    Invalid.NoSuchAction,
    Invalid.NoSuchResource,
    Query.NoReadAction
  }

  @doc """
  Return a changeset over a resource or a record. `params` can be either attributes, relationship values or arguments.

  If you are using external input, you almost certainly want to use `Ash.Changeset.for_<action_type>`. However, you can
  use `Ash.Changeset.new/2` to start a changeset and make a few changes prior to calling `for_action`. For example:


  ```elixir
  Ash.Changeset.new()
  |> Ash.Changeset.change_attribute(:name, "foobar")
  |> Ash.Changeset.for_action(...)
  ```

  Anything that is modified prior to `for_action` is validated against the rules of the action, while *anything after it is not*.

  This changeset does not consider an action, and so allows you to change things with minimal validation. Values are
  validated when changed, and the existence of attributes and relationships are validated. If you want to essentially
  "run an action", and get back a changeset with any errors that would be generated by that action (with the exception
  of errors that can only be generated by the data layer), use `for_action/2`.

  Additionally, this format only supports supplying attributes in the params. This is because we don't know what the
  behavior should be for relationship changes, nor what arguments are available. You can manage them yourself with
  the functions that allow managing arguments/relationships that are provided in this module, e.g `set_argument/3` and
  `replace_relationship/3`
  """
  @spec new(Ash.resource() | Ash.record(), params :: map) :: t
  def new(resource, params \\ %{})

  def new(%resource{} = record, params) do
    tenant =
      record
      |> Map.get(:__metadata__, %{})
      |> Map.get(:tenant, nil)

    context = Ash.Resource.default_context(resource) || %{}

    if Ash.Resource.resource?(resource) do
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
    if Ash.Resource.resource?(resource) do
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
  Constructs a changeset for a given create action, and validates it.

  Anything that is modified prior to `for_create/4` is validated against the rules of the action, while *anything after it is not*.

  ### Params
  `params` may be attributes, relationships, or arguments. You can safely pass user/form input directly into this function.
  Only public attributes and relationships are supported. If you want to change private attributes as well, see the
  Customization section below. `params` are stored directly as given in the `params` field of the changeset, which is used

  ### Opts

  * `:relationships` - customize relationship behavior. See the Relationships section below.
  * `:actor` - set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)

  ### Relationships

  By default, any relationships are *replaced* via `replace_relationship`. To change this behavior, provide the
  `relationships` option.

  For example:

      Ash.Changeset.for_create(MyResource, :create, params, relationships: [relationship: :append, other_relationship: :remove])

  ### Customization

  A changeset can be provided as the first argument, instead of a resource, to allow
  setting specific attributes ahead of time.

  For example:

      MyResource
      |> Changeset.change_attribute(:foo, 1)
      |> Changeset.for_create(:create, ...opts)

  Once a changeset has been validated by `for_create/4` (or `for_update/4`), it isn't validated again in the action.
  New changes added are validated individually, though. This allows you to create a changeset according
  to a given action, and then add custom changes if necessary.
  """
  def for_create(initial, action, params, opts \\ []) do
    changeset =
      case initial do
        %__MODULE__{action_type: :create} = changeset ->
          changeset

        %__MODULE__{} = changeset ->
          add_error(
            changeset,
            "Initial changeset provided with invalid action type: #{changeset.action_type}"
          )

        resource when is_atom(resource) ->
          new(resource)

        other ->
          %__MODULE__{resource: other, action_type: :create}
          |> add_error(NoSuchResource.exception(resource: other))
      end

    for_action(changeset, action, params, opts)
  end

  @doc """
  Constructs a changeset for a given update action, and validates it.

  Anything that is modified prior to `for_update/4` is validated against the rules of the action, while *anything after it is not*.

  See `for_create/4` for more information
  """
  def for_update(initial, action, params, opts \\ []) do
    changeset =
      case initial do
        # We accept :destroy here to support soft deletes
        %__MODULE__{action_type: type} = changeset when type in [:update, :destroy] ->
          changeset

        %__MODULE__{} = changeset ->
          add_error(
            changeset,
            "Initial changeset provided with invalid action type: #{changeset.action_type}"
          )

        %_{} = struct ->
          new(struct)

        _other ->
          %__MODULE__{resource: nil, action_type: :update}
          |> add_error(NoSuchResource.exception(resource: nil))
      end

    for_action(changeset, action, params, opts)
  end

  @doc """
  Constructs a changeset for a given destroy action, and validates it.

  Pass an `actor` option to specify the actor

  Anything that is modified prior to `for_destroy/4` is validated against the rules of the action, while *anything after it is not*.

  Once a changeset has been validated by `for_destroy/4`, it isn't validated again in the action.
  New changes added are validated individually, though. This allows you to create a changeset according
  to a given action, and then add custom changes if necessary.
  """
  def for_destroy(initial, action_name, params, opts \\ []) do
    changeset =
      case initial do
        %__MODULE__{} = changeset ->
          changeset
          |> Map.put(:action_type, :destroy)

        %_{} = struct ->
          struct
          |> new()
          |> Map.put(:action_type, :destroy)

        _other ->
          %__MODULE__{resource: nil, action_type: :destroy}
          |> add_error(NoSuchResource.exception(resource: nil))
      end

    if changeset.valid? do
      action = Ash.Resource.action(changeset.resource, action_name, changeset.action_type)

      if action do
        changeset
        |> cast_params(action, params, opts)
        |> Map.put(:action, action)
        |> Map.put(:__validated_for_action__, action.name)
        |> cast_arguments(action)
        |> add_validations()
        |> validate_multitenancy()
        |> mark_validated(action.name)
      else
        add_error(
          changeset,
          NoSuchAction.exception(
            resource: changeset.resource,
            action: action_name,
            type: :destroy
          )
        )
      end
    else
      changeset
    end
  end

  defp for_action(changeset, action, params, opts) do
    if changeset.valid? do
      action = Ash.Resource.action(changeset.resource, action, changeset.action_type)

      if action do
        changeset
        |> cast_params(action, params || %{}, opts)
        |> cast_arguments(action)
        |> Map.put(:action, action)
        |> Map.put(:__validated_for_action__, action.name)
        |> validate_attributes_accepted(action)
        |> validate_relationships_accepted(action)
        |> run_action_changes(action, opts[:actor])
        |> set_defaults(changeset.action_type)
        |> validate_required_belongs_to()
        |> add_validations()
        |> require_values(changeset.action_type)
        |> validate_multitenancy()
        |> mark_validated(action.name)
      else
        add_error(
          changeset,
          NoSuchAction.exception(
            resource: changeset.resource,
            action: action,
            type: changeset.action_type
          )
        )
      end
    else
      changeset
    end
  end

  defp mark_validated(changeset, action_name) do
    %{changeset | __validated_for_action__: action_name}
  end

  defp validate_multitenancy(changeset) do
    if Ash.Resource.multitenancy_strategy(changeset.resource) &&
         not Ash.Resource.multitenancy_global?(changeset.resource) && is_nil(changeset.tenant) do
      add_error(
        changeset,
        "#{inspect(changeset.resource)} changesets require a tenant to be specified"
      )
    else
      changeset
    end
  end

  defp cast_params(changeset, action, params, opts) do
    changeset = %{changeset | params: Enum.into(params, %{})}

    Enum.reduce(params, changeset, fn {name, value}, changeset ->
      cond do
        has_argument?(action, name) ->
          set_argument(changeset, name, value)

        attr = Ash.Resource.public_attribute(changeset.resource, name) ->
          if attr.writable? do
            change_attribute(changeset, attr.name, value)
          else
            changeset
          end

        rel = Ash.Resource.public_relationship(changeset.resource, name) ->
          if rel.writable? do
            behaviour = opts[:relationships][rel.name] || :replace

            case behaviour do
              :replace ->
                replace_relationship(changeset, rel.name, value)

              :append ->
                append_to_relationship(changeset, rel.name, value)

              :remove ->
                append_to_relationship(changeset, rel.name, value)
            end
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

  defp validate_relationships_accepted(changeset, %{accept: nil}), do: changeset

  defp validate_relationships_accepted(changeset, %{accept: accepted_relationships}) do
    changeset.relationships
    |> Enum.reject(fn {key, _value} ->
      key in accepted_relationships
    end)
    |> Enum.reduce(changeset, fn {key, _}, changeset ->
      add_error(
        changeset,
        InvalidRelationship.exception(
          relationship: key,
          message: "Cannot be changed"
        )
      )
    end)
  end

  defp run_action_changes(changeset, %{changes: changes}, actor) do
    Enum.reduce(changes, changeset, fn
      %{change: {module, opts}}, changeset ->
        module.change(changeset, opts, %{actor: actor})

      %{validation: _} = validation, changeset ->
        if validation.expensive? and not changeset.valid? do
          changeset
        else
          do_validation(changeset, validation)
        end
    end)
  end

  defp set_defaults(changeset, :create) do
    changeset.resource
    |> Ash.Resource.attributes()
    |> Enum.filter(&(not is_nil(&1.default)))
    |> Enum.reduce(changeset, fn attribute, changeset ->
      force_change_new_attribute_lazy(changeset, attribute.name, fn ->
        default(:create, attribute)
      end)
    end)
  end

  defp set_defaults(changeset, :update) do
    changeset.resource
    |> Ash.Resource.attributes()
    |> Enum.filter(&(not is_nil(&1.update_default)))
    |> Enum.reduce(changeset, fn attribute, changeset ->
      force_change_new_attribute_lazy(changeset, attribute.name, fn ->
        default(:update, attribute)
      end)
    end)
  end

  defp set_defaults(changeset, _) do
    changeset
  end

  defp default(:create, %{default: {mod, func, args}}), do: apply(mod, func, args)
  defp default(:create, %{default: function}) when is_function(function, 0), do: function.()
  defp default(:create, %{default: value}), do: value

  defp default(:update, %{update_default: {mod, func, args}}), do: apply(mod, func, args)

  defp default(:update, %{update_default: function}) when is_function(function, 0),
    do: function.()

  defp default(:update, %{update_default: value}), do: value

  defp validate_required_belongs_to(changeset) do
    changeset.resource
    |> Ash.Resource.relationships()
    |> Enum.filter(&(&1.type == :belongs_to))
    |> Enum.filter(& &1.required?)
    |> Enum.reduce(changeset, fn required_relationship, changeset ->
      case Map.fetch(changeset.relationships, required_relationship.name) do
        {:ok, %{add: adding}} when adding != nil and adding != [] ->
          changeset

        {:ok, %{replace: replacing}} when replacing != nil and replacing != [] ->
          changeset

        _ ->
          case Map.fetch(changeset.attributes, required_relationship.source_field) do
            {:ok, value} when not is_nil(value) ->
              changeset

            _ ->
              add_error(
                changeset,
                Required.exception(
                  field: required_relationship.name,
                  type: :relationship
                )
              )
          end
      end
    end)
  end

  defp add_validations(changeset) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      changeset.resource
      # We use the `changeset.action_type` to support soft deletes
      # Because a delete is an `update` with an action type of `update`
      |> Ash.Resource.validations(changeset.action_type)
      |> Enum.reduce(changeset, fn validation, changeset ->
        if validation.expensive? and not changeset.valid? do
          changeset
        else
          do_validation(changeset, validation)
        end
      end)
    end)
  end

  defp do_validation(changeset, validation) do
    case validation.module.validate(changeset, validation.opts) do
      :ok ->
        changeset

      {:error, error} when is_binary(error) ->
        Ash.Changeset.add_error(changeset, validation.message || error)

      {:error, error} when is_exception(error) ->
        if validation.message do
          error =
            case error do
              %{field: field} when not is_nil(field) ->
                error
                |> Map.take([:field, :vars])
                |> Map.to_list()
                |> Keyword.put(:message, validation.message)
                |> InvalidAttribute.exception()

              %{fields: fields} when fields not in [nil, []] ->
                error
                |> Map.take([:fields, :vars])
                |> Map.to_list()
                |> Keyword.put(:message, validation.message)
                |> InvalidChanges.exception()

              _ ->
                validation.message
            end

          Ash.Changeset.add_error(changeset, error)
        else
          Ash.Changeset.add_error(changeset, error)
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

  defp require_values(changeset, :create) do
    changeset.resource
    |> Ash.Resource.attributes()
    |> Enum.reject(&(&1.allow_nil? || &1.private? || &1.generated?))
    |> Enum.reduce(changeset, fn required_attribute, changeset ->
      if Ash.Changeset.changing_attribute?(changeset, required_attribute.name) do
        changeset
      else
        Ash.Changeset.add_error(
          changeset,
          Required.exception(field: required_attribute.name, type: :attribute)
        )
      end
    end)
  end

  defp require_values(changeset, _), do: changeset

  @doc """
  Wraps a function in the before/after action hooks of a changeset.

  The function takes a changeset and if it returns
  `{:ok, result}`, the result will be passed through the after
  action hooks.
  """
  @spec with_hooks(
          t(),
          (t() ->
             {:ok, Ash.record(), %{notifications: list(Ash.notification())}} | {:error, term})
        ) ::
          {:ok, term, t(), %{notifications: list(Ash.notification())}} | {:error, term}
  def with_hooks(changeset, func) do
    {changeset, %{notifications: before_action_notifications}} =
      Enum.reduce_while(
        changeset.before_action,
        {changeset, %{notifications: []}},
        fn before_action, {changeset, instructions} ->
          case before_action.(changeset) do
            {%{valid?: true} = changeset, %{notifications: notifications}} ->
              {:cont,
               {changeset,
                %{
                  instructions
                  | notifications: instructions.notifications ++ List.wrap(notifications)
                }}}

            %{valid?: true} = changeset ->
              {:cont, {changeset, instructions}}

            changeset ->
              {:halt, {changeset, instructions}}
          end
        end
      )

    if changeset.valid? do
      case func.(changeset) do
        {:ok, result} ->
          run_after_actions(result, changeset, before_action_notifications)

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, changeset.errors}
    end
  end

  defp run_after_actions(result, changeset, before_action_notifications) do
    Enum.reduce_while(
      changeset.after_action,
      {:ok, result, changeset, %{notifications: before_action_notifications}},
      fn after_action, {:ok, result, changeset, %{notifications: notifications} = acc} ->
        case after_action.(changeset, result) do
          {:ok, new_result, new_notifications} ->
            all_notifications =
              Enum.map(notifications ++ new_notifications, fn notification ->
                %{
                  notification
                  | resource: notification.resource || changeset.resource,
                    action:
                      notification.action ||
                        Ash.Resource.action(
                          changeset.resource,
                          changeset.action,
                          changeset.action_type
                        ),
                    data: notification.data || new_result,
                    changeset: notification.changeset || changeset,
                    actor: notification.actor || Map.get(changeset.context, :actor)
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
    Map.get(changeset.arguments, argument) || Map.get(changeset.arguments, to_string(argument))
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

  @doc "Gets the new value for an attribute, or `:error` if it is not being changed"
  @spec fetch_change(t, atom) :: {:ok, any} | :error
  def fetch_change(changeset, attribute) do
    Map.fetch(changeset.attributes, attribute)
  end

  @doc "Gets the original value for an attribute"
  @spec get_data(t, atom) :: {:ok, any} | :error
  def get_data(changeset, attribute) do
    Map.get(changeset.data, attribute)
  end

  @spec put_context(t(), atom, term) :: t()
  def put_context(changeset, key, value) do
    %{changeset | context: Map.put(changeset.context, key, value)}
  end

  @spec set_tenant(t(), String.t()) :: t()
  def set_tenant(changeset, tenant) do
    %{changeset | tenant: tenant}
  end

  @spec set_context(t(), map | nil) :: t()
  def set_context(changeset, nil), do: changeset

  def set_context(changeset, map) do
    %{changeset | context: Map.merge(changeset.context, map)}
  end

  defp cast_arguments(changeset, action) do
    Enum.reduce(action.arguments, %{changeset | arguments: %{}}, fn argument, new_changeset ->
      value = get_argument(changeset, argument.name) || argument_default(argument.default)

      if is_nil(value) && !argument.allow_nil? do
        Ash.Changeset.add_error(
          changeset,
          Required.exception(field: argument.name, type: :argument)
        )
      else
        val =
          case fetch_argument(changeset, argument.name) do
            :error ->
              if argument.default do
                {:ok, argument_default(argument.default)}
              else
                :error
              end

            {:ok, val} ->
              {:ok, val}
          end

        with {:found, {:ok, value}} <- {:found, val},
             {:ok, casted} <- Ash.Type.cast_input(argument.type, value),
             {:ok, casted} <-
               Ash.Type.apply_constraints(argument.type, casted, argument.constraints) do
          %{new_changeset | arguments: Map.put(new_changeset.arguments, argument.name, casted)}
        else
          {:error, error} ->
            add_invalid_errors(:argument, changeset, argument, error)

          {:found, :error} ->
            changeset
        end
      end
    end)
  end

  defp argument_default(value) when is_function(value, 0), do: value.()
  defp argument_default(value), do: value

  @manage_opts [
    authorize?: [
      type: :boolean,
      default: true,
      doc:
        "Authorize changes to the destination records, if the primary change is being authorized as well."
    ],
    on_create: [
      type: :any,
      default: :create,
      doc: """
      instructions for handling records where no matching record existed in the relationship
          * `:create`(default) - the records are created using the destination's primary create action
          * `{:create, :action_name}` - the records are created using the specified action on the destination resource
          * `{:create, :action_name, :join_table_action_name, [:list, :of, :join_table, :params]}` - Same as `{:update, :action_name}` but takes
              the list of params specified out and applies them when creating the join table row.
          * `:ignore` - those inputs are ignored
          * `:error`  - an eror is returned indicating that a record would have been created
      """
    ],
    on_update: [
      type: :any,
      default: :update,
      doc: """
      instructions for handling records where a matching record existed in the relationship already
          * `:update`(default) - the record is updated using the destination's primary update action
          * `{:update, :action_name}` - the record is updated using the specified action on the destination resource
          * `{:update, :action_name, :join_table_action_name, [:list, :of, :params]}` - Same as `{:update, :action_name}` but takes
              the list of params specified out and applies them as an update to the join table row (only valid for many to many).
          * `:ignore` - those inputs are ignored
          * `:error`  - an eror is returned indicating that a record would have been updated
          * `:create` - ignores the primary key match and follows the create instructions with these records instead.
          * `:destroy` - follows the destroy instructions for any records with matching primary keys
      """
    ],
    on_destroy: [
      type: :any,
      default: :destroy,
      doc: """
      instructions for handling records that existed in the current relationship but not in the input
          * `:destroy`(default) - the record is destroyed using the destination's primary destroy action
          * `{:destroy, :action_name}` - the record is destroyed using the specified action on the destination resource
          * `{:destroy, :action_name, :join_resource_action_name}` - the record is destroyed using the specified action on the destination resource,
            but first the join resource is destroyed with its specified action
          * `:ignore` - those inputs are ignored
          * `:error`  - an error is returned indicating that a record would have been updated
          * `:unrelate` - the related item is not destroyed, but the data is "unrelated", making this behave like `remove_from_relationship/3`
            * many_to_many - the join resource row is destroyed
            * has_many - the destination_field (on the related record) is set to `nil`
            * has_one - the destination_field (on the related record) is set to `nil`
            * belongs_to - the source_field (on this record) is set to `nil`
          * `{:unrelate, :action_name}` - the record is unrelated using the provided update action.
            * many_to_many - a destroy action on the join resource
            * has_many - an update action on the destination resource
            * has_one - an update action on the destination resource
            * belongs_to - an update action on the source resource
      """
    ]
  ]

  @doc false
  def manage_relationship_schema, do: @manage_opts

  @doc """
  Manages the related records by creating, updating, or destroying them as necessary.

  Generally speaking, this function should not be used with user input. If you want to accept user
  input to manage a relationship, e.g via a form, api, or controller input, instead add a `managed_relationship`
  to your action. See the DSL documentation for more on that

  Unlike `append_to_relationship/4`, `replace_relationship/3` and `remove_from_relationship/3`,
  this will actually make changes to the non-relationship fields of the destination resource.
  For the  other functions, the only authorization is involved is the authorization on this resource,
  however `manage_relationship/4` will authorization/validate each individual operation.

  If you want the input to update existing entities (when the `type` is `:replace`, the default),
  you need to ensure that the primary key is provided as part of the input. See the example below:

      Changeset.manage_relationship(
        changeset,
        :comments,
        [%{rating: 10, contents: "foo"}],
        on_create: {:create, :create_action},
        on_destroy: :ignore
      )
      Changeset.manage_relationship(
        changeset,
        :comments,
        [%{id: 10, rating: 10, contents: "foo"}],
        on_update: {:update, :update_action},
        on_create: {:create, :create_action})

  ## Options

  #{Ash.OptionsHelpers.docs(@manage_opts)}

  ### Mixing with other relationship functions

  If mixed with `append_to_relationship/3`, `remove_from_relationship/3` and `replace_relationship/3`, those actions will
  happen first. After all of those changes have been made, the relationship will be "managed" according to the options provided
  to this.

      %Post{}
      |> Ash.Changeset.new()
      |> Ash.Changeset.manage_relationship(:comments, [%{text: "bar"}])
      |> Ash.Changeset.append_to_relationship(:comments, [%{text: "foo"}])
      |> Api.update!()
      # %Post{comments: [%Comment{text: "bar"}, %Comment{text: "foo"}]}

  This is a simple way to manage a relationship. If you need custom behavior, you can customize the action that is
  called, which allows you to add arguments/changes. However, at some point you may want to forego this function
  and make the changes yourself. For example:

      input = [%{id: 10, rating: 10, contents: "foo"}]

      changeset
      |> Changeset.after_action(fn _changeset, result ->
        # An example of updating comments based on a result of other changes
        for comment <- input do
          comment = MyApi.get(Comment, comment.id)

          comment
          |> Map.update(:rating, 0, &(&1 * result.rating_weight))
          |> MyApi.update!()
        end

        {:ok, result}
      end)
  """
  def manage_relationship(changeset, relationship, input, opts \\ []) do
    opts = Ash.OptionsHelpers.validate!(opts, @manage_opts)

    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship when length(input) > 1 ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot manage to a #{type} relationship with a list of records"
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      relationship ->
        value =
          case {relationship.cardinality, input} do
            {:one, []} -> nil
            {:one, [val]} -> val
            {:one, val} -> val
            {:many, val_or_vals} -> List.wrap(val_or_vals)
          end

        relationships =
          changeset.relationships
          |> Map.put_new(relationship.name, %{})
          |> add_to_relationship_key_and_reconcile(relationship, :manage, {value, opts})

        %{changeset | relationships: relationships}
    end
  end

  @doc """
  Appends a record or a list of records to a relationship. Stacks with previous removals/additions.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  For many to many relationships, accepts changes for any `join_attributes` configured on
  the resource. See the section on "Join Attributes" in the module documentation for more.

  Does not authorize changes on the destination resource, nor notify those changes.

  Cannot be used with `belongs_to` or `has_one` relationships.
  See `replace_relationship/3` for manipulating `belongs_to` and `has_one` relationships.
  """
  @spec append_to_relationship(t, atom, Ash.primary_key() | [Ash.primary_key()]) :: t()
  def append_to_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot append to a #{type} relationship"
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      relationship ->
        case primary_key(relationship, List.wrap(record_or_records)) do
          {:ok, primary_keys} ->
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, %{})
              |> add_to_relationship_key_and_reconcile(relationship, :add, primary_keys)

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  @doc """
  Removes a record or a list of records to a relationship. Stacks with previous removals/additions.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  Does not authorize changes on the destination resource, nor notify those changes.

  Cannot be used with `belongs_to` or `has_one` relationships.
  See `replace_relationship/3` for manipulating those relationships.
  """
  @spec remove_from_relationship(t, atom, Ash.primary_key() | [Ash.primary_key()]) :: t()
  def remove_from_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot remove from a #{type} relationship"
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      relationship ->
        case primary_key(relationship, List.wrap(record_or_records)) do
          {:ok, primary_keys} ->
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, %{})
              |> add_to_relationship_key_and_reconcile(relationship, :remove, primary_keys)

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  defp add_to_relationship_key_and_reconcile(relationships, relationship, :manage, manage) do
    Map.update!(relationships, relationship.name, &Map.put(&1, :manage, manage))
  end

  defp add_to_relationship_key_and_reconcile(relationships, relationship, key, to_add) do
    Map.update!(relationships, relationship.name, fn relationship_changes ->
      relationship_changes
      |> Map.put_new(key, [])
      |> Map.update!(key, &Kernel.++(to_add, &1))
      |> reconcile_relationship_changes()
    end)
  end

  @doc """
  Replaces the value of a relationship. Any previous additions/removals are cleared.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  For many to many relationships, accepts changes for any `join_attributes` configured on
  the resource. See the section on "Join Attributes" in the module documentation for more.

  For a `has_many` or `many_to_many` relationship, this means removing any currently related
  records that are not present in the replacement list, and creating any that do not exist
  in the data layer.

  For a `belongs_to` or `has_one`, replace with a `nil` value to unset a relationship.

  Does not authorize changes on the destination resource, nor notify those changes.
  """
  @spec replace_relationship(
          t(),
          atom(),
          Ash.primary_key() | [Ash.primary_key()] | nil
        ) :: t()
  def replace_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
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

      %{cardinality: :one, type: type}
      when is_list(record_or_records) and length(record_or_records) > 1 ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot replace a #{type} relationship with multiple records"
          )

        add_error(changeset, error)

      %{type: :many_to_many} = relationship ->
        do_replace_many_to_many_relationship(changeset, relationship, record_or_records)

      relationship ->
        if Ash.Resource.primary_action(relationship.destination, :read) do
          records =
            if relationship.cardinality == :one do
              if is_list(record_or_records) do
                List.first(record_or_records)
              else
                record_or_records
              end
            else
              List.wrap(record_or_records)
            end

          case primary_key(relationship, records) do
            {:ok, primary_key} ->
              relationships =
                Map.put(changeset.relationships, relationship.name, %{replace: primary_key})

              changeset
              |> check_entities_for_direct_write(relationship.name, List.wrap(records))
              |> Map.put(:relationships, relationships)

            {:error, error} ->
              add_error(changeset, error)
          end
        else
          add_error(
            changeset,
            NoReadAction.exception(
              resource: changeset.resource,
              when: "replacing relationship #{relationship.name}"
            )
          )
        end
    end
  end

  defp do_replace_many_to_many_relationship(changeset, relationship, record_or_records) do
    cond do
      !Ash.Resource.primary_action(relationship.destination, :read) ->
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "replacing relationship #{relationship.name}"
          )
        )

      !Ash.Resource.primary_action(relationship.through, :read) ->
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "replacing relationship #{relationship.name}"
          )
        )

      true ->
        case primary_keys_with_changes(relationship, List.wrap(record_or_records)) do
          {:ok, primary_key} ->
            relationships =
              Map.put(changeset.relationships, relationship.name, %{replace: primary_key})

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  defp check_entities_for_direct_write(changeset, relationship_name, records) do
    if Enum.all?(records, &is_resource?/1) do
      relation_entities =
        Map.merge(Map.get(changeset.context, :destination_entities, %{}), %{
          relationship_name => Enum.group_by(records, & &1.__struct__)
        })

      put_context(changeset, :destination_entities, relation_entities)
    else
      if Ash.Resource.primary_action(
           Ash.Resource.related(changeset.resource, relationship_name),
           :read
         ) do
        changeset
      else
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "editing relationship #{relationship_name} and not supplying full records"
          )
        )
      end
    end
  end

  defp is_resource?(record) do
    Ash.Resource.resource?(record.__struct__)
  rescue
    _error ->
      false
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
    %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
  end

  @doc """
  Remove an argument from the changeset
  """
  def delete_argument(changeset, argument_or_arguments) do
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
    %{changeset | arguments: Map.merge(changeset.arguments, map)}
  end

  @doc """
  Force change an attribute if is not currently being changed, by calling the provided function

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
    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      change_attribute(changeset, key, value)
    end)
  end

  @doc "Adds a change to the changeset, unless the value matches the existing value"
  def change_attribute(changeset, attribute, value) do
    case Ash.Resource.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            name: attribute
          )

        add_error(changeset, error)

      %{writable?: false} = attribute ->
        add_invalid_errors(:attribute, changeset, attribute, "Attribute is not writable")

      attribute ->
        with {:ok, prepared} <- prepare_change(changeset, attribute, value),
             {:ok, casted} <- Ash.Type.cast_input(attribute.type, prepared),
             {:ok, casted} <- handle_change(changeset, attribute, casted),
             :ok <- validate_allow_nil(attribute, casted),
             {:ok, casted} <-
               Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)

          cond do
            is_nil(data_value) and is_nil(casted) ->
              changeset

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              changeset

            true ->
              %{changeset | attributes: Map.put(changeset.attributes, attribute.name, casted)}
          end
        else
          :error ->
            add_invalid_errors(:attribute, changeset, attribute)

          {:error, error_or_errors} ->
            add_invalid_errors(:attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  @doc "Calls `force_change_attribute/3` for each key/value pair provided"
  @spec force_change_attributes(t(), map) :: t()
  def force_change_attributes(changeset, changes) do
    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      force_change_attribute(changeset, key, value)
    end)
  end

  @doc "Changes an attribute even if it isn't writable"
  @spec force_change_attribute(t(), atom, any) :: t()
  def force_change_attribute(changeset, attribute, value) do
    case Ash.Resource.attribute(changeset.resource, attribute) do
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
        with {:ok, prepared} <- prepare_change(changeset, attribute, value),
             {:ok, casted} <- Ash.Type.cast_input(attribute.type, prepared),
             {:ok, casted} <- handle_change(changeset, attribute, casted),
             {:ok, casted} <-
               Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)

          cond do
            is_nil(data_value) and is_nil(casted) ->
              changeset

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              changeset

            true ->
              %{changeset | attributes: Map.put(changeset.attributes, attribute.name, casted)}
          end
        else
          :error ->
            add_invalid_errors(:attribute, changeset, attribute)

          {:error, error_or_errors} ->
            add_invalid_errors(:attribute, changeset, attribute, error_or_errors)
        end
    end
  end

  @doc "Adds a before_action hook to the changeset."
  @spec before_action(t(), (t() -> t() | {t(), %{notificactions: list(Ash.notification())}})) ::
          t()
  def before_action(changeset, func) do
    %{changeset | before_action: [func | changeset.before_action]}
  end

  @doc "Adds an after_action hook to the changeset."
  @spec after_action(
          t(),
          (t(), Ash.record() ->
             {:ok, Ash.record()} | {:ok, Ash.record(), list(Ash.notification())} | {:error, term})
        ) :: t()
  def after_action(changeset, func) do
    %{changeset | after_action: [func | changeset.after_action]}
  end

  @doc "Returns the original data with attribute changes merged, if the changeset is valid."
  @spec apply_attributes(t()) :: {:ok, Ash.record()} | {:error, t()}
  def apply_attributes(%{valid?: true} = changeset) do
    {:ok,
     Enum.reduce(changeset.attributes, changeset.data, fn {attribute, value}, data ->
       Map.put(data, attribute, value)
     end)}
  end

  def apply_attribute(changeset), do: {:error, changeset}

  @doc "Clears an attribute or relationship change off of the changeset"
  def clear_change(changeset, field) do
    cond do
      attr = Ash.Resource.attribute(changeset.resource, field) ->
        %{changeset | attributes: Map.delete(changeset.attributes, attr.name)}

      rel = Ash.Resource.relationship(changeset.resource, field) ->
        %{changeset | relationships: Map.delete(changeset.relationships, rel.name)}

      true ->
        changeset
    end
  end

  @doc "Adds an error to the changesets errors list, and marks the change as `valid?: false`"
  @spec add_error(t(), Ash.error() | String.t() | list(Ash.error() | String.t())) :: t()
  def add_error(changeset, errors) when is_list(errors) do
    if Keyword.keyword?(errors) do
      %{
        changeset
        | errors: [to_change_error(errors) | changeset.errors],
          valid?: false
      }
    else
      Enum.reduce(errors, changeset, &add_error(&2, &1))
    end
  end

  def add_error(changeset, error) when is_binary(error) do
    add_error(
      changeset,
      InvalidChanges.exception(message: error)
    )
  end

  def add_error(changeset, error) do
    %{changeset | errors: [error | changeset.errors], valid?: false}
  end

  defp to_change_error(keyword) do
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
  end

  defp prepare_change(%{action_type: :create}, _attribute, value), do: {:ok, value}

  defp prepare_change(changeset, attribute, value) do
    old_value = Map.get(changeset.data, attribute.name)
    Ash.Type.prepare_change(attribute.type, old_value, value)
  end

  defp handle_change(%{action_type: :create}, _attribute, value), do: {:ok, value}

  defp handle_change(changeset, attribute, value) do
    old_value = Map.get(changeset.data, attribute.name)
    Ash.Type.handle_change(attribute.type, old_value, value)
  end

  defp reconcile_relationship_changes(%{replace: _, add: add} = changes) do
    changes
    |> Map.delete(:add)
    |> Map.update!(:replace, fn replace ->
      replace ++ add
    end)
    |> reconcile_relationship_changes()
  end

  defp reconcile_relationship_changes(%{replace: _, remove: remove} = changes) do
    changes
    |> Map.delete(:remove)
    |> Map.update!(:replace, fn replace ->
      Enum.reject(replace, &(&1 in remove))
    end)
    |> reconcile_relationship_changes()
  end

  defp reconcile_relationship_changes(changes) do
    changes
    |> update_if_present(:replace, &uniq_if_list/1)
    |> update_if_present(:remove, &uniq_if_list/1)
    |> update_if_present(:add, &uniq_if_list/1)
  end

  defp uniq_if_list(list) when is_list(list), do: Enum.uniq(list)
  defp uniq_if_list(other), do: other

  defp update_if_present(map, key, func) do
    if Map.has_key?(map, key) do
      Map.update!(map, key, func)
    else
      map
    end
  end

  defp through_changeset(relationship, changes) do
    new(relationship.through, changes)
  end

  defp primary_keys_with_changes(_, []), do: {:ok, []}

  defp primary_keys_with_changes(relationship, records) do
    Enum.reduce_while(records, {:ok, []}, fn
      {record, changes}, {:ok, acc} ->
        with {:ok, primary_key} <- primary_key(relationship, record),
             %{valid?: true} = changeset <- through_changeset(relationship, changes) do
          {:cont, {:ok, [{primary_key, changeset} | acc]}}
        else
          %{valid?: false, errors: errors} -> {:halt, {:error, errors}}
          {:error, error} -> {:halt, {:error, error}}
        end

      record, {:ok, acc} ->
        case primary_key(relationship, record) do
          {:ok, primary_keys} when is_list(primary_keys) ->
            {:cont, {:ok, primary_keys ++ acc}}

          {:ok, primary_key} ->
            {:cont, {:ok, [primary_key | acc]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
  end

  defp primary_key(_, nil), do: {:ok, nil}

  defp primary_key(relationship, records) when is_list(records) do
    case Ash.Resource.primary_key(relationship.destination) do
      [_field] ->
        multiple_primary_keys(relationship, records)

      _ ->
        pluck_pk_fields(relationship, records)
    end
  end

  defp primary_key(relationship, record) do
    do_primary_key(relationship, record)
  end

  defp pluck_pk_fields(relationship, records) do
    Enum.reduce_while(
      records,
      {:ok, []},
      fn
        record, {:ok, acc} ->
          case do_primary_key(relationship, record) do
            {:ok, pk} -> {:cont, {:ok, [pk | acc]}}
            {:error, error} -> {:halt, {:error, error}}
          end
      end
    )
  end

  defp do_primary_key(relationship, record) when is_map(record) do
    primary_key = Ash.Resource.primary_key(relationship.destination)

    is_pkey_map? =
      Enum.all?(
        primary_key,
        fn key ->
          Map.has_key?(record, key) || Map.has_key?(record, to_string(key))
        end
      )

    if is_pkey_map? do
      pkey =
        Enum.reduce(primary_key, %{}, fn key, acc ->
          case Map.fetch(record, key) do
            {:ok, value} -> Map.put(acc, key, value)
            :error -> Map.put(acc, key, Map.get(record, to_string(key)))
          end
        end)

      {:ok, pkey}
    else
      error =
        InvalidRelationship.exception(
          relationship: relationship.name,
          message: "Invalid identifier #{inspect(record)}"
        )

      {:error, error}
    end
  end

  defp do_primary_key(relationship, record) do
    single_primary_key(relationship, record)
  end

  defp multiple_primary_keys(relationship, values) do
    Enum.reduce_while(values, {:ok, []}, fn record, {:ok, primary_keys} ->
      case do_primary_key(relationship, record) do
        {:ok, pkey} -> {:cont, {:ok, [pkey | primary_keys]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp single_primary_key(relationship, value) do
    with [field] <- Ash.Resource.primary_key(relationship.destination),
         attribute <- Ash.Resource.attribute(relationship.destination, field),
         {:ok, casted} <- Ash.Type.cast_input(attribute.type, value) do
      {:ok, %{field => casted}}
    else
      _ ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Invalid identifier #{inspect(value)}"
          )

        {:error, error}
    end
  end

  @doc false
  def changes_depend_on(changeset, dependency) do
    %{changeset | change_dependencies: [dependency | changeset.change_dependencies]}
  end

  @doc false
  def add_requests(changeset, requests) when is_list(requests) do
    Enum.reduce(requests, changeset, &add_requests(&2, &1))
  end

  def add_requests(changeset, request) do
    %{changeset | requests: [request | changeset.requests]}
  end

  defp validate_allow_nil(%{allow_nil?: false} = attribute, nil) do
    {:error,
     InvalidAttribute.exception(
       field: attribute.name,
       message: "must be present"
     )}
  end

  defp validate_allow_nil(_, _), do: :ok

  defp add_invalid_errors(type, changeset, attribute, message \\ nil) do
    messages =
      if Keyword.keyword?(message) do
        [message]
      else
        List.wrap(message)
      end

    Enum.reduce(messages, changeset, fn message, changeset ->
      opts =
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

        add_error(changeset, error)
      end)
    end)
  end

  defp add_field(message, field) do
    "at field #{field} " <> message
  end

  defp add_index(message, opts) do
    if opts[:index] do
      "at index #{opts[:index]} " <> message
    else
      message
    end
  end
end

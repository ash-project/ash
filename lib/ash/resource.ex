defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.Dsl`
  """

  alias Ash.Dsl.Extension

  defmacro __using__(opts) do
    data_layer = Macro.expand(opts[:data_layer], __CALLER__)
    embedded? = data_layer == :embedded

    data_layer =
      if embedded? do
        Ash.DataLayer.Simple
      else
        data_layer || Ash.DataLayer.Simple
      end

    opts = Keyword.put(opts, :data_layer, data_layer)

    authorizers =
      opts[:authorizers]
      |> List.wrap()
      |> Enum.map(&Macro.expand(&1, __CALLER__))

    notifiers =
      opts[:notifiers]
      |> List.wrap()
      |> Enum.map(&Macro.expand(&1, __CALLER__))

    extensions =
      if Ash.implements_behaviour?(data_layer, Ash.Dsl.Extension) do
        [data_layer, Ash.Resource.Dsl]
      else
        [Ash.Resource.Dsl]
      end

    authorizer_extensions =
      Enum.filter(authorizers, &Ash.implements_behaviour?(&1, Ash.Dsl.Extension))

    notifier_extensions =
      Enum.filter(notifiers, &Ash.implements_behaviour?(&1, Ash.Dsl.Extension))

    extensions =
      Enum.concat([
        extensions,
        opts[:extensions] || [],
        authorizer_extensions,
        notifier_extensions
      ])

    body =
      quote bind_quoted: [opts: opts, embedded?: embedded?] do
        @before_compile Ash.Resource

        @authorizers opts[:authorizers] || []
        @notifiers opts[:notifiers] || []
        @data_layer opts[:data_layer] || Ash.DataLayer.Simple
        @extensions (opts[:extensions] || []) ++
                      List.wrap(opts[:data_layer] || Ash.DataLayer.Simple) ++
                      (opts[:authorizers] || [])
        @embedded embedded?

        if embedded? do
          Ash.Resource.define_embeddable_type()
        end
      end

    preparations = Extension.prepare(extensions)

    [body | preparations]
  end

  @embedded_resource_array_constraints [
    sort: [
      type: :any,
      doc: """
      A sort to be applied when casting the data.

      Only relevant for a type of {:array, `EmbeddedResource}`

      The sort is not applied when reading the data, so if the sort changes you will
      need to fix it in your database or wait for the data to be written again, at which
      point it will be sorted when casting.
      """
    ],
    load: [
      type: {:list, :atom},
      doc: """
      A list of calculations to load on the resource.

      Only relevant for a type of {:array, `EmbeddedResource}`

      Aggregates are not supported on embedded resources.
      """
    ]
  ]

  @doc false
  def embedded_resource_array_constraints, do: @embedded_resource_array_constraints

  @doc false
  def handle_errors(errors) do
    errors
    |> do_handle_errors()
    |> List.wrap()
    |> Ash.Error.flatten_preserving_keywords()
  end

  defp do_handle_errors(errors) when is_list(errors) do
    if Keyword.keyword?(errors) do
      main_fields = Keyword.take(errors, [:message, :field, :fields])
      vars = Keyword.merge(main_fields, Keyword.get(errors, :vars, []))

      main_fields
      |> Keyword.put(:vars, vars)
      |> Enum.into(%{})
      |> do_handle_errors()
    else
      Enum.map(errors, &do_handle_errors/1)
    end
  end

  defp do_handle_errors(%{errors: errors}) do
    errors
    |> List.wrap()
    |> do_handle_errors()
  end

  defp do_handle_errors(%Ash.Error.Changes.InvalidAttribute{
         message: message,
         field: field,
         vars: vars
       }) do
    vars
    |> Keyword.put(:field, field)
    |> Keyword.put(:message, message)
    |> add_index()
  end

  defp do_handle_errors(%{message: message, vars: vars, field: field}) do
    vars
    |> Keyword.put(:message, message)
    |> Keyword.put(:field, field)
    |> add_index()
  end

  defp do_handle_errors(%{message: message, vars: vars}) do
    vars
    |> Keyword.put(:message, message)
    |> add_index()
  end

  defp do_handle_errors(%{field: field} = exception) do
    [field: field, message: Exception.message(exception)]
  end

  defp do_handle_errors(error) when is_binary(error) do
    [message: error]
  end

  defp do_handle_errors(error) do
    [message: Exception.message(error)]
  end

  defp add_index(opts) do
    cond do
      opts[:index] && opts[:field] ->
        Keyword.put(opts, :field, "#{opts[:field]}[#{opts[:index]}]")

      opts[:index] ->
        Keyword.put(opts, :field, "[#{opts[:index]}]")

      true ->
        opts
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro single_embed_implementation do
    # credo:disable-for-next-line Credo.Check.Refactor.LongQuoteBlocks
    quote do
      alias __MODULE__.ShadowApi
      def storage_type, do: :map

      def cast_input(%{__struct__: __MODULE__} = input), do: {:ok, input}

      def cast_input(value) when is_map(value) do
        action = Ash.Resource.primary_action(__MODULE__, :create)

        __MODULE__
        |> Ash.Changeset.for_create(action.name, value)
        |> ShadowApi.create()
        |> case do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.Resource.handle_errors(error)}
        end
      end

      def cast_input(nil), do: {:ok, nil}
      def cast_input(_), do: :error

      def cast_stored(value) when is_map(value) do
        __MODULE__
        |> Ash.Resource.attributes()
        |> Enum.reduce_while(%{__struct__: __MODULE__}, fn attr, {:ok, struct} ->
          with {:fetch, {:ok, value}} <- {:fetch, fetch_key(value, attr.name)},
               {:ok, casted} <- Ash.Type.cast_stored(attr.type, value) do
            {:ok, Map.put(struct, attr.name, casted)}
          else
            {:fetch, :error} ->
              {:ok, struct}

            other ->
              {:halt, other}
          end
        end)
      end

      def cast_stored(nil), do: {:ok, nil}

      def cast_stored(_other) do
        :error
      end

      def fetch_key(map, atom) do
        case Map.fetch(map, atom) do
          {:ok, value} ->
            value

          :error ->
            Map.fetch(map, to_string(atom))
        end
      end

      def dump_to_native(value) when is_map(value) do
        attributes = Ash.Resource.attributes(__MODULE__)
        calculations = Ash.Resource.calculations(__MODULE__)
        Map.take(value, Enum.map(attributes ++ calculations, & &1.name))
      end

      def dump_to_native(nil), do: nil
      def dump_to_native(_), do: :error
      def constraints, do: Keyword.take(array_constraints(), [:load])

      def apply_constraints(nil, _), do: {:ok, nil}

      def apply_constraints(term, constraints) do
        __MODULE__
        |> Ash.Query.put_context(:data, [term])
        |> Ash.Query.load(constraints[:load] || [])
        |> ShadowApi.read()
        |> case do
          {:ok, [result]} ->
            {:ok, result}

          {:error, errors} ->
            {:error, Ash.Resource.handle_errors(errors)}
        end
      end

      def handle_change(nil, new_value) do
        {:ok, new_value}
      end

      def handle_change(old_value, nil) do
        case ShadowApi.destroy(old_value) do
          :ok -> {:ok, nil}
          {:error, error} -> {:error, Ash.Resource.handle_errors(error)}
        end
      end

      def handle_change(old_value, new_value) do
        pkey_fields = Ash.Resource.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.attribute(__MODULE__, pkey_field).private?
           end) do
          {:ok, new_value}
        else
          pkey = Map.take(old_value, pkey_fields)

          if Map.take(new_value, pkey_fields) == pkey do
            {:ok, new_value}
          else
            case ShadowApi.destroy(old_value) do
              :ok -> {:ok, new_value}
              {:error, error} -> {:error, Ash.Resource.handle_errors(error)}
            end
          end
        end
      end

      def prepare_change(_old_value, nil) do
        {:ok, nil}
      end

      def prepare_change(_old_value, %{__struct__: __MODULE__} = new_value) do
        {:ok, new_value}
      end

      def prepare_change(nil, new_value) do
        {:ok, new_value}
      end

      def prepare_change(old_value, new_uncasted_value) do
        pkey_fields = Ash.Resource.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.attribute(__MODULE__, pkey_field).private?
           end) do
          action = Ash.Resource.primary_action!(__MODULE__, :update)

          old_value
          |> Ash.Changeset.for_update(action.name, new_uncasted_value)
          |> ShadowApi.update()
          |> case do
            {:ok, value} -> {:ok, value}
            {:error, error} -> {:error, Ash.Resource.handle_errors(error)}
          end
        else
          pkey =
            Enum.into(pkey_fields, %{}, fn pkey_field ->
              case fetch_key(new_uncasted_value, pkey_field) do
                :error ->
                  {pkey_field, :error}

                value ->
                  attribute = Ash.Resource.attribute(__MODULE__, pkey_field)

                  case Ash.Type.cast_input(attribute.type, value) do
                    {:ok, casted} ->
                      {pkey_field, casted}

                    _ ->
                      {pkey_field, :error}
                  end
              end
            end)

          if Enum.any?(Map.values(pkey), &(&1 == :error)) do
            {:ok, new_uncasted_value}
          else
            old_pkey = Map.take(old_value, pkey_fields)

            if old_pkey == pkey do
              action = Ash.Resource.primary_action!(__MODULE__, :update)

              old_value
              |> Ash.Changeset.for_update(action.name, new_uncasted_value)
              |> ShadowApi.update()
              |> case do
                {:ok, value} -> {:ok, value}
                {:error, error} -> {:error, Ash.Resource.handle_errors(error)}
              end
            else
              {:ok, new_uncasted_value}
            end
          end
        end
      end
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro array_embed_implementation do
    # credo:disable-for-next-line Credo.Check.Refactor.LongQuoteBlocks
    quote do
      alias __MODULE__.ShadowApi
      def array_constraints, do: Ash.Resource.embedded_resource_array_constraints()

      def apply_constraints_array([], _constraints), do: {:ok, []}

      def apply_constraints_array(term, constraints) do
        pkey = Ash.Resource.primary_key(__MODULE__)
        unique_keys = Enum.map(Ash.Resource.identities(__MODULE__), & &1.keys) ++ [pkey]

        case Enum.find(unique_keys, fn unique_key ->
               has_duplicates?(term, &Map.take(&1, unique_key))
             end) do
          nil ->
            query =
              __MODULE__
              |> Ash.Query.put_context(:data, term)
              |> Ash.Query.load(constraints[:load] || [])

            query =
              if constraints[:sort] do
                Ash.Query.sort(query, constraints[:sort])
              else
                query
              end

            ShadowApi.read(query)

          keys ->
            {:error, message: "items must be unique on keys %{keys}", keys: Enum.join(keys, ",")}
        end
      end

      defp has_duplicates?(list, func) do
        list
        |> Enum.reduce_while(MapSet.new(), fn x, acc ->
          x = func.(x)

          if MapSet.member?(acc, x) do
            {:halt, 0}
          else
            {:cont, MapSet.put(acc, x)}
          end
        end)
        |> is_integer()
      end

      def handle_change_array(nil, new_values) do
        handle_change_array([], new_values)
      end

      def handle_change_array(old_values, nil) do
        handle_change_array(old_values, [])
      end

      def handle_change_array(old_values, new_values) do
        pkey_fields = Ash.Resource.primary_key(__MODULE__)

        old_values
        |> Enum.with_index()
        |> Enum.reject(fn {old_value, _} ->
          pkey = Map.take(old_value, pkey_fields)

          Enum.any?(new_values, fn new_value ->
            Map.take(new_value, pkey_fields) == pkey
          end)
        end)
        |> Enum.reduce_while(:ok, fn {record, index}, :ok ->
          case ShadowApi.destroy(record) do
            :ok ->
              {:cont, :ok}

            {:error, error} ->
              errors =
                error
                |> Ash.Resource.handle_errors()
                |> Enum.map(fn keyword ->
                  Keyword.put(keyword, :index, index)
                end)

              {:halt, {:error, errors}}
          end
        end)
        |> case do
          :ok ->
            {:ok, new_values}

          {:error, error} ->
            {:error, error}
        end
      end

      def prepare_change_array(old_values, new_uncasted_values) do
        pkey_fields = Ash.Resource.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.attribute(__MODULE__, pkey_field).private?
           end) do
          {:ok, new_uncasted_values}
        else
          pkey_attributes =
            Enum.into(pkey_fields, %{}, fn field ->
              {field, Ash.Resource.attribute(__MODULE__, field)}
            end)

          new_uncasted_values
          |> Enum.with_index()
          |> Enum.reduce_while({:ok, []}, fn {new, index}, {:ok, new_uncasted_values} ->
            pkey =
              Enum.into(pkey_fields, %{}, fn pkey_field ->
                case fetch_key(new, pkey_field) do
                  :error ->
                    :error

                  value ->
                    case Ash.Type.cast_input(Map.get(pkey_attributes, pkey_field).type, value) do
                      {:ok, casted} ->
                        {pkey_field, casted}

                      _ ->
                        :error
                    end
                end
              end)

            if Enum.any?(Map.values(pkey), &(&1 == :error)) do
              {:cont, {:ok, [new | new_uncasted_values]}}
            else
              value_updating_from =
                Enum.find(old_values, fn old_value ->
                  Map.take(old_value, pkey_fields) == pkey
                end)

              if value_updating_from do
                default_update = Ash.Resource.primary_action!(__MODULE__, :update)

                value_updating_from
                |> Ash.Changeset.for_update(default_update.name, new)
                |> ShadowApi.update()
                |> case do
                  {:ok, value} ->
                    {:cont, {:ok, [value | new_uncasted_values]}}

                  {:error, error} ->
                    errors =
                      error
                      |> Ash.Resource.handle_errors()
                      |> Enum.map(fn keyword ->
                        Keyword.put(keyword, :index, index)
                      end)

                    {:halt, {:error, errors}}
                end
              else
                {:cont, {:ok, [new | new_uncasted_values]}}
              end
            end
          end)
          |> case do
            {:ok, values} -> {:ok, Enum.reverse(values)}
            {:error, error} -> {:error, error}
          end
        end
      end
    end
  end

  defmacro define_embeddable_type do
    quote do
      use Ash.Type

      parent = __MODULE__

      defmodule ShadowApi do
        @moduledoc false
        use Ash.Api

        @parent parent

        resources do
          resource @parent, warn_on_compile_failure?: false
        end
      end

      Ash.Resource.single_embed_implementation()
      Ash.Resource.array_embed_implementation()
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro __before_compile__(_env) do
    quote unquote: false do
      @doc false
      alias Ash.Dsl.Extension

      @type t :: %__MODULE__{}

      Module.register_attribute(__MODULE__, :is_ash_resource, persist: true, accumulate: false)
      @is_ash_resource true

      @on_load :on_load

      ash_dsl_config =
        Macro.escape(
          Extension.set_state(
            notifiers: @notifiers,
            authorizers: @authorizers,
            data_layer: @data_layer,
            embedded?: @embedded
          )
        )

      @doc false
      def ash_dsl_config do
        unquote(ash_dsl_config)
      end

      def on_load do
        Extension.load()
      end

      require Ash.Schema

      Ash.Schema.define_schema()
    end
  end

  @spec extensions(Ash.resource()) :: [module]
  def extensions(resource) do
    Extension.get_persisted(resource, :extensions)
  end

  @spec embedded?(Ash.resource()) :: boolean
  def embedded?(resource) do
    Extension.get_persisted(resource, :embedded?, false)
  end

  @spec description(Ash.resource()) :: String.t() | nil
  def description(resource) do
    Extension.get_opt(resource, [:resource], :description, "no description")
  end

  @spec base_filter(Ash.resource()) :: term
  def base_filter(resource) do
    Extension.get_opt(resource, [:resource], :base_filter, nil)
  end

  @doc "A list of identities for the resource"
  @spec identities(Ash.resource()) :: [Ash.Resource.Identity.t()]
  def identities(resource) do
    resource
    |> Extension.get_entities([:resource, :identities])
  end

  @doc "A list of authorizers to be used when accessing"
  @spec authorizers(Ash.resource()) :: [module]
  def authorizers(resource) do
    Extension.get_persisted(resource, :authorizers, [])
  end

  @doc "A list of notifiers to be used when accessing"
  @spec notifiers(Ash.resource()) :: [module]
  def notifiers(resource) do
    Extension.get_persisted(resource, :notifiers, [])
  end

  @spec validations(Ash.resource(), :create | :update | :destroy) :: [Ash.validation()]
  def validations(resource, type) do
    resource
    |> validations()
    |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all validations for the resource"
  @spec validations(Ash.resource()) :: [Ash.validation()]
  def validations(resource) do
    Extension.get_entities(resource, [:validations])
  end

  @doc "Whether or not a given module is a resource module"
  @spec resource?(module) :: boolean
  def resource?(module) when is_atom(module) do
    module.module_info(:attributes)[:is_ash_resource] == [true]
  end

  def resource?(_), do: false

  @doc "A list of field names corresponding to the primary key"
  @spec primary_key(Ash.resource()) :: list(atom)
  def primary_key(resource) do
    Ash.Dsl.Extension.get_persisted(resource, :primary_key, [])
  end

  @doc "Returns all relationships of a resource"
  @spec relationships(Ash.resource()) :: list(Ash.relationship())
  def relationships(resource) do
    Extension.get_entities(resource, [:relationships])
  end

  @doc "Get a relationship by name or path"
  @spec relationship(Ash.resource(), atom | String.t() | [atom | String.t()]) ::
          Ash.relationship() | nil
  def relationship(resource, [name]) do
    relationship(resource, name)
  end

  def relationship(resource, [name | rest]) do
    case relationship(resource, name) do
      nil ->
        nil

      relationship ->
        relationship(relationship.destination, rest)
    end
  end

  def relationship(resource, relationship_name) when is_binary(relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(to_string(&1.name) == relationship_name))
  end

  def relationship(resource, relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(&1.name == relationship_name))
  end

  @doc "Returns all public relationships of a resource"
  @spec public_relationships(Ash.resource()) :: list(Ash.relationship())
  def public_relationships(resource) do
    resource
    |> relationships()
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public relationship by name or path"
  def public_relationship(resource, [name | rest]) do
    case public_relationship(resource, name) do
      nil ->
        nil

      relationship ->
        public_relationship(relationship.destination, rest)
    end
  end

  def public_relationship(resource, relationship_name) when is_binary(relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(to_string(&1.name) == relationship_name && !&1.private?))
  end

  def public_relationship(resource, relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(&1.name == relationship_name && !&1.private?))
  end

  @doc "Get the multitenancy strategy for a resource"
  @spec multitenancy_strategy(Ash.resource()) :: :context | :attribute | nil
  def multitenancy_strategy(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :strategy, nil)
  end

  @spec multitenancy_attribute(Ash.resource()) :: atom | nil
  def multitenancy_attribute(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :attribute, nil)
  end

  @spec multitenancy_parse_attribute(Ash.resource()) :: {atom, atom, list(any)}
  def multitenancy_parse_attribute(resource) do
    Ash.Dsl.Extension.get_opt(
      resource,
      [:multitenancy],
      :parse_attribute,
      {__MODULE__, :identity, []}
    )
  end

  @doc false
  def identity(x), do: x

  @spec multitenancy_global?(Ash.resource()) :: atom | nil
  def multitenancy_global?(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :global?, nil)
  end

  @spec multitenancy_source(Ash.resource()) :: atom | nil
  def multitenancy_source(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :source, nil)
  end

  @spec multitenancy_template(Ash.resource()) :: atom | nil
  def multitenancy_template(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :template, nil)
  end

  @doc "Returns all calculations of a resource"
  @spec calculations(Ash.resource()) :: list(Ash.calculation())
  def calculations(resource) do
    Extension.get_entities(resource, [:calculations])
  end

  @doc "Get a calculation by name"
  @spec calculation(Ash.resource(), atom | String.t()) :: Ash.calculation() | nil
  def calculation(resource, name) when is_binary(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public calculations of a resource"
  @spec public_calculations(Ash.resource()) :: list(Ash.calculation())
  def public_calculations(resource) do
    resource
    |> Extension.get_entities([:calculations])
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public calculation by name"
  @spec public_calculation(Ash.resource(), atom | String.t()) :: Ash.calculation() | nil
  def public_calculation(resource, name) when is_binary(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @doc "Returns all aggregates of a resource"
  @spec aggregates(Ash.resource()) :: list(Ash.aggregate())
  def aggregates(resource) do
    Extension.get_entities(resource, [:aggregates])
  end

  @doc "Get an aggregate by name"
  @spec aggregate(Ash.resource(), atom | String.t()) :: Ash.aggregate() | nil
  def aggregate(resource, name) when is_binary(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public aggregates of a resource"
  @spec public_aggregates(Ash.resource()) :: list(Ash.aggregate())
  def public_aggregates(resource) do
    resource
    |> Extension.get_entities([:aggregates])
    |> Enum.reject(& &1.private?)
  end

  @doc "Get an aggregate by name"
  @spec public_aggregate(Ash.resource(), atom | String.t()) :: Ash.aggregate() | nil
  def public_aggregate(resource, name) when is_binary(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @doc "Returns the primary action of the given type"
  @spec primary_action!(Ash.resource(), Ash.action_type()) :: Ash.action() | no_return
  def primary_action!(resource, type) do
    case primary_action(resource, type) do
      nil -> raise "Required primary #{type} action for #{inspect(resource)}"
      action -> action
    end
  end

  @doc "Returns the primary action of a given type"
  @spec primary_action(Ash.resource(), Ash.action_type()) :: Ash.action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  @doc "Returns the configured default actions"
  @spec default_actions(Ash.resource()) :: [:create | :read | :update | :destroy]
  def default_actions(resource) do
    Extension.get_opt(
      resource,
      [:actions],
      :defaults,
      [:create, :read, :update, :destroy]
    )
  end

  @doc "Returns all actions of a resource"
  @spec actions(Ash.resource()) :: [Ash.action()]
  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(Ash.resource(), atom(), Ash.action_type()) :: Ash.action() | nil
  def action(resource, name, type) do
    resource
    |> actions()
    |> Enum.find(&(&1.name == name && &1.type == type))
  end

  @doc "Returns all attributes of a resource"
  @spec attributes(Ash.resource()) :: [Ash.attribute()]
  def attributes(resource) do
    Extension.get_entities(resource, [:attributes])
  end

  @doc "Get an attribute name from the resource"
  @spec attribute(Ash.resource(), String.t() | atom) :: Ash.attribute() | nil
  def attribute(resource, name) when is_binary(name) do
    resource
    |> attributes()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def attribute(resource, name) do
    resource
    |> attributes()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public attributes of a resource"
  @spec public_attributes(Ash.resource()) :: [Ash.attribute()]
  def public_attributes(resource) do
    resource
    |> attributes()
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public attribute name from the resource"
  @spec public_attribute(Ash.resource(), String.t() | atom) :: Ash.attribute() | nil
  def public_attribute(resource, name) when is_binary(name) do
    resource
    |> attributes()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_attribute(resource, name) do
    resource
    |> attributes()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @spec related(Ash.resource(), atom() | String.t() | [atom() | String.t()]) ::
          Ash.resource() | nil
  def related(resource, relationship) when not is_list(relationship) do
    related(resource, [relationship])
  end

  def related(resource, []), do: resource

  def related(resource, [path | rest]) do
    case relationship(resource, path) do
      %{destination: destination} -> related(destination, rest)
      nil -> nil
    end
  end

  @doc "The data layer of the resource, or nil if it does not have one"
  @spec data_layer(Ash.resource()) :: Ash.data_layer()
  def data_layer(resource) do
    Extension.get_persisted(resource, :data_layer)
  end

  @doc "Whether or not the data layer supports a specific feature"
  @spec data_layer_can?(Ash.resource(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && Ash.DataLayer.can?(feature, resource)
  end

  @doc "Custom operators supported by the data layer of the resource"
  @spec data_layer_operators(Ash.resource()) :: map
  def data_layer_operators(resource) do
    Ash.DataLayer.operators(resource)
  end

  @doc "Custom functions supported by the data layer of the resource"
  @spec data_layer_functions(Ash.resource()) :: map
  def data_layer_functions(resource) do
    Ash.DataLayer.functions(resource)
  end

  @doc "Whether or not the data layer for the resource is currently in a transaction"
  @spec in_transaction?(Ash.resource()) :: boolean
  def in_transaction?(resource) do
    data_layer(resource).in_transaction?(resource)
  end

  @doc "Wraps the execution of the function in a transaction with the resource's data_layer"
  @spec transaction(Ash.resource(), (() -> term)) :: term
  def transaction(resource, func) do
    if data_layer_can?(resource, :transact) do
      data_layer(resource).transaction(resource, func)
    else
      func.()
    end
  end

  @doc "Rolls back the current transaction"
  @spec rollback(Ash.resource(), term) :: no_return
  def rollback(resource, term) do
    data_layer(resource).rollback(resource, term)
  end
end

defmodule Ash.Resource.Builder do
  @moduledoc """
  Tools for transforming resources in DSL Transformers.
  """

  alias Spark.Dsl.Transformer
  use Spark.Dsl.Builder

  @doc """
  Builds and adds a new action unless an action with that name already exists
  """
  @spec add_new_action(
          Spark.Dsl.Builder.input(),
          type :: Ash.Resource.Actions.action_type(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_action(dsl_state, type, name, opts \\ []) do
    if Ash.Resource.Info.action(dsl_state, name) do
      dsl_state
    else
      add_action(dsl_state, type, name, opts)
    end
  end

  @doc """
  Builds and adds an action
  """
  @spec add_action(
          Spark.Dsl.Builder.input(),
          type :: Ash.Resource.Actions.action_type(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_action(dsl_state, type, name, opts \\ []) do
    with {:ok, opts} <-
           handle_nested_builders(opts, [
             :pagination
           ]),
         {:ok, action} <- build_action(type, name, opts) do
      Transformer.add_entity(dsl_state, [:actions], action, type: :append)
    end
  end

  @doc """
  Builds and adds an action to the front of the actions list
  """
  @spec prepend_action(
          Spark.Dsl.Builder.input(),
          type :: Ash.Resource.Actions.action_type(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder prepend_action(dsl_state, type, name, opts \\ []) do
    with {:ok, opts} <-
           handle_nested_builders(opts, [
             :pagination
           ]),
         {:ok, action} <- build_action(type, name, opts) do
      Transformer.add_entity(dsl_state, [:actions], action)
    end
  end

  @doc """
  Builds an action
  """
  @spec build_action(
          type :: Ash.Resource.Actions.action_type(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Actions.action()} | {:error, term}
  def build_action(type, name, opts \\ []) do
    with {:ok, opts} <-
           handle_nested_builders(opts, [
             :changes,
             :arguments,
             :metadata,
             :pagination,
             :preparations
           ]) do
      Transformer.build_entity(
        Ash.Resource.Dsl,
        [:actions],
        type,
        Keyword.merge(opts, name: name)
      )
    end
  end

  @doc """
  Builds and adds a new relationship unless a relationship with that name already exists
  """
  @spec add_new_relationship(
          Spark.Dsl.Builder.input(),
          type :: Ash.Resource.Relationships.type(),
          name :: atom,
          destination :: module,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_relationship(dsl_state, type, name, destination, opts \\ []) do
    if Ash.Resource.Info.relationship(dsl_state, name) do
      dsl_state
    else
      add_relationship(dsl_state, type, name, destination, opts)
    end
  end

  @doc """
  Builds and adds an action
  """
  @spec add_relationship(
          Spark.Dsl.Builder.input(),
          type :: Ash.Resource.Relationships.type(),
          name :: atom,
          destination :: module,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_relationship(dsl_state, type, name, destination, opts \\ []) do
    with {:ok, relationship} <- build_relationship(type, name, destination, opts) do
      Transformer.add_entity(dsl_state, [:relationships], relationship, type: :append)
    end
  end

  @doc """
  Builds a relationship
  """
  @spec build_relationship(
          type :: Ash.Resource.Relationships.type(),
          name :: atom,
          destination :: module,
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Relationships.relationship()} | {:error, term}
  def build_relationship(type, name, destination, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:relationships],
      type,
      Keyword.merge(opts, name: name, destination: destination)
    )
  end

  @doc """
  Builds and adds a new identity unless an identity with that name already exists
  """
  @spec add_new_identity(
          Spark.Dsl.Builder.input(),
          name :: atom,
          fields :: atom | list(atom),
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_identity(dsl_state, name, fields, opts \\ []) do
    if Ash.Resource.Info.identity(dsl_state, name) do
      dsl_state
    else
      add_identity(dsl_state, name, fields, opts)
    end
  end

  @doc """
  Builds and adds an identity
  """
  @spec add_identity(
          Spark.Dsl.Builder.input(),
          name :: atom,
          fields :: atom | list(atom),
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_identity(dsl_state, name, fields, opts \\ []) do
    with {:ok, identity} <- build_identity(name, fields, opts) do
      Transformer.add_entity(dsl_state, [:identities], identity, type: :append)
    end
  end

  @doc """
  Builds an action
  """
  @spec build_identity(
          name :: atom,
          fields :: atom | list(atom),
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Identity.t()} | {:error, term}
  def build_identity(name, fields, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:identities],
      :identity,
      Keyword.merge(opts, name: name, keys: fields)
    )
  end

  @doc """
  Builds and adds a change
  """
  @spec add_change(
          Spark.Dsl.Builder.input(),
          ref :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_change(dsl_state, ref, opts \\ []) do
    ref =
      case ref do
        {module, opts} -> {module, opts}
        module -> {module, []}
      end

    type =
      if opts[:prepend?] do
        :prepend
      else
        :append
      end

    opts = Keyword.delete(opts, :prepend?)

    with {:ok, change} <- build_change(ref, opts) do
      Transformer.add_entity(dsl_state, [:changes], change, type: type)
    end
  end

  @doc """
  Builds a change
  """
  @spec build_change(
          ref :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Change.t()} | {:error, term}
  def build_change(ref, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:changes],
      :change,
      Keyword.merge(opts, change: ref)
    )
  end

  @doc """
  Builds and adds a preparation
  """
  @spec add_preparation(
          Spark.Dsl.Builder.input(),
          ref :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_preparation(dsl_state, ref, opts \\ []) do
    ref =
      case ref do
        {module, opts} -> {module, opts}
        module -> {module, []}
      end

    with {:ok, preparation} <- build_preparation(ref, opts) do
      Transformer.add_entity(dsl_state, [:preparations], preparation, type: :append)
    end
  end

  @doc """
  Builds a preparation
  """
  @spec build_preparation(
          ref :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Preparation.t()} | {:error, term}
  def build_preparation(ref, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:preparations],
      :prepare,
      Keyword.merge(opts, preparation: ref)
    )
  end

  @doc """
  Builds a pagination object
  """
  @spec build_pagination(pts :: Keyword.t()) ::
          {:ok, Ash.Resource.Actions.Read.Pagination.t()} | {:error, term}
  def build_pagination(opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      # All action types that support arguments have the same entity, so we just say `create` here
      [:actions, :read],
      :pagination,
      opts
    )
  end

  @doc """
  Builds an action argument
  """
  @spec build_action_argument(name :: atom, type :: Ash.Type.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Actions.Argument.t()} | {:error, term}
  def build_action_argument(name, type, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      # All action types that support arguments have the same entity, so we just say `create` here
      [:actions, :create],
      :argument,
      Keyword.merge(opts, name: name, type: type)
    )
  end

  @doc """
  Builds a calculation argument
  """
  @spec build_calculation_argument(name :: atom, type :: Ash.Type.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Calculation.Argument.t()} | {:error, term}
  def build_calculation_argument(name, type, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:calculations, :calculate],
      :argument,
      Keyword.merge(opts, name: name, type: type)
    )
  end

  @doc """
  Builds an action metadata
  """
  @spec build_action_metadata(name :: atom, type :: Ash.Type.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Actions.Metadata.t()} | {:error, term}
  def build_action_metadata(name, type, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      # All action types that support arguments have the same entity, so we just say `create` here
      [:actions, :create],
      :metadata,
      Keyword.merge(opts, name: name, type: type)
    )
  end

  @doc """
  Builds an action change
  """
  @spec build_action_change(change :: Ash.Resource.Change.ref(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Change.t()} | {:error, term}
  def build_action_change(change, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      # All action types  that support changes have the same change entity, so we just say `create` here
      [:actions, :create],
      :change,
      Keyword.put(opts, :change, change)
    )
  end

  @doc """
  Builds and adds an update_timestamp unless an update_timestamp with that name already exists
  """
  @spec add_new_update_timestamp(Spark.Dsl.Builder.input(), name :: atom, opts :: Keyword.t()) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_update_timestamp(dsl_state, name, opts \\ []) do
    if Ash.Resource.Info.attribute(dsl_state, name) do
      dsl_state
    else
      add_update_timestamp(dsl_state, name, opts)
    end
  end

  @doc """
  Builds and adds an update_timestamp
  """
  @spec add_update_timestamp(Spark.Dsl.Builder.input(), name :: atom, opts :: Keyword.t()) ::
          Spark.Dsl.Builder.result()
  defbuilder add_update_timestamp(dsl_state, name, opts \\ []) do
    with {:ok, update_timestamp} <- build_update_timestamp(name, opts) do
      Transformer.add_entity(dsl_state, [:attributes], update_timestamp, type: :append)
    end
  end

  @doc """
  Builds an update_timestamp with the given name, type, and options
  """
  @spec build_update_timestamp(name :: atom, opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Attribute.t()} | {:error, term}
  def build_update_timestamp(name, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:attributes],
      :update_timestamp,
      Keyword.merge(opts, name: name)
    )
  end

  @doc """
  Builds and adds a create_timestamp unless a create_timestamp with that name already exists
  """
  @spec add_new_create_timestamp(Spark.Dsl.Builder.input(), name :: atom, opts :: Keyword.t()) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_create_timestamp(dsl_state, name, opts \\ []) do
    if Ash.Resource.Info.attribute(dsl_state, name) do
      dsl_state
    else
      add_create_timestamp(dsl_state, name, opts)
    end
  end

  @doc """
  Builds and adds a create_timestamp to a resource
  """
  @spec add_create_timestamp(
          Spark.Dsl.Builder.input(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_create_timestamp(dsl_state, name, opts \\ []) do
    with {:ok, create_timestamp} <- build_create_timestamp(name, opts) do
      Transformer.add_entity(dsl_state, [:attributes], create_timestamp, type: :append)
    end
  end

  @doc """
  Builds an create_timestamp with the given name, type, and options
  """
  @spec build_create_timestamp(name :: atom, opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Attribute.t()} | {:error, term}
  def build_create_timestamp(name, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:attributes],
      :create_timestamp,
      Keyword.merge(opts, name: name)
    )
  end

  @doc """
  Builds and adds an attribute unless an attribute with that name already exists
  """
  @spec add_new_attribute(
          Spark.Dsl.Builder.input(),
          name :: atom,
          type :: Ash.Type.t(),
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_attribute(dsl_state, name, type, opts \\ []) do
    if Ash.Resource.Info.attribute(dsl_state, name) do
      {:ok, dsl_state}
    else
      add_attribute(dsl_state, name, type, opts)
    end
  end

  @doc """
  Builds and adds an attribute to a resource
  """
  @spec add_attribute(
          Spark.Dsl.Builder.input(),
          name :: atom,
          type :: Ash.Type.t(),
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_attribute(dsl_state, name, type, opts \\ []) do
    with {:ok, attribute} <- build_attribute(name, type, opts) do
      Transformer.add_entity(dsl_state, [:attributes], attribute, type: :append)
    end
  end

  @doc """
  Builds an attribute with the given name, type, and options
  """
  @spec build_attribute(name :: atom, type :: Ash.Type.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Attribute.t()} | {:error, term}
  def build_attribute(name, type, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:attributes],
      :attribute,
      Keyword.merge(opts, name: name, type: type)
    )
  end

  @doc """
  Builds and adds a calculation unless a calculation with that name already exists
  """
  @spec add_new_calculation(
          Spark.Dsl.Builder.input(),
          name :: atom,
          type :: Ash.Type.t(),
          calculation :: module | {module, Keyword.t()} | Ash.Expr.t(),
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_calculation(dsl_state, name, type, calculation, opts \\ []) do
    if Ash.Resource.Info.calculation(dsl_state, name) do
      {:ok, dsl_state}
    else
      add_calculation(dsl_state, name, type, calculation, opts)
    end
  end

  @doc """
  Builds and adds a calculation to a resource
  """
  @spec add_calculation(
          Spark.Dsl.Builder.input(),
          name :: atom,
          type :: Ash.Type.t(),
          calculation :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_calculation(dsl_state, name, type, calculation, opts \\ []) do
    with {:ok, opts} <- handle_nested_builders(opts, [:arguments]),
         {:ok, calculation} <- build_calculation(name, type, calculation, opts) do
      Transformer.add_entity(dsl_state, [:calculations], calculation, type: :append)
    end
  end

  @doc """
  Builds a calculation with the given name, type, and options
  """
  @spec build_calculation(
          name :: atom,
          type :: Ash.Type.t(),
          calculation :: module | {module, Keyword.t()},
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Calculation.t()} | {:error, term}
  def build_calculation(name, type, calculation, opts \\ []) do
    opts = Keyword.put_new(opts, :arguments, [])

    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:calculations],
      :calculate,
      Keyword.merge(opts, name: name, type: type, calculation: calculation)
    )
  end

  @doc """
  Builds and adds an aggregate unless an aggregate with that name already exists
  """
  @spec add_new_aggregate(
          Spark.Dsl.Builder.input(),
          name :: atom,
          kind :: Ash.Query.Aggregate.kind(),
          relationship_path :: atom | [atom],
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_aggregate(dsl_state, name, kind, relationship_path, opts \\ []) do
    if Ash.Resource.Info.aggregate(dsl_state, name) do
      {:ok, dsl_state}
    else
      add_calculation(dsl_state, name, kind, relationship_path, opts)
    end
  end

  @doc """
  Builds and adds an aggregate to a resource
  """
  @spec add_aggregate(
          Spark.Dsl.Builder.input(),
          name :: atom,
          kind :: Ash.Query.Aggregate.kind(),
          relationship_path :: atom | [atom],
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_aggregate(dsl_state, name, kind, relationship_path, opts \\ []) do
    with {:ok, aggregate} <- build_aggregate(name, kind, relationship_path, opts) do
      Transformer.add_entity(dsl_state, [:aggregates], aggregate, type: :append)
    end
  end

  @doc """
  Builds a calculation with the given name, type, and options
  """
  @spec build_aggregate(
          name :: atom,
          kind :: Ash.Query.Aggregate.kind(),
          relationship_path :: atom | [atom],
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.Aggregate.t()} | {:error, term}
  def build_aggregate(name, kind, relationship_path, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:aggregates],
      kind,
      Keyword.merge(opts, name: name, relationship_path: List.wrap(relationship_path))
    )
  end

  @doc """
  Builds and adds an interface unless an interface with that name already exists
  """
  @spec add_new_interface(
          Spark.Dsl.Builder.input(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_interface(dsl_state, name, opts \\ []) do
    if Ash.Resource.Info.interface(dsl_state, name) do
      {:ok, dsl_state}
    else
      add_interface(dsl_state, name, opts)
    end
  end

  @doc """
  Builds and adds an interface to a resource
  """
  @spec add_interface(
          Spark.Dsl.Builder.input(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_interface(dsl_state, name, opts \\ []) do
    with {:ok, interface} <- build_interface(name, opts) do
      Transformer.add_entity(dsl_state, [:code_interface], interface, type: :append)
    end
  end

  @doc """
  Builds an interface with the given name, type, and options
  """
  @spec build_interface(name :: atom, opts :: Keyword.t()) ::
          {:ok, Ash.Resource.Interface.t()} | {:error, term}
  def build_interface(name, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:code_interface],
      :define,
      Keyword.merge(opts, name: name)
    )
  end

  @doc """
  Builds and adds an calculation interface unless an calculation interface with that name already exists
  """
  @spec add_new_calculation_interface(
          Spark.Dsl.Builder.input(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_new_calculation_interface(dsl_state, name, opts \\ []) do
    if Ash.Resource.Info.calculation_interface(dsl_state, name) do
      {:ok, dsl_state}
    else
      add_calculation_interface(dsl_state, name, opts)
    end
  end

  @doc """
  Builds and adds an calculation interface to a resource
  """
  @spec add_calculation_interface(
          Spark.Dsl.Builder.input(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_calculation_interface(dsl_state, name, opts \\ []) do
    with {:ok, calculation_interface} <- build_calculation_interface(name, opts) do
      Transformer.add_entity(dsl_state, [:code_interface], calculation_interface, type: :append)
    end
  end

  @doc """
  Builds an calculation interface with the given name, type, and options
  """
  @spec build_calculation_interface(name :: atom, opts :: Keyword.t()) ::
          {:ok, Ash.Resource.CalculationInterface.t()} | {:error, term}
  def build_calculation_interface(name, opts \\ []) do
    Transformer.build_entity(
      Ash.Resource.Dsl,
      [:code_interface],
      :define_calculation,
      Keyword.merge(opts, name: name)
    )
  end
end

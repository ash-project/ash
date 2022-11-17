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
          Spark.Dsl.t(),
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
          Spark.Dsl.t(),
          type :: Ash.Resource.Actions.action_type(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_action(dsl_state, type, name, opts \\ []) do
    with {:ok, action} <- build_action(type, name, opts) do
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
    with {:ok, opts} <- handle_nested_builders(opts, [:changes, :arguments, :metadata]) do
      Transformer.build_entity(
        Ash.Resource.Dsl,
        [:actions],
        type,
        Keyword.merge(opts, name: name)
      )
    end
  end

  @doc """
  Builds and adds an action
  """
  @spec add_change(
          Spark.Dsl.t(),
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

    with {:ok, change} <- build_change(ref, opts) do
      Transformer.add_entity(dsl_state, [:changes], change)
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
      Transformer.add_entity(dsl_state, [:attributes], update_timestamp)
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
          Spark.Dsl.t(),
          name :: atom,
          opts :: Keyword.t()
        ) ::
          Spark.Dsl.Builder.result()
  defbuilder add_create_timestamp(dsl_state, name, opts \\ []) do
    with {:ok, create_timestamp} <- build_create_timestamp(name, opts) do
      Transformer.add_entity(dsl_state, [:attributes], create_timestamp)
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
      Transformer.add_entity(dsl_state, [:attributes], attribute)
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
end

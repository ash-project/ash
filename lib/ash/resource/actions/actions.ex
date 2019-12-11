defmodule Ash.Resource.Actions do
  @moduledoc """
  DSL components for declaring resource actions.

  All manipulation of data through the underlying data layer happens through actions.
  There are four types of action: `create`, `read`, `update`, and `delete`. You may
  recognize these from the acronym `CRUD`. You can have multiple actions of the same
  type, as long as they have different names. This is the primary mechanism for customizing
  your resources to conform to your business logic. It is normal and expected to have
  multiple actions of each type in a large application.

  If you have multiple actions of the same type, one of them must be designated as the
  primary action for that type, via: `primary?: true`. This tells the ash what to do
  if an action of that type is requested, but no specific action name is given.

  Authorization in ash is done via supplying a list of rules to actions in the
  `rules` option. To understand rules and authorization, see the documentation in `Ash.Authorization`
  """

  @doc false
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions

      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Authorization.Rule, only: []
    end
  end

  @doc """
  Declares a `create` action. For calling this action, see the `Ash.Api` documentation.

  #{Ashton.document(Ash.Resource.Actions.Create.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  create :register, primary?: true
  ```
  """
  defmacro create(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      unless is_atom(name) do
        raise Ash.Error.ResourceDslError,
          message: "action name must be an atom",
          path: [:actions, :create]
      end

      case Ash.Resource.Actions.Create.new(name, opts) do
        {:ok, action} ->
          @actions action

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:actions, :create, name]
      end
    end
  end

  @doc """
  Declares a `read` action. For calling this action, see the `Ash.Api` documentation.

  #{Ashton.document(Ash.Resource.Actions.Read.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  read :read_all, primary?: true
  ```
  """
  defmacro read(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      unless is_atom(name) do
        raise Ash.Error.ResourceDslError,
          message: "action name must be an atom",
          path: [:actions, :read]
      end

      case Ash.Resource.Actions.Read.new(name, opts) do
        {:ok, action} ->
          @actions action

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:actions, :read, name]
      end
    end
  end

  @doc """
  Declares an `update` action. For calling this action, see the `Ash.Api` documentation.

  #{Ashton.document(Ash.Resource.Actions.Update.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  update :flag_for_review, primary?: true
  ```
  """
  defmacro update(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      unless is_atom(name) do
        raise Ash.Error.ResourceDslError,
          message: "action name must be an atom",
          path: [:actions, :update]
      end

      case Ash.Resource.Actions.Update.new(name, opts) do
        {:ok, action} ->
          @actions action

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:actions, :update, name]
      end
    end
  end

  @doc """
  Declares an `destroy` action. For calling this action, see the `Ash.Api` documentation.

  #{Ashton.document(Ash.Resource.Actions.Destroy.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  destroy :soft_delete, primary?: true
  ```
  """
  defmacro destroy(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      unless is_atom(name) do
        raise Ash.Error.ResourceDslError,
          message: "action name must be an atom",
          path: [:actions, :destroy]
      end

      case Ash.Resource.Actions.Destroy.new(name, opts) do
        {:ok, action} ->
          @actions action

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:actions, :destroy, name]
      end
    end
  end
end

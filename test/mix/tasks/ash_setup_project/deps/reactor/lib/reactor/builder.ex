# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Builder do
  @moduledoc """
  Build a new Reactor programmatically.

  You don't _have_ to use the Reactor DSL to create a Reactor.  The functions in
  this module allow you to define a Reactor programmatically.  This is
  especially useful if you need to create a reactor dynamically (maybe based on
  a UI such as [React Flow](https://reactflow.dev/)).

  ## Example

  ```elixir
  reactor = Builder.new()
  {:ok, reactor} = Builder.add_input(reactor, :name)
  argument = Argument.from_input(:name)
  {:ok, reactor} = Builder.add_step(reactor, :greet, [argument])
  {:ok, reactor} = Builder.return(reactor, :greet)
  ```
  """

  alias Reactor.{Argument, Builder, Middleware, Step}
  import Reactor, only: :macros
  import Reactor.Utils

  @type step_options :: [
          async? | description | guards | max_retries | arguments_transform | context | ref
        ]

  @typedoc "Should the step be run asynchronously?"
  @type async? :: {:async?, boolean | (keyword -> boolean)}

  @typedoc "An optional step description"
  @type description :: {:description, nil | String.t()}

  @typedoc "How many times is the step allowed to retry?"
  @type max_retries :: {:max_retries, :infinity | non_neg_integer()}

  @typedoc "Optionally transform all the arguments into new arguments"
  @type arguments_transform ::
          {:transform,
           nil | (%{optional(atom) => any} -> %{optional(atom) => any}) | {module, keyword} | mfa}

  @type ref :: {:ref, :step_name | :make_ref}
  @type guards :: {:guards, [Reactor.Guard.Build.t()]}

  @typedoc "Optional context which will be merged with the reactor context when calling this step."
  @type context :: Reactor.context()

  @type step_argument :: Argument.t() | {atom, {:input | :result, any}}
  @type impl :: module | {module, keyword}

  @doc """
  Build a new, empty Reactor.

  Optionally an identifier for the Reactor. This is primarily used for recursive
  composition tracking.
  """
  @spec new(any) :: Reactor.t()
  def new(id \\ make_ref()),
    do: %Reactor{id: id, context: %{private: %{composed_reactors: MapSet.new([id])}}}

  @doc """
  Add a named input to the Reactor.

  This both places the input in the Reactor for later input validation and adds
  steps to the Reactor which will emit and (possibly) transform the input.
  """
  @spec add_input(Reactor.t(), any, Builder.Input.options()) :: {:ok, Reactor.t()} | {:error, any}
  def add_input(reactor, name, options \\ nil)

  def add_input(reactor, _name, _options) when not is_reactor(reactor),
    do: {:error, argument_error(:reactor, "not a Reactor", reactor)}

  def add_input(reactor, name, options),
    do: Builder.Input.add_input(reactor, name, options)

  @doc """
  Raising version of `add_input/2..3`.
  """
  @spec add_input!(Reactor.t(), any, Builder.Input.options()) :: Reactor.t() | no_return
  def add_input!(reactor, name, options \\ nil)

  def add_input!(reactor, name, options) do
    case add_input(reactor, name, options) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Add a step to the Reactor.

  Add a new step to the Reactor.  Rewrites input arguments to use the result of
  the input steps and injects transformation steps as required.
  """
  @spec add_step(
          Reactor.t(),
          name :: any,
          impl,
          [step_argument],
          step_options
        ) :: {:ok, Reactor.t()} | {:error, any}

  def add_step(reactor, name, impl, arguments \\ [], options \\ [])

  def add_step(reactor, _name, _impl, _arguments, _options) when not is_reactor(reactor),
    do: {:error, argument_error(:reactor, "not a Reactor", reactor)}

  def add_step(_reactor, _name, _impl, arguments, _options) when not is_list(arguments),
    do: {:error, argument_error(:arguments, "not a list", arguments)}

  def add_step(_reactor, _name, _impl, _arguments, options) when not is_list(options),
    do: {:error, argument_error(:options, "not a list", options)}

  def add_step(reactor, name, impl, arguments, options),
    do: Builder.Step.add_step(reactor, name, impl, arguments, options)

  @doc """
  Raising version of `add_step/3..5`.
  """
  @spec add_step!(Reactor.t(), name :: any, impl, [step_argument], step_options) ::
          Reactor.t() | no_return
  def add_step!(reactor, name, impl, arguments \\ [], options \\ [])

  def add_step!(reactor, name, impl, arguments, options) do
    case add_step(reactor, name, impl, arguments, options) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Build a step which can be added to a reactor at runtime.

  Note that the built step doesn't support transformations - you should add an
  additional step to do the transformation needed (this is what `add_step/5`
  does anyway).
  """
  @spec new_step(any, impl, [step_argument], step_options) ::
          {:ok, Step.t()} | {:error, any}
  def new_step(name, impl, arguments \\ [], options \\ [])

  def new_step(_name, _impl, arguments, _options) when not is_list(arguments),
    do: {:error, argument_error(:arguments, "not a list", arguments)}

  def new_step(_name, _impl, _arguments, options) when not is_list(options),
    do: {:error, argument_error(:options, "not a list", options)}

  def new_step(name, impl, arguments, options),
    do: Builder.Step.new_step(name, impl, arguments, options)

  @doc """
  Raising version of `new_step/2..4`.
  """
  @spec new_step!(any, impl, [step_argument], step_options) :: Step.t() | no_return
  def new_step!(name, impl, arguments \\ [], options \\ [])

  def new_step!(name, impl, arguments, options) do
    case new_step(name, impl, arguments, options) do
      {:ok, step} -> step
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Specify the return value of the Reactor.

  The return value must be the name of a step.
  """
  @spec return(Reactor.t(), any) :: {:ok, Reactor.t()} | {:error, any}
  def return(reactor, name) do
    step_names =
      reactor.steps
      |> Enum.map(& &1.name)

    if name in step_names do
      {:ok, %{reactor | return: name}}
    else
      {:error, argument_error(:name, "not an existing step name.", name)}
    end
  end

  @doc """
  Raising version of `return/2`.
  """
  @spec return!(Reactor.t(), any) :: Reactor.t() | no_return
  def return!(reactor, name) do
    case return(reactor, name) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Compose another Reactor inside this one.

  Whenever possible this function will extract the steps from inner Reactor and
  place them inside the parent Reactor.  In order to achieve this the composer
  will rename the steps to ensure that there are no conflicts.

  If you're attempting to create a recursive Reactor (ie compose a Reactor
  within itself) then this will be detected and runtime composition will be used
  instead.  See `Reactor.Step.Compose` for more details.
  """
  @spec compose(Reactor.t(), atom, Reactor.t() | module, [step_argument], Keyword.t()) ::
          {:ok, Reactor.t()} | {:error, any}
  def compose(reactor, name, inner_reactor, arguments \\ [], options \\ [])

  def compose(reactor, _name, _inner_reactor, _arguments, _options) when not is_reactor(reactor),
    do: {:error, argument_error(:reactor, "not a Reactor", reactor)}

  def compose(_reactor, name, _inner_reactor, _arguments, _options) when not is_atom(name),
    do: {:error, argument_error(:name, "not an atom", name)}

  def compose(_reactor, _name, inner_reactor, _arguments, _options)
      when not is_reactor(inner_reactor) and not is_atom(inner_reactor),
      do: {:error, argument_error(:inner_reactor, "not a Reactor", inner_reactor)}

  def compose(_reactor, _name, _inner_reactor, arguments, _options) when not is_list(arguments),
    do: {:error, argument_error(:arguments, "not a list", arguments)}

  def compose(_reactor, _name, _inner_reactor, _arguments, options) when not is_list(options),
    do: {:error, argument_error(:options, "not a list", options)}

  def compose(reactor, name, inner_reactor, arguments, options),
    do: Builder.Compose.compose(reactor, name, inner_reactor, arguments, options)

  @doc """
  Raising version of `compose/4`.
  """
  @spec compose!(Reactor.t(), atom, Reactor.t() | module, [step_argument], Keyword.t()) ::
          Reactor.t() | no_return
  def compose!(reactor, name, inner_reactor, arguments, options \\ []) do
    case compose(reactor, name, inner_reactor, arguments, options) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Recurse a Reactor until an exit condition is met or maximum iterations are reached.

  Recursion takes the output of one execution of the reactor and feeds it as input
  to the next execution, continuing until either:
  1. The exit_condition function returns true when applied to the latest result
  2. The maximum number of iterations is reached
  3. An error occurs during execution

  This provides a powerful way to implement iterative algorithms where the 
  processing continues until a convergence condition is met.
  """
  @spec recurse(Reactor.t(), atom, Reactor.t() | module, [step_argument], Keyword.t()) ::
          {:ok, Reactor.t()} | {:error, any}
  def recurse(reactor, name, inner_reactor, arguments \\ [], options \\ [])

  def recurse(reactor, _name, _inner_reactor, _arguments, _options) when not is_reactor(reactor),
    do: {:error, argument_error(:reactor, "not a Reactor", reactor)}

  def recurse(_reactor, name, _inner_reactor, _arguments, _options) when not is_atom(name),
    do: {:error, argument_error(:name, "not an atom", name)}

  def recurse(_reactor, _name, inner_reactor, _arguments, _options)
      when not is_reactor(inner_reactor) and not is_atom(inner_reactor),
      do: {:error, argument_error(:inner_reactor, "not a Reactor", inner_reactor)}

  def recurse(_reactor, _name, _inner_reactor, arguments, _options) when not is_list(arguments),
    do: {:error, argument_error(:arguments, "not a list", arguments)}

  def recurse(_reactor, _name, _inner_reactor, _arguments, options) when not is_list(options),
    do: {:error, argument_error(:options, "not a list", options)}

  def recurse(reactor, name, inner_reactor, arguments, options),
    do: Builder.Recurse.recurse(reactor, name, inner_reactor, arguments, options)

  @doc """
  Raising version of `recurse/4`.
  """
  @spec recurse!(Reactor.t(), atom, Reactor.t() | module, [step_argument], Keyword.t()) ::
          Reactor.t() | no_return
  def recurse!(reactor, name, inner_reactor, arguments, options \\ []) do
    case recurse(reactor, name, inner_reactor, arguments, options) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Add a middleware to the Reactor.

  Returns an error if the middleware is already present on the Reactor.
  """
  @spec add_middleware(Reactor.t(), Middleware.t()) :: {:ok, Reactor.t()} | {:error, any}
  def add_middleware(reactor, middleware) when is_reactor(reactor) and is_atom(middleware) do
    with :ok <- assert_unique_middleware(reactor, middleware),
         :ok <- assert_is_middleware(middleware) do
      {:ok, %{reactor | middleware: [middleware | reactor.middleware]}}
    end
  end

  @doc """
  Raising version of `add_middleware/2`.
  """
  @spec add_middleware!(Reactor.t(), Middleware.t()) :: Reactor.t() | no_return
  def add_middleware!(reactor, middleware) do
    case add_middleware(reactor, middleware) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Ensure that a middleware is present on the Reactor.
  """
  @spec ensure_middleware(Reactor.t(), Middleware.t()) :: {:ok, Reactor.t()} | {:error, any}
  def ensure_middleware(reactor, middleware) when is_reactor(reactor) and is_atom(middleware) do
    with false <- middleware in reactor.middleware,
         :ok <- assert_is_middleware(middleware) do
      {:ok, %{reactor | middleware: [middleware | reactor.middleware]}}
    else
      true -> {:ok, reactor}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Raising version of `ensure_middleware/2`.
  """
  @spec ensure_middleware!(Reactor.t(), Middleware.t()) :: Reactor.t() | no_return
  def ensure_middleware!(reactor, middleware) do
    case ensure_middleware(reactor, middleware) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  defp assert_unique_middleware(reactor, middleware) do
    if middleware in reactor.middleware do
      argument_error(:middleware, "Middleware is already registered on this Reactor.", middleware)
    else
      :ok
    end
  end

  defp assert_is_middleware(middleware) do
    if Spark.implements_behaviour?(middleware, Middleware) do
      assert_middleware_process_callbacks(
        middleware,
        function_exported?(middleware, :get_process_context, 0),
        function_exported?(middleware, :set_process_context, 1)
      )
    else
      {:error,
       argument_error(
         :middleware,
         "Module does not implement the `Middleware` behaviour.",
         middleware
       )}
    end
  end

  defp assert_middleware_process_callbacks(middleware, true, false) do
    {:error,
     argument_error(
       :middleware,
       "When `get_process_context/0` is implemented `set_process_context/1` must also be implemented.",
       middleware
     )}
  end

  defp assert_middleware_process_callbacks(middleware, false, true) do
    {:error,
     argument_error(
       :middleware,
       "When `set_process_context/1` is implemented `get_process_context/0` must also be implemented.",
       middleware
     )}
  end

  defp assert_middleware_process_callbacks(_, _, _), do: :ok
end

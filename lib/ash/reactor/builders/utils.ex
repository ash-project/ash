defmodule Ash.Reactor.BuilderUtils do
  @moduledoc false

  alias Ash.Reactor.MergeInputsStep
  alias Reactor.{Argument, Builder, Step.ReturnAllArguments, Template}

  @doc false
  @spec build_input_arguments(Reactor.t(), Ash.Reactor.action()) ::
          {:ok, Reactor.t(), Enum.t()} | {:error, any}
  def build_input_arguments(reactor, action) when action.inputs == [],
    do: {:ok, reactor, [%Argument{name: :input, source: %Template.Value{value: %{}}}]}

  def build_input_arguments(reactor, action) do
    Enum.reduce_while(action.inputs, {:ok, reactor, []}, fn input, {:ok, reactor, result_names} ->
      arguments = Enum.map(input.template, &%Argument{name: elem(&1, 0), source: elem(&1, 1)})
      name = {:__input__, action.name, Map.keys(input.template)}

      case Builder.add_step(reactor, name, ReturnAllArguments, arguments,
             transform: input.transform,
             ref: :step_name
           ) do
        {:ok, reactor} -> {:cont, {:ok, reactor, [name | result_names]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:error, reason} ->
        {:error, reason}

      {:ok, reactor, []} ->
        {:ok, reactor, []}

      {:ok, reactor, [result_name]} ->
        {:ok, reactor, [input: {:result, result_name}]}

      {:ok, reactor, result_names} ->
        arguments =
          result_names
          |> Enum.with_index()
          |> Enum.map(&{input_prepend_atom(elem(&1, 1)), {:result, elem(&1, 0)}})

        name = {:__input__, action.name, :__merge__, result_names}

        case Builder.add_step(reactor, name, MergeInputsStep, arguments, ref: :step_name) do
          {:ok, reactor} -> {:ok, reactor, [input: {:result, name}]}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp input_prepend_atom(value) do
    "input_#{value}"
    |> String.to_atom()
  end

  @doc false
  @spec maybe_append(Enum.t(), Enum.t() | nil) :: Enum.t()
  def maybe_append(lhs, nil), do: lhs
  def maybe_append(lhs, rhs), do: Enum.concat(lhs, [rhs])

  @doc false
  @spec ensure_hooked(Reactor.t()) :: {:ok, Reactor.t()} | {:error, any}
  def ensure_hooked(reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor, :init, &Ash.Reactor.Notifications.init_hook/1),
         {:ok, reactor} <- ensure_hooked(reactor, :halt, &Ash.Reactor.Notifications.halt_hook/1),
         {:ok, reactor} <-
           ensure_hooked(reactor, :complete, &Ash.Reactor.Notifications.complete_hook/2) do
      ensure_hooked(reactor, :error, &Ash.Reactor.Notifications.error_hook/2)
    end
  end

  # Ensure that each hook is only added one time to the reactor.
  defp ensure_hooked(reactor, hook_type, hook_fn) do
    hooks = Map.get(reactor.hooks, hook_type, [])

    if hook_fn in hooks do
      reactor
    else
      really_hook(reactor, hook_type, hook_fn)
    end
  end

  defp really_hook(reactor, :init, hook_fn), do: Builder.on_init(reactor, hook_fn)
  defp really_hook(reactor, :halt, hook_fn), do: Builder.on_halt(reactor, hook_fn)
  defp really_hook(reactor, :complete, hook_fn), do: Builder.on_complete(reactor, hook_fn)
  defp really_hook(reactor, :error, hook_fn), do: Builder.on_error(reactor, hook_fn)
end

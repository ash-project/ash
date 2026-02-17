defmodule Iter.Iterable.Enumerable do
  defstruct pid: nil

  @moduledoc """
  Can we convert a enum into an iterable?  Let's find out.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{pid: pid}

  use GenServer

  @doc "Wrap an enumerable in a genserver"
  @spec new(Enumerable.t()) :: t
  def new(enum) do
    case GenServer.start_link(__MODULE__, enum) do
      {:ok, pid} -> %__MODULE__{pid: pid}
      {:error, reason} -> raise reason
    end
  end

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(%{pid: pid} = enum) do
      case GenServer.call(pid, :next, :infinity) do
        {:ok, element} -> {:ok, element, enum}
        :done -> :done
      end
    catch
      :exit, _ -> :done
    end
  end

  defimpl IntoIterable do
    @doc false
    @impl true
    def into_iterable(self), do: self
  end

  @doc false
  @impl GenServer
  def init(enum) do
    {:ok, %{enum: enum}}
  end

  @doc false
  @impl GenServer
  def handle_call(:next, from, %{enum: enum}) do
    {:ok, pid} = reduce_in_task(enum, self())

    Process.monitor(pid)

    {:noreply, %{next_reply_to: from, source: pid}}
  end

  def handle_call(:next, _from, %{element: element, element_reply_to: from} = state) do
    GenServer.reply(from, :ok)
    {:reply, {:ok, element}, Map.drop(state, [:element, :element_reply_to])}
  end

  def handle_call(:next, _from, %{source: :done} = state) do
    {:stop, :normal, :done, state}
  end

  def handle_call(:next, from, state) do
    {:noreply, Map.put(state, :next_reply_to, from)}
  end

  def handle_call({:element, element}, from, state) do
    case Map.pop(state, :next_reply_to) do
      {nil, state} ->
        state = Map.merge(state, %{element: element, element_reply_to: from})
        {:noreply, state}

      {from, state} ->
        GenServer.reply(from, {:ok, element})
        {:reply, :ok, state}
    end
  end

  @doc false
  @impl true
  def handle_info({:DOWN, _, :process, pid, _}, %{source: pid} = state)
      when is_map_key(state, :next_reply_to) do
    GenServer.reply(state.next_reply_to, :done)

    {:stop, :normal}
  end

  def handle_info({:DOWN, _, :process, pid, _}, %{source: pid} = state) do
    {:noreply, %{state | source: :done}}
  end

  defp reduce_in_task(enum, receiver) do
    Task.start_link(fn ->
      Enum.reduce_while(enum, :ok, &task_reducer(&1, &2, receiver))
    end)
  end

  defp task_reducer(element, :ok, receiver) do
    case GenServer.call(receiver, {:element, element}, :infinity) do
      :ok -> {:cont, :ok}
      :halt -> {:halt, :ok}
    end
  end
end

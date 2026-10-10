# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Test do
  @moduledoc """
  Test helpers for asserting on emitted signals.

  `assert_emits_signal/4` and `refute_emits_signal/4` check the signals emitted while running a
  function, and need no setup:

  ```elixir
  import Ash.Signals.Test

  test "creating a post emits post_created" do
    post =
      assert_emits_signal MyApp.Blog.Signals, :post_created, %{title: "Hello"}, fn ->
        MyApp.Blog.create_post!("Hello")
      end

    assert post.title == "Hello"
  end
  ```

  `assert_signal_emitted/4` and `refute_signal_emitted/4` check the signals emitted so far, and
  require calling `capture_signals/1` first:

  ```elixir
  setup :capture_signals

  test "creating a post emits post_created" do
    %{id: post_id} = MyApp.Blog.create_post!("Hello")

    assert_signal_emitted MyApp.Blog.Signals, :post_created, %{post_id: ^post_id}
  end
  ```

  Only signals emitted by the test process, or by processes it started (that have it in their
  `$callers`, like tasks and LiveViews under test), are seen. No other process is sent anything,
  and listeners still run as usual.

  Patterns match against the signal struct, and may use pins or bind variables.

  To also match on who emitted a signal, receive the messages sent after `capture_signals/1`
  directly. They have the shape `{:ash_signal_emitted, signal_module, signal_name, signal,
  metadata}`, where `metadata` is the metadata of the `[:ash, :signal, :emitted]` telemetry
  event, see `Ash.Signals`.
  """

  @event [:ash, :signal, :emitted]
  @capturing {__MODULE__, :capturing?}

  @doc """
  Sends signals emitted by the current test, or by processes it started, to the test process,
  for `assert_signal_emitted/4` and `refute_signal_emitted/4`.

  Can be used as `setup :capture_signals`. Stops when the test exits.
  """
  @spec capture_signals(map()) :: :ok
  def capture_signals(_context \\ %{}) do
    if !Process.get(@capturing) do
      test_pid = self()
      id = {__MODULE__, test_pid, make_ref()}

      :ok = :telemetry.attach(id, @event, &__MODULE__.handle_event/4, test_pid)
      ExUnit.Callbacks.on_exit(fn -> :telemetry.detach(id) end)
      Process.put(@capturing, true)
    end

    :ok
  end

  @doc false
  def handle_event(@event, _measurements, metadata, {test_pid, ref}) do
    if from_test?(test_pid) do
      send(test_pid, {ref, metadata.signal_module, metadata.name, metadata.signal})
    end
  end

  def handle_event(@event, _measurements, metadata, test_pid) do
    if from_test?(test_pid) do
      send(
        test_pid,
        {:ash_signal_emitted, metadata.signal_module, metadata.name, metadata.signal, metadata}
      )
    end
  end

  defp from_test?(test_pid) do
    self() == test_pid or test_pid in Process.get(:"$callers", [])
  end

  @doc """
  Runs the function, asserts that it emitted a signal matching the given pattern, and returns
  the function's result. Variables bound by the pattern are available afterwards.

  ```elixir
  assert_emits_signal MyApp.Blog.Signals, :post_created, fn ->
    MyApp.Blog.create_post!("Hello")
  end

  post =
    assert_emits_signal MyApp.Blog.Signals, :post_created, %{post_id: post_id}, fn ->
      MyApp.Blog.create_post!("Hello")
    end

  assert post.id == post_id
  ```
  """
  defmacro assert_emits_signal(signal_module, signal_name, pattern \\ quote(do: _), fun) do
    quote do
      signal_module = unquote(signal_module)
      signal_name = unquote(signal_name)

      {result, signals} =
        Ash.Signals.Test.__run_capturing__(unquote(fun), signal_module, signal_name)

      signal =
        Enum.find(signals, &match?(unquote(ignore_vars(pattern)), &1)) ||
          raise ExUnit.AssertionError,
            message:
              Ash.Signals.Test.__not_emitted__(
                signal_module,
                signal_name,
                unquote(Macro.to_string(pattern)),
                signals
              )

      unquote(pattern) = signal
      result
    end
  end

  @doc """
  Runs the function, asserts that it did not emit a signal matching the given pattern, and
  returns the function's result.

  ```elixir
  refute_emits_signal MyApp.Blog.Signals, :post_created, fn ->
    MyApp.Blog.create_draft!("Hello")
  end
  ```
  """
  defmacro refute_emits_signal(signal_module, signal_name, pattern \\ quote(do: _), fun) do
    quote do
      signal_module = unquote(signal_module)
      signal_name = unquote(signal_name)

      {result, signals} =
        Ash.Signals.Test.__run_capturing__(unquote(fun), signal_module, signal_name)

      if signal = Enum.find(signals, &match?(unquote(ignore_vars(pattern)), &1)) do
        raise ExUnit.AssertionError,
          message:
            "Expected signal #{inspect(signal_name)} from #{inspect(signal_module)} matching " <>
              unquote(Macro.to_string(pattern)) <>
              " not to be emitted, but got:\n\n    #{inspect(signal)}"
      end

      result
    end
  end

  @doc """
  Asserts that a signal matching the given pattern was emitted since `capture_signals/1` was
  called, and returns the signal.

  ```elixir
  assert_signal_emitted MyApp.Blog.Signals, :post_created
  assert_signal_emitted MyApp.Blog.Signals, :post_created, %{title: "Hello"}

  %{post_id: post_id} = assert_signal_emitted MyApp.Blog.Signals, :post_created
  ```

  The optional timeout, defaulting to `0`, waits for signals emitted by other processes.
  """
  defmacro assert_signal_emitted(
             signal_module,
             signal_name,
             pattern \\ quote(do: _),
             timeout \\ 0
           ) do
    quote do
      Ash.Signals.Test.__ensure_capturing__!(:assert_signal_emitted)
      signal_module = unquote(signal_module)
      signal_name = unquote(signal_name)

      {:ash_signal_emitted, _, _, signal, _} =
        ExUnit.Assertions.assert_receive(
          {:ash_signal_emitted, ^signal_module, ^signal_name, unquote(pattern), _},
          unquote(timeout),
          "Expected signal #{inspect(signal_name)} from #{inspect(signal_module)} matching " <>
            unquote(Macro.to_string(pattern)) <> " to be emitted."
        )

      signal
    end
  end

  @doc """
  Asserts that no signal matching the given pattern was emitted since `capture_signals/1` was
  called.

  ```elixir
  refute_signal_emitted MyApp.Blog.Signals, :post_created
  refute_signal_emitted MyApp.Blog.Signals, :post_created, %{title: "Draft"}
  ```

  The optional timeout, defaulting to `0`, waits for signals emitted by other processes.
  """
  defmacro refute_signal_emitted(
             signal_module,
             signal_name,
             pattern \\ quote(do: _),
             timeout \\ 0
           ) do
    quote do
      Ash.Signals.Test.__ensure_capturing__!(:refute_signal_emitted)
      signal_module = unquote(signal_module)
      signal_name = unquote(signal_name)

      ExUnit.Assertions.refute_receive(
        {:ash_signal_emitted, ^signal_module, ^signal_name, unquote(pattern), _},
        unquote(timeout),
        "Expected signal #{inspect(signal_name)} from #{inspect(signal_module)} matching " <>
          unquote(Macro.to_string(pattern)) <> " not to be emitted."
      )
    end
  end

  # Replaces the variables a pattern binds with `_`, keeping pinned ones, so it can be used to
  # find a match without warning about unused variables.
  defp ignore_vars({:^, _, _} = pinned), do: pinned

  defp ignore_vars({name, meta, context}) when is_atom(name) and is_atom(context),
    do: {:_, meta, context}

  defp ignore_vars({left, meta, right}),
    do: {ignore_vars(left), meta, ignore_vars(right)}

  defp ignore_vars({left, right}), do: {ignore_vars(left), ignore_vars(right)}
  defp ignore_vars(list) when is_list(list), do: Enum.map(list, &ignore_vars/1)
  defp ignore_vars(other), do: other

  @doc false
  # Runs the function, and returns its result with the matching signals it emitted, in order.
  def __run_capturing__(fun, signal_module, signal_name) do
    test_pid = self()
    ref = make_ref()
    id = {__MODULE__, test_pid, ref}

    :ok = :telemetry.attach(id, @event, &__MODULE__.handle_event/4, {test_pid, ref})

    result =
      try do
        fun.()
      after
        :telemetry.detach(id)
      end

    {result, receive_signals(ref, signal_module, signal_name, [])}
  end

  defp receive_signals(ref, signal_module, signal_name, acc) do
    receive do
      {^ref, ^signal_module, ^signal_name, signal} ->
        receive_signals(ref, signal_module, signal_name, [signal | acc])

      {^ref, _, _, _} ->
        receive_signals(ref, signal_module, signal_name, acc)
    after
      0 -> Enum.reverse(acc)
    end
  end

  @doc false
  def __not_emitted__(signal_module, signal_name, pattern, []) do
    "Expected signal #{inspect(signal_name)} from #{inspect(signal_module)} matching #{pattern} " <>
      "to be emitted, but no #{inspect(signal_name)} signals were emitted."
  end

  def __not_emitted__(signal_module, signal_name, pattern, signals) do
    "Expected signal #{inspect(signal_name)} from #{inspect(signal_module)} matching #{pattern} " <>
      "to be emitted, but none of the emitted #{inspect(signal_name)} signals matched:\n\n" <>
      Enum.map_join(signals, "\n", &"    #{inspect(&1)}")
  end

  @doc false
  def __ensure_capturing__!(helper) do
    if !Process.get(@capturing) do
      raise ExUnit.AssertionError,
        message:
          "`#{helper}` only sees signals emitted after `Ash.Signals.Test.capture_signals/1` is " <>
            "called in the test process, for example with `setup :capture_signals`. " <>
            "To check the signals emitted by a function instead, use `assert_emits_signal/4` " <>
            "or `refute_emits_signal/4`."
    end
  end
end

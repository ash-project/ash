# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Tracer.Simple do
  @moduledoc """
  A simple tracer that can send traces to the current process or call a module with the trace.
  """
  use Ash.Tracer

  defmodule Span do
    @moduledoc "A span produced by `Ash.Tracer.Simple`"
    defstruct [:type, :id, :parent_id, :name, :start, metadata: %{}, spans: []]
  end

  @doc """
  Starts a span in the current process and makes it the active span.

  If a span is already active, its ID becomes the new span's parent ID.
  Otherwise, the parent ID comes from the context installed with
  `set_span_context/1`. Pair each call with `stop_span/0` in the same process,
  or let `Ash.Tracer.span/4` manage the lifecycle.
  """
  @impl true
  def start_span(type, name) do
    context = get_span_context()
    id = Ash.UUID.generate()

    spans = Process.get(:tracer_spans, [])

    parent_id =
      case spans do
        [%{id: id} | _] ->
          id

        _ ->
          context[:parent_id]
      end

    if is_nil(context[:parent_id]) do
      set_span_context(Map.put(context, :parent_id, id))
    end

    Process.put(:tracer_spans, [
      %Span{
        type: type,
        id: id,
        parent_id: parent_id,
        name: name,
        start: System.monotonic_time(:microsecond)
      }
      | spans
    ])

    :ok
  end

  @doc """
  Finishes the active span and sends `{:span, span}` to the context's recipient.

  The completed span includes a `:duration` key in microseconds. Its `:start`
  field is a monotonic timestamp in microseconds. It can be used for elapsed
  time calculations. Call `gather_spans/0` in the receiving process to collect
  completed spans.
  """
  @impl true
  def stop_span do
    [span | spans] = Process.get(:tracer_spans, [])
    Process.put(:tracer_spans, spans)

    duration = System.monotonic_time(:microsecond) - span.start
    span = Map.put(span, :duration, duration)
    context = get_span_context()

    if context[:parent_id] == span.id do
      set_span_context(Map.put(context, :parent_id, nil))
    end

    send(context[:pid], {:span, span})
    :ok
  end

  @doc """
  Returns the context to carry into another process.

  The context contains `:pid`, the recipient of completed spans, and may
  contain `:parent_id`, the ID to use when opening a span without a local
  parent. If no context has been set, the current process is the recipient.
  """
  @impl true
  def get_span_context do
    Process.get(:span_context) || %{pid: self()}
  end

  @doc """
  Installs span context in the current process.

  Pass a context from `get_span_context/0` to send spans from another process
  to the same recipient and carry over the parent ID. To choose a recipient
  for a new trace, set `%{pid: collector_pid}` before starting the first span.
  """
  @impl true
  def set_span_context(context) do
    Process.put(:span_context, context)
    :ok
  end

  @doc """
  Merges metadata into the active span, replacing values for matching keys.

  The active span is the most recently started span in the current process.
  Ash uses the type argument when dispatching to tracers.
  """
  @impl true
  def set_metadata(_type, metadata) do
    [span | spans] = Process.get(:tracer_spans, [])

    Process.put(
      :tracer_spans,
      [Map.update!(span, :metadata, &Map.merge(&1, metadata)) | spans]
    )

    :ok
  end

  @doc """
  Adds an exception under the `:error` key of the active span.

  `Ash.Tracer.span/4` calls this when its block raises.
  """
  @impl true
  def set_error(error) do
    [span | spans] = Process.get(:tracer_spans, [])
    Process.put(:tracer_spans, [Map.put(span, :error, error) | spans])
    :ok
  end

  @doc """
  Collects completed spans from the calling process's mailbox.

  Pass `tracer: Ash.Tracer.Simple` to an Ash action to inspect its spans.
  Gather them in the same process after the action returns. To trace
  preparations or changes as well, pass the option when building the query
  or changeset.

  Completed spans include their type, name, metadata, and duration in
  microseconds. This function uses `:id` and `:parent_id` to nest child spans
  under their parent's `:spans` key. Spans whose parents cannot be found are
  returned at the top level.
  Each call consumes the available span messages and returns immediately.
  If you start independent tasks, wait for them to finish before gathering
  their spans.

  ## Examples

      Ash.read!(MyApp.Post, tracer: Ash.Tracer.Simple)
      spans = Ash.Tracer.Simple.gather_spans()

      Enum.map(spans, fn span -> {span.name, span.duration} end)

  To include query preparation with the read:

      MyApp.Post
      |> Ash.Query.for_read(:read, %{}, tracer: Ash.Tracer.Simple)
      |> Ash.read!()

      Ash.Tracer.Simple.gather_spans()

  ## See also

  - `Ash.Tracer.span/4` for tracing your own code
  - `get_span_context/0` and `set_span_context/1` for sharing a recipient across processes
  - [Monitoring](/documentation/topics/advanced/monitoring.md)
  """
  def gather_spans do
    {spans, unparented} = do_gather_spans()
    spans ++ unparented
  end

  defp do_gather_spans(spans \\ [], unparented \\ []) do
    receive do
      {:span, span} ->
        {spans, unparented} =
          Enum.reduce([span | unparented], {spans, []}, fn span, {spans, unparented} ->
            case gather_span(spans, span) do
              {:ok, spans} ->
                {spans, unparented}

              :error ->
                {spans, [span | unparented]}
            end
          end)

        do_gather_spans(spans, Enum.reverse(unparented))
    after
      0 ->
        if Enum.empty?(unparented) do
          {spans, []}
        else
          case do_gather_spans(spans, unparented) do
            {^spans, ^unparented} ->
              {spans, unparented}

            {spans, unparented} ->
              do_gather_spans(spans, unparented)
          end
        end
    end
  end

  defp gather_span(spans, %{parent_id: parent_id} = gathering_span) do
    if parent_id do
      {new_spans, found?} =
        Enum.reduce(spans, {[], false}, fn
          span, {spans, true} ->
            {spans ++ [span], false}

          %{id: ^parent_id} = span, {spans, false} ->
            {spans ++ [%{span | spans: span.spans ++ [gathering_span]}], true}

          span, {spans, false} ->
            case gather_span(span.spans, gathering_span) do
              {:ok, new_spans} ->
                {spans ++ [%{span | spans: new_spans}], true}

              :error ->
                {spans ++ [span], false}
            end
        end)

      if found? do
        {:ok, new_spans}
      else
        :error
      end
    else
      {:ok, spans ++ [gathering_span]}
    end
  end
end

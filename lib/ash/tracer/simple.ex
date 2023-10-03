defmodule Ash.Tracer.Simple do
  @moduledoc """
  A simple tracer that can send traces to the current process or call a module with the trace.
  """
  use Ash.Tracer

  defmodule Span do
    @moduledoc "A span produced by `Ash.Tracer.Simple`"
    defstruct [:type, :id, :parent_id, :name, :start, metadata: %{}, spans: []]
  end

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
  end

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
  end

  @impl true
  def get_span_context do
    Process.get(:span_context) || %{pid: self()}
  end

  @impl true
  def set_span_context(context) do
    Process.put(:span_context, context)
  end

  @impl true
  def set_metadata(_type, metadata) do
    [span | spans] = Process.get(:tracer_spans, [])

    Process.put(
      :tracer_spans,
      [Map.update!(span, :metadata, &Map.merge(&1, metadata)) | spans]
    )
  end

  @impl true
  def set_error(error) do
    [span | spans] = Process.get(:tracer_spans, [])
    Process.put(:tracer_spans, [Map.put(span, :error, error) | spans])
  end

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

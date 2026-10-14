# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Graph do
  @moduledoc """
  Maps the signals of an application: the actions that emit each signal, and the listeners that
  react to it.

  Used by `mix ash.signals`, which prints the map as text or as a Mermaid flowchart.
  """

  @type emitter :: %{
          resource: Ash.Resource.t(),
          action: atom(),
          action_type: Ash.Resource.Actions.action_type(),
          emit: Ash.Resource.Actions.Emit.t()
        }

  @type entry :: %{
          signal_module: Ash.Signals.t(),
          signal: Ash.Signals.Signal.t(),
          emitters: [emitter()],
          listeners: [Ash.Resource.SignalListener.t()]
        }

  @doc """
  Returns an entry for each signal, with the actions that emit it and the listeners of it.

  Listeners are found wherever they are defined, with `Ash.Signals.Registry`. Emitters are found
  in the given resources, and in the resources that listen to signals, since there is no registry
  of emitters.

  ## Options

  * `:resources` - Resources to find emitters in. Defaults to `[]`.
  * `:signal_modules` - Only map the signals of these signal modules. Defaults to every signal
    module, found with `Ash.Signals.Registry.signal_modules/0`, or emitted or listened to by the
    resources.
  """
  @spec build(Keyword.t()) :: [entry()]
  def build(opts \\ []) do
    listeners = Ash.Signals.Registry.discover_all()

    emitters =
      (Keyword.get(opts, :resources, []) ++ Enum.map(listeners, & &1.resource))
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.flat_map(&emitters/1)

    signal_modules =
      opts[:signal_modules] ||
        Enum.uniq(
          Ash.Signals.Registry.signal_modules() ++
            Enum.map(emitters, & &1.emit.signal_module) ++
            Enum.map(listeners, & &1.signal_module)
        )

    emitters = Enum.group_by(emitters, &{&1.emit.signal_module, &1.emit.signal})
    listeners = Enum.group_by(listeners, &{&1.signal_module, &1.signal})

    for signal_module <- Enum.sort(signal_modules),
        signal <- Ash.Signals.Info.signals(signal_module) do
      %{
        signal_module: signal_module,
        signal: signal,
        emitters: Map.get(emitters, {signal_module, signal.name}, []),
        listeners: Map.get(listeners, {signal_module, signal.name}, [])
      }
    end
  end

  defp emitters(resource) do
    for action <- Ash.Resource.Info.actions(resource),
        emit <- Map.get(action, :emits, []) do
      %{resource: resource, action: action.name, action_type: action.type, emit: emit}
    end
  end

  @doc """
  Renders entries from `build/1` as text, grouped by signal module, with the emitters and
  listeners of each signal grouped by resource, and the descriptions of signals, emits and
  listeners under them.
  """
  @spec text([entry()]) :: String.t()
  def text(entries) do
    entries
    |> Enum.chunk_by(& &1.signal_module)
    |> Enum.map_join("\n", fn [%{signal_module: signal_module} | _] = entries ->
      widths =
        entries
        |> Enum.flat_map(fn entry ->
          Enum.map(entry.emitters, &emitter_row/1) ++ Enum.map(entry.listeners, &listener_row/1)
        end)
        |> Enum.reduce([0, 0], fn {_resource, [name, type, _details], _description},
                                  [name_width, type_width] ->
          [max(name_width, String.length(name)), max(type_width, String.length(type))]
        end)

      signals =
        Enum.map_join(entries, "\n", fn entry ->
          """

            #{inspect(entry.signal.name)} (#{entry.signal.phase})#{description(entry.signal.description, 4)}
              emitted by:
          #{rows(Enum.map(entry.emitters, &emitter_row/1), widths)}
              listened to by:
          #{rows(Enum.map(entry.listeners, &listener_row/1), widths)}\
          """
        end)

      inspect(signal_module) <> "\n" <> signals <> "\n"
    end)
  end

  defp rows([], _widths), do: "      (none)"

  defp rows(rows, [name_width, type_width]) do
    rows
    |> Enum.chunk_by(&elem(&1, 0))
    |> Enum.map_join("\n", fn [{resource, _, _} | _] = rows ->
      lines =
        Enum.map_join(rows, "\n", fn {_resource, [name, type, details], description} ->
          [String.pad_trailing(name, name_width), String.pad_trailing(type, type_width), details]
          |> Enum.join("  ")
          |> String.trim_trailing()
          |> then(&("        " <> &1 <> description(description, 10)))
        end)

      "      #{resource}\n#{lines}"
    end)
  end

  # Each line of the description, on lines of its own with the given indentation.
  defp description(nil, _indent), do: ""

  defp description(description, indent) do
    description
    |> String.trim()
    |> String.split("\n")
    |> Enum.map_join(&("\n" <> String.duplicate(" ", indent) <> String.trim_trailing(&1)))
  end

  defp emitter_row(emitter) do
    {inspect(emitter.resource),
     [
       inspect(emitter.action),
       to_string(emitter.action_type),
       Enum.join(emitter_details(emitter), ", ")
     ], emitter.emit.description}
  end

  defp listener_row(listener) do
    details =
      [
        listener.action != listener.name && "action: #{inspect(listener.action)}",
        listener.batch? && "batch",
        delivery(listener) && "delivery: #{inspect(delivery(listener))}"
      ]
      |> Enum.filter(& &1)
      |> Enum.join(", ")

    {inspect(listener.resource),
     [inspect(listener.name), to_string(listener.action_type), details], listener.description}
  end

  defp emitter_details(emitter) do
    Enum.filter(
      [emitter.emit.dynamic? && "dynamic", emitter.emit.where != [] && "conditional"],
      & &1
    )
  end

  @doc """
  Renders entries from `build/1` as a Mermaid flowchart.

  Each resource is a subgraph of the actions that emit or listen to signals, and each signal
  module is a subgraph of its signals. Listeners with a delivery other than
  `Ash.Signals.Delivery.Inline` are dotted lines, labelled with their delivery.
  """
  @spec mermaid([entry()]) :: String.t()
  def mermaid(entries) do
    signal_ids =
      entries
      |> Enum.with_index()
      |> Map.new(fn {entry, index} -> {{entry.signal_module, entry.signal.name}, "s#{index}"} end)

    actions =
      entries
      |> Enum.flat_map(fn entry ->
        Enum.map(entry.emitters, &{&1.resource, &1.action}) ++
          Enum.map(entry.listeners, &{&1.resource, &1.action})
      end)
      |> Enum.uniq()
      |> Enum.sort()

    action_ids =
      actions
      |> Enum.with_index()
      |> Map.new(fn {action, index} -> {action, "a#{index}"} end)

    resources =
      actions
      |> Enum.chunk_by(&elem(&1, 0))
      |> Enum.with_index()
      |> Enum.map(fn {[{resource, _} | _] = actions, index} ->
        nodes =
          Enum.map_join(actions, "\n", fn {_, action} = key ->
            "    #{action_ids[key]}[#{label(action)}]"
          end)

        "  subgraph r#{index}[#{label(inspect(resource))}]\n#{nodes}\n  end"
      end)

    signal_modules =
      entries
      |> Enum.chunk_by(& &1.signal_module)
      |> Enum.with_index()
      |> Enum.map(fn {[%{signal_module: signal_module} | _] = entries, index} ->
        nodes =
          Enum.map_join(entries, "\n", fn entry ->
            name =
              if entry.signal.phase == :after_action do
                to_string(entry.signal.name)
              else
                "#{entry.signal.name}<br/>(#{entry.signal.phase})"
              end

            "    #{signal_ids[{entry.signal_module, entry.signal.name}]}{{#{label(name)}}}"
          end)

        "  subgraph m#{index}[#{label(inspect(signal_module))}]\n#{nodes}\n  end"
      end)

    edges =
      Enum.flat_map(entries, fn entry ->
        signal_id = signal_ids[{entry.signal_module, entry.signal.name}]

        emitted =
          Enum.map(entry.emitters, fn emitter ->
            from = action_ids[{emitter.resource, emitter.action}]

            case emitter_details(emitter) do
              [] -> "  #{from} --> #{signal_id}"
              details -> "  #{from} -->|#{label(Enum.join(details, ", "))}| #{signal_id}"
            end
          end)

        listened =
          Enum.map(entry.listeners, fn listener ->
            to = action_ids[{listener.resource, listener.action}]

            name = listener.action != listener.name && to_string(listener.name)
            delivery = delivery(listener) && "via #{inspect(delivery(listener))}"
            arrow = if delivery, do: "-.->", else: "-->"

            case Enum.filter([name, delivery], & &1) do
              [] -> "  #{signal_id} #{arrow} #{to}"
              parts -> "  #{signal_id} #{arrow}|#{label(Enum.join(parts, " "))}| #{to}"
            end
          end)

        emitted ++ listened
      end)
      |> Enum.uniq()

    Enum.join(["flowchart LR" | resources ++ signal_modules ++ edges], "\n") <> "\n"
  end

  defp delivery(%{delivery: {Ash.Signals.Delivery.Inline, _}}), do: nil
  defp delivery(%{delivery: {delivery, _}}), do: delivery

  defp label(text) do
    ~s("#{String.replace(to_string(text), "\"", "#quot;")}")
  end
end

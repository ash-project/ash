defmodule Ash.Flow.Chart.Mermaid do
  @moduledoc "Tools to render an Ash.Flow as a mermaid chart."
  @opts [
    expand: [
      type: :boolean,
      default: true,
      doc: """
      If the flow should be fully expanded (all `run_flow` steps will be inlined)
      """
    ]
  ]

  def chart(flow, opts \\ []) do
    opts = Ash.OptionsHelpers.validate!(opts, @opts)

    # This is a hack that may not work forever
    # Eventually, we may need a separate mode/option for `build`
    # that doesn't attempt to substitute arguments into templates
    args =
      flow
      |> Ash.Flow.Info.arguments()
      |> Map.new(fn argument ->
        {argument.name, {:_arg, argument.name}}
      end)

    steps =
      if opts[:expand] do
        {:ok, %{steps: steps}} = Ash.Flow.Executor.AshEngine.build(flow, args, [])
        unwrap(steps)
      else
        Ash.Flow.Info.steps(flow)
      end

    arguments = flow |> Ash.Flow.Info.arguments()

    init = "flowchart TB"

    init
    |> add_arguments(arguments, flow)
    |> add_steps(steps, steps, opts)
    |> add_links(steps, steps, opts)
    |> IO.iodata_to_binary()
  end

  defp add_arguments(message, arguments, flow) do
    message =
      message
      |> add_line("subgraph Flow")
      |> add_line("direction TB")

    add_line(
      message,
      "_top_level_flow_(\"#{inspect(flow)}#{flow_description(flow)}\")"
    )

    Enum.reduce(arguments, message, fn argument, message ->
      question_mark =
        if argument.allow_nil? do
          "?"
        else
          ""
        end

      add_line(
        message,
        "_arguments.#{argument.name}(\"#{argument.name}#{question_mark}: #{inspect(argument.type)}\")"
      )
    end)
    |> add_line("end")
  end

  defp unwrap(steps) do
    Enum.map(steps, fn %{step: step} ->
      case step do
        %{steps: steps} ->
          %{step | steps: unwrap(steps)}

        step ->
          step
      end
    end)
  end

  defp add_steps(message, steps, all_steps, opts) do
    Enum.reduce(steps, message, fn step, message ->
      case step do
        %Ash.Flow.Step.Map{steps: steps, over: over} = step ->
          id = "#{format_name(step)}.element"

          name = format_name(step)
          template = format_template(over, all_steps)

          message =
            message
            |> add_line("subgraph #{name} [\"Map Over #{template}\"]")
            |> add_line("direction TB")
            |> add_line("#{id}(\"Element in #{template}\")")
            |> add_steps(steps, all_steps, opts)
            |> add_line("end")

          message

        %Ash.Flow.Step.Transaction{steps: steps} = step ->
          name = format_name(step)

          message
          |> add_line("subgraph #{name}.subgraph [Transaction]")
          |> add_line("direction TB")
          |> add_steps(steps, all_steps, opts)
          |> add_line("end")

        %Ash.Flow.Step.RunFlow{flow: flow} = step ->
          returns = Ash.Flow.Info.returns(flow)

          if returns && opts[:expand] do
            escaped_returns = escape(inspect(Ash.Flow.Info.returns(flow)))
            name = format_name(step)

            header =
              if is_atom(returns) do
                "Gather Value"
              else
                "Gather Values"
              end

            message
            |> add_line("#{name}(\"#{header}: #{escaped_returns}\")")
          else
            message
            |> add_line("#{format_name(step)}(\"#{inspect(flow)}#{flow_description(flow)}\")")
          end

        %{input: input} = step when not is_nil(input) ->
          add_line(
            message,
            "#{format_name(step)}(\"#{short_name(step)} <br/> #{format_template(input, all_steps)}#{description(step)}\")"
          )

        step ->
          add_line(message, "#{format_name(step)}(\"#{short_name(step)}\"#{description(step)})")
      end
    end)
  end

  defp flow_description(flow) do
    case Ash.Flow.Info.description(flow) do
      nil ->
        ""

      description ->
        "<br/>" <> as_html!(description)
    end
  end

  defp description(%{description: description}) when not is_nil(description) do
    "<br/>" <> as_html!(description)
  end

  defp description(%Ash.Flow.Step.Custom{custom: {mod, opts}}) do
    if function_exported?(mod, :describe, 1) do
      case mod.describe(opts) do
        nil ->
          ""

        description ->
          "<br/>" <> as_html!(description)
      end
    else
      ""
    end
  end

  defp description(_), do: ""

  if Code.ensure_loaded?(Earmark) do
    defp as_html!(value) do
      value
      |> Earmark.as_html!()
      |> String.replace("<br>", "<br/>")
    end
  else
    defp as_html!(value), do: value |> String.split("\n", trim: true) |> Enum.join("<br/>")
  end

  defp short_name(%Ash.Flow.Step.Custom{custom: {mod, opts}}) do
    if function_exported?(mod, :short_name, 1) do
      mod.short_name(opts)
    else
      escape(inspect({mod, opts}))
    end
  end

  defp short_name(%Ash.Flow.Step.Map{steps: steps, output: output}) do
    child_step =
      if output do
        find_step(steps, output)
      else
        List.last(steps)
      end

    "Element of #{short_name(child_step)}"
  end

  defp short_name(%Ash.Flow.Step.RunFlow{flow: flow}) do
    "Run Flow: #{inspect(flow)}"
  end

  defp short_name(%Ash.Flow.Step.Create{action: action, resource: resource}) do
    "Create: #{inspect(resource)}.#{action}"
  end

  defp short_name(%Ash.Flow.Step.Update{action: action, resource: resource}) do
    "Update: #{inspect(resource)}.#{action}"
  end

  defp short_name(%Ash.Flow.Step.Destroy{action: action, resource: resource}) do
    "Destroy: #{inspect(resource)}.#{action}"
  end

  defp short_name(%Ash.Flow.Step.Read{action: action, resource: resource}) do
    "Read: #{inspect(resource)}.#{action}"
  end

  # defp highlight(message, id) do
  #   add_line(message, "style #{id} fill:#4287f5,stroke:#333,stroke-width:4px")
  # end

  defp add_links(message, steps, all_steps, opts) do
    Enum.reduce(steps, message, fn step, message ->
      case step do
        %Ash.Flow.Step.Map{steps: steps, over: over} = step ->
          id = "#{format_name(step)}.element"

          message
          |> add_dependencies(step, all_steps, [over], id)
          |> add_links(steps, all_steps, opts)

        %Ash.Flow.Step.Transaction{steps: steps} = step ->
          message
          |> add_dependencies(step, all_steps)
          |> add_links(steps, all_steps, opts)

        %Ash.Flow.Step.RunFlow{flow: flow} = run_flow_step ->
          returns = Ash.Flow.Info.returns(flow)
          name = format_name(step)

          message =
            Enum.reduce(List.wrap(returns), message, fn
              {key, _}, message ->
                {source, note} =
                  if opts[:expand] do
                    link_source(all_steps, List.wrap(step.name) ++ List.wrap(key))
                  else
                    link_source(all_steps, step.name)
                  end

                message
                |> add_link(source, note, name)

              value, message ->
                {source, note} =
                  if opts[:expand] do
                    link_source(all_steps, List.wrap(step.name) ++ List.wrap(value))
                  else
                    link_source(all_steps, step.name)
                  end

                message
                |> add_link(source, note, name)
            end)

          if opts[:expand] do
            message
          else
            add_dependencies(message, run_flow_step, all_steps)
          end

        step ->
          add_dependencies(message, step, all_steps)
      end
    end)
  end

  defp add_link(message, source, nil, name) do
    add_line(message, "#{source}-->#{name}")
  end

  defp add_link(message, source, note, name) do
    add_line(message, "#{source}-->|#{note}|#{name}")
  end

  defp format_template(template, all_steps) do
    do_format_template(template, all_steps)
  end

  defp do_format_template(template, all_steps) when is_map(template) do
    body =
      Enum.map_join(template, ",<br/>", fn {key, value} ->
        case do_format_template(key, all_steps) do
          ":" <> key ->
            "#{key}: #{do_format_template(value, all_steps)}"

          key ->
            "#{inspect(key)} => #{do_format_template(value, all_steps)}"
        end
      end)

    "%{<br/>#{body}<br/>}"
  end

  defp do_format_template(template, all_steps) when is_list(template) do
    "[#{Enum.map_join(template, ", ", &do_format_template(&1, all_steps))}]"
  end

  defp do_format_template({:_path, value, path}, all_steps) do
    "get_in(#{do_format_template(value, all_steps)}, #{Enum.map_join(path, ", ", &do_format_template(&1, all_steps))})"
  end

  defp do_format_template({:_result, step_name}, all_steps) do
    "result(#{short_name(find_step(all_steps, step_name))})"
  end

  defp do_format_template({:_element, step_name}, all_steps) do
    "element(#{short_name(find_step(all_steps, step_name))})"
  end

  defp do_format_template(value, all_steps) when is_tuple(value) do
    "{#{value |> Tuple.to_list() |> Enum.map_join(", ", &do_format_template(&1, all_steps))}}"
  end

  defp do_format_template(value, _), do: inspect(value)

  defp find_step(steps, name) do
    case do_find_step(steps, name) do
      nil ->
        raise "Could not find step called #{inspect(name)} in #{steps |> step_names() |> Enum.map_join(", ", &inspect/1)}"

      step ->
        step
    end
  end

  defp do_find_step(steps, name) when is_list(steps),
    do: Enum.find_value(steps, &do_find_step(&1, name))

  defp do_find_step(%{name: name} = step, name), do: step
  defp do_find_step(%{steps: steps}, name), do: do_find_step(steps, name)
  defp do_find_step(_, _), do: nil

  defp step_names(steps) do
    Enum.flat_map(steps, fn step ->
      case step do
        %{name: name, steps: steps} ->
          [name | step_names(steps)]

        %{name: name} ->
          [name]
      end
    end)
  end

  defp escape(string) do
    String.replace(string, "\"", "'")
  end

  defp add_dependencies(message, step, all_steps, additional_inputs \\ [], name \\ nil) do
    deps =
      Enum.flat_map(Ash.Flow.Executor.AshEngine.deps_keys(), fn key ->
        case Map.fetch(step, key) do
          {:ok, value} ->
            [value]

          :error ->
            []
        end
      end)

    add_deps(message, deps ++ additional_inputs, name || format_name(step), all_steps)
  end

  defp add_deps(message, template, destination, all_steps) do
    result_refs = Ash.Flow.result_refs(template) |> Enum.uniq()
    arg_refs = Ash.Flow.arg_refs(template) |> Enum.uniq()
    element_refs = Ash.Flow.element_refs(template) |> Enum.uniq()

    message =
      Enum.reduce(element_refs, message, fn element, message ->
        add_line(message, "#{do_format_name(element)}.element --> #{destination}")
      end)

    message =
      Enum.reduce(arg_refs, message, fn arg, message ->
        add_line(message, "_arguments.#{arg} -.-> #{destination}")
      end)

    Enum.reduce(result_refs, message, fn dep, message ->
      {source, note} = link_source(all_steps, dep)

      add_link(message, source, note, destination)
    end)
  end

  defp link_source(all_steps, dep, note \\ nil) do
    case find_step(all_steps, dep) do
      %Ash.Flow.Step.Map{steps: steps, output: output} = step ->
        output_step =
          if output do
            find_step(steps, output)
          else
            List.last(steps)
          end

        case output_step do
          nil ->
            {format_name(step), note}

          output_step ->
            link_source(all_steps, output_step.name, "list")
        end

      step ->
        {format_name(step), note}
    end
  end

  defp add_line(message, line) do
    [message, "\n", line]
  end

  defp format_name(step) do
    do_format_name(step.name)
  end

  defp do_format_name(name) do
    name
    |> List.wrap()
    |> List.flatten()
    |> Enum.join(".")
  end
end

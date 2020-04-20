defmodule Ash.Authorization.Report do
  alias Ash.Authorization.Clause

  defstruct [
    :scenarios,
    :requests,
    :facts,
    :state,
    :header,
    :authorized?,
    :reason,
    path: [],
    no_steps_configured: false
  ]

  def report(%{no_steps_configured: %Ash.Engine.Request{} = request}) do
    "forbidden:\n" <>
      request.source <> ": no authorization steps configured. Resource: #{request.resource}"
  end

  # We know that each group of authorization steps shares the same relationship
  def report(report) do
    header = (report.header || "Authorization Report") <> "\n"

    header =
      if report.path do
        Enum.join(report.path, ".") <> " " <> header
      else
        header
      end

    facts = Ash.Authorization.Clause.prune_facts(report.facts)

    explained_steps = explain_steps(report.requests, facts)

    explained_facts = explain_facts(facts)

    reason =
      if report.reason do
        "\n" <> report.reason <> "\n"
      else
        ""
      end

    main_message =
      header <> reason <> indent("Facts Gathered\n" <> indent(explained_facts) <> explained_steps)

    if report.authorized? do
      main_message
    else
      main_message <> indent("\n\nScenarios:\n" <> indent(explain_scenarios(report.scenarios)))
    end
  end

  defp explain_scenarios(scenarios) when scenarios in [nil, []] do
    """
    No scenarios found. Under construction.
    Eventually, scenarios will explain what data you could change to make the request possible.
    """
  end

  defp explain_scenarios(scenarios) do
    """
    #{Enum.count(scenarios)} found. Under construction.
    Eventually, scenarios will explain what data you could change to make the request possible.
    """
  end

  # defp explain_steps_with_data(requests, facts, data) do
  #   title = "\n\nAuthorization Steps:\n\n"

  #   contents =
  #     requests
  #     |> Enum.map_join("\n---\n", fn request ->
  #       relationship = request.relationship
  #       resource = request.resource

  #       inner_title =
  #         if relationship == [] do
  #           request.source <> " -> " <> inspect(resource) <> ": "
  #         else
  #           Enum.join(relationship, ".") <> " - " <> inspect(resource) <> ": "
  #         end

  #       full_inner_title =
  #         if request.strict_access? do
  #           inner_title <> " (strict access)"
  #         else
  #           inner_title
  #         end

  #       rules_legend =
  #         request.rules
  #         |> Enum.with_index()
  #         |> Enum.map_join("\n", fn {{step, check}, index} ->
  #           "#{index + 1}| " <>
  #             to_string(step) <> ": " <> check.check_module.describe(check.check_opts)
  #         end)

  #       pkey = Ash.primary_key(resource)

  #       # TODO: data has to change with relationships
  #       data_info =
  #         data
  #         |> Enum.map(fn item ->
  #           formatted =
  #             item
  #             |> Map.take(pkey)
  #             |> format_pkey()

  #           {formatted, Map.take(item, pkey)}
  #         end)
  #         |> add_header_line(indent("Record"))
  #         |> pad()
  #         |> add_step_info(request.rules, facts)

  #       full_inner_title <>
  #         ":\n" <> indent(rules_legend <> "\n\n" <> data_info <> "\n")
  #     end)

  #   title <> indent(contents)
  # end

  # defp add_step_info([header | rest], steps, facts) do
  #   key = Enum.join(1..Enum.count(steps), "|")

  #   header <>
  #     indent(
  #       " |" <>
  #         key <>
  #         "|\n" <>
  #         do_add_step_info(rest, steps, facts)
  #     )
  # end

  # defp do_add_step_info(pkeys, steps, facts) do
  #   Enum.map_join(pkeys, "\n", fn {pkey_line, pkey} ->
  #     steps
  #     |> Enum.reduce({true, pkey_line <> " "}, fn
  #       {_step, _clause}, {false, string} ->
  #         {false, string <> "|~"}

  #       {step, clause}, {true, string} ->
  #         status =
  #           case Clause.find(facts, %{clause | pkey: pkey}) do
  #             {:ok, value} -> value
  #             _ -> nil
  #           end

  #         mark = step_to_mark(step, status)

  #         new_mark =
  #           if mark == "↓" do
  #             "→"
  #           else
  #             mark
  #           end

  #         continue? = new_mark not in ["✓", "✗"]

  #         {continue?, string <> "|" <> new_mark}
  #     end)
  #     |> elem(1)
  #     |> Kernel.<>("|")
  #   end)
  # end

  # defp add_header_line(lines, title) do
  #   [title | lines]
  # end

  # defp pad(lines) do
  #   longest =
  #     lines
  #     |> Enum.map(fn
  #       {line, _pkey} ->
  #         String.length(line)

  #       line ->
  #         String.length(line)
  #     end)
  #     |> Enum.max()

  #   Enum.map(
  #     lines,
  #     fn
  #       {line, pkey} ->
  #         length = String.length(line)

  #         {line <> String.duplicate(" ", longest - length), pkey}

  #       line ->
  #         length = String.length(line)

  #         line <> String.duplicate(" ", longest - length)
  #     end
  #   )
  # end

  defp count_of_clauses(nil), do: 0

  defp count_of_clauses(filter) do
    relationship_clauses =
      filter.relationships
      |> Map.values()
      |> Enum.map(fn related_filter ->
        1 + count_of_clauses(related_filter)
      end)
      |> Enum.sum()

    or_clauses =
      filter.ors
      |> Kernel.||([])
      |> Enum.map(&count_of_clauses/1)
      |> Enum.sum()

    not_clauses = count_of_clauses(filter.not)

    Enum.count(filter.attributes) + relationship_clauses + or_clauses + not_clauses
  end

  defp explain_facts(facts) when facts == %{}, do: "No facts gathered."

  defp explain_facts(facts) do
    facts
    |> Map.drop([true, false])
    |> Enum.map(fn {%{filter: filter} = key, value} ->
      {key, value, count_of_clauses(filter)}
    end)
    # TODO: nest child filters under parent filters?
    |> Enum.group_by(fn {clause, _value, _count_of_clauses} ->
      if clause.filter do
        clause.filter.resource
      else
        nil
      end

      # clause.filter.resource
    end)
    |> Enum.map_join("\n", fn {resource, clauses} ->
      clauses =
        Enum.sort_by(clauses, fn {_, _, count_of_clauses} ->
          count_of_clauses
        end)

      explained =
        Enum.map_join(clauses, "\n", fn {clause, value, count_of_clauses} ->
          if count_of_clauses == 0 do
            clause.check_module.describe(clause.check_opts) <> " " <> status_to_mark(value)
          else
            inspect(clause.filter) <>
              ": " <>
              clause.check_module.describe(clause.check_opts) <> " " <> status_to_mark(value)
          end
        end)

      if resource do
        "#{inspect(resource)}: \n" <> indent(explained) <> "\n"
      else
        explained <> "\n"
      end
    end)

    # |> Enum.group_by(fn {clause, _status} ->
    #   clause.filter
    # end)
    # |> Enum.sort_by(fn {filter, _} -> not is_nil(filter) end)
    # |> Enum.map_join("\n---\n", fn {pkey, clauses_and_statuses} ->
    #   title = format_pkey(pkey) <> " facts"

    #   contents =
    #     clauses_and_statuses
    #     |> Enum.group_by(fn {clause, _} ->
    #       {clause.source, clause.path}
    #     end)
    #     |> Enum.sort_by(fn {{_, relationship}, _} ->
    #       {Enum.count(relationship), relationship}
    #     end)
    #     |> Enum.map_join("\n", fn {{source, relationship}, clauses_and_statuses} ->
    #       contents =
    #         Enum.map_join(clauses_and_statuses, "\n", fn {clause, status} ->
    #           mod = clause.check_module
    #           opts = clause.check_opts

    #           status_to_mark(status) <> " " <> mod.describe(opts)
    #         end)

    #       if relationship == [] do
    #         indent(contents)
    #       else
    #         operation =
    #           if source == :side_load do
    #             "SideLoad "
    #           else
    #             "Related "
    #           end

    #         operation <> Enum.join(relationship, ".") <> ":\n" <> indent(contents)
    #       end
    #     end)

    #   title <> ":\n" <> contents
    # end)
  end

  defp status_to_mark(true), do: "✓"
  defp status_to_mark(false), do: "✗"
  defp status_to_mark(:unknowable), do: "?"
  defp status_to_mark(:irrelevant), do: "⊘"
  defp status_to_mark(nil), do: "-"

  defp indent(string) do
    string
    |> String.split("\n")
    |> Enum.map(fn line -> "  " <> line end)
    |> Enum.join("\n")
  end

  defp explain_steps(requests, facts) do
    title = "\n\nAuthorization Steps:\n"

    contents =
      requests
      |> Enum.sort_by(fn request -> Enum.count(request.path) end)
      |> Enum.map_join("\n------\n", fn request ->
        title =
          if request.strict_access? do
            request.name <> " (strict access)"
          else
            request.name
          end

        contents =
          request.rules
          |> Enum.map(fn {step, clause} ->
            status =
              case Clause.find(facts, clause) do
                {:ok, value} -> value
                _ -> nil
              end

            status_mark = status_to_mark(status)
            step_mark = step_to_mark(step, status)

            mod = clause.check_module
            opts = clause.check_opts
            relationship = clause.path

            if relationship == [] do
              step_mark <>
                " | " <> to_string(step) <> ": " <> mod.describe(opts) <> " " <> status_mark
            else
              step_mark <>
                " | " <>
                to_string(step) <>
                ": #{Enum.join(relationship || [], ".")}: " <>
                mod.describe(opts) <> " " <> status_mark
            end
          end)
          |> Enum.join("\n")

        title <> ":\n" <> indent(contents)
      end)

    title <> indent(contents)
  end

  defp step_to_mark(:authorize_if, true), do: "✓"
  defp step_to_mark(:authorize_if, false), do: "↓"
  defp step_to_mark(:authorize_if, _), do: "↓"

  defp step_to_mark(:forbid_if, true), do: "✗"
  defp step_to_mark(:forbid_if, false), do: "↓"
  defp step_to_mark(:forbid_if, _), do: "✗"

  defp step_to_mark(:authorize_unless, true), do: "↓"
  defp step_to_mark(:authorize_unless, false), do: "✓"
  defp step_to_mark(:authorize_unless, _), do: "↓"

  defp step_to_mark(:forbid_unless, true), do: "↓"
  defp step_to_mark(:forbid_unless, false), do: "✗"
  defp step_to_mark(:forbid_unless, _), do: "✗"
end

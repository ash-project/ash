defmodule Ash.Policy.Chart.Mermaid do
  @moduledoc "Generates policy mermaid charts"

  def chart(resource) do
    policies = Ash.Policy.Info.policies(nil, resource)
    policy_count = Enum.count(policies)

    at_least_one_policy =
      Enum.flat_map(policies, fn policy ->
        case Enum.reject(List.wrap(policy.condition), fn
               %{check_module: Ash.Policy.Check.Static, check_opts: opts} ->
                 opts[:result] == true

               {Ash.Policy.Check.Static, opts} ->
                 opts[:result] == true

               _ ->
                 false
             end) do
          [] ->
            []

          conditions ->
            conditions
            |> Enum.map(fn condition ->
              {mod, opts} =
                case condition do
                  %{module: module, opts: opts} ->
                    {module, opts}

                  {module, opts} ->
                    {module, opts}
                end

              mod.describe(opts)
            end)
            |> Enum.intersperse(" and ")
            |> Enum.join()
            |> List.wrap()
        end
      end)
      |> case do
        [one] ->
          one

        [] ->
          nil

        multiple ->
          Enum.map_join(multiple, "\nor ", fn one ->
            if String.contains?(one, " and ") do
              "(#{one})"
            else
              one
            end
          end)
      end

    at_least_one_policy =
      if at_least_one_policy do
        """
        subgraph at least one policy applies
        direction TB
        at_least_one_policy[#{quote_and_escape(at_least_one_policy)}]
        end
        at_least_one_policy--False-->Forbidden
        at_least_one_policy--True-->0_conditions
        """
      end

    policy_text =
      policies
      |> Enum.with_index()
      |> Enum.map_join("\n", fn {%{condition: conditions, policies: checks} = policy,
                                 policy_index} ->
        conditions = List.wrap(conditions)

        # remove `always()` here
        condition_text =
          conditions
          |> Enum.map_join(" and ", fn condition ->
            {mod, opts} =
              case condition do
                %{module: module, opts: opts} ->
                  {module, opts}

                {module, opts} ->
                  {module, opts}
              end

            mod.describe(opts)
          end)

        last_policy? = policy_index == policy_count - 1
        next_policy = "#{policy_index + 1}_conditions"

        false_destination =
          if last_policy? do
            "Authorized"
          else
            next_policy
          end

        conditions =
          """
          #{policy_index}_conditions{#{quote_and_escape(condition_text)}}
          #{policy_index}_conditions--True-->#{policy_index}_checks_0
          """

        conditions =
          conditions <> "\n" <> "#{policy_index}_conditions--False-->#{false_destination}"

        checks = List.wrap(checks)
        check_count = Enum.count(checks)

        checks_text =
          checks
          |> Enum.with_index()
          |> Enum.map_join("\n", fn {check, check_index} ->
            next_policy_or_authorized =
              if last_policy? do
                "Authorized"
              else
                next_policy
              end

            next_check = "#{policy_index}_checks_#{check_index + 1}"

            next_check_or_forbidden =
              if check_index == check_count - 1 do
                "Forbidden"
              else
                next_check
              end

            next_check_or_next_policy_or_end =
              cond do
                check_index != check_count - 1 ->
                  next_check

                last_policy? ->
                  nil

                true ->
                  next_policy
              end

            {true_dest, false_dest} =
              case check.type do
                :authorize_if ->
                  if policy.bypass? do
                    {"Authorized", next_policy_or_authorized}
                  else
                    {next_policy_or_authorized, next_check_or_forbidden}
                  end

                :forbid_if ->
                  forbidden =
                    if policy.bypass? do
                      next_policy_or_authorized
                    else
                      "Forbidden"
                    end

                  {forbidden, next_check_or_next_policy_or_end}

                :authorize_unless ->
                  if policy.bypass? do
                    {"Authorized", next_policy_or_authorized}
                  else
                    {next_policy_or_authorized, next_check_or_forbidden}
                  end

                :forbid_unless ->
                  forbidden =
                    if policy.bypass? do
                      next_policy_or_authorized
                    else
                      "Forbidden"
                    end

                  {next_check_or_next_policy_or_end, forbidden}
              end

            true_path = "#{policy_index}_checks_#{check_index}--True-->#{true_dest}"

            false_path =
              if false_dest do
                "#{policy_index}_checks_#{check_index}--False-->#{false_dest}"
              end

            description = check.check_module.describe(check.check_opts)

            description =
              if description && description != "" do
                "{#{quote_and_escape(description)}}"
              else
                "{}"
              end

            """
            #{policy_index}_checks_#{check_index}#{description}
            #{true_path}
            #{false_path}
            """
            |> String.trim()
          end)

        description =
          if policy.description && policy.description != "" do
            "Policy #{policy_index + 1}[#{policy.description}]"
          else
            "Policy #{policy_index + 1}"
          end

        {nodes, rules} =
          """
          #{conditions}
          #{checks_text}
          """
          |> String.split("\n")
          |> Enum.split_with(fn thing ->
            is_node?(thing)
          end)

        """
        subgraph #{description}
        direction TB
        #{Enum.join(nodes, "\n")}
        end
        #{Enum.join(rules, "\n")}
        """
      end)

    """
    flowchart TB
    #{at_least_one_policy}
    #{policy_text}
    subgraph results[Results]
    Authorized([Authorized])
    Forbidden([Forbidden])
    end
    """
    |> remove_always_links()
    |> collapse_constant_nodes()
    |> remove_empty_subgraphs()
  end

  defp remove_empty_subgraphs(lines) do
    lines
    |> String.split("\n")
    |> Enum.reject(fn line ->
      String.trim(line) == ""
    end)
    |> do_remove_empty_subgraphs([])
    |> Enum.join("\n")
  end

  defp do_remove_empty_subgraphs([], acc), do: Enum.reverse(acc)

  defp do_remove_empty_subgraphs(["subgraph " <> _, "direction TB", "end" | rest], acc) do
    do_remove_empty_subgraphs(rest, acc)
  end

  defp do_remove_empty_subgraphs([item | rest], acc) do
    do_remove_empty_subgraphs(rest, [item | acc])
  end

  defp collapse_constant_nodes(text) do
    lines = String.split(text, "\n")

    Enum.find_value(lines, fn line ->
      if is_node?(line) do
        case String.split(line, "{\"") do
          [node_id | _] ->
            true_to = find_branch_to(lines, node_id, "True")
            false_to = find_branch_to(lines, node_id, "False")

            if true_to && false_to && true_to == false_to do
              phrase =
                if false_to in ["Authorized", "Forbidden"] do
                  ""
                else
                  "Or"
                end

              {node_id, true_to, phrase}
            end

          _ ->
            nil
        end
      end
    end)
    |> case do
      nil ->
        text

      {from, to, phrase} ->
        lines
        |> delete_branches(from, to)
        |> Enum.concat(["#{from}--#{phrase}-->#{to}"])
        |> Enum.join("\n")
        |> collapse_constant_nodes()
    end
  end

  defp delete_branches(lines, from, to) do
    Enum.reject(lines, fn line ->
      line in ["#{from}--True-->#{to}", "#{from}--False-->#{to}"]
    end)
  end

  defp is_node?(line) do
    not (String.contains?(line, "--True-->") or String.contains?(line, "--False-->")) &&
      String.contains?(line, "{\"")
  end

  defp quote_and_escape(text) do
    "\"#{escape(text)}\""
  end

  defp escape(string) do
    String.replace(string, "\"", "'")
  end

  defp remove_always_links(text) do
    lines = String.split(text, "\n")

    lines
    |> Enum.find(fn string ->
      String.contains?(string, "always true") || String.contains?(string, "always false")
    end)
    |> case do
      nil ->
        text

      text ->
        target_branch =
          if String.contains?(text, "always true") do
            "True"
          else
            "False"
          end

        node_id = text |> String.split("{") |> Enum.at(0)

        lines
        |> Enum.reduce([], fn line, acc ->
          case from_and_to(line) do
            {from, target, to} ->
              if to == node_id do
                true_to = find_branch_to(lines, node_id, target_branch)
                [build_line(target, from, true_to) | acc]
              else
                [line | acc]
              end

            nil ->
              [line | acc]
          end
        end)
        |> Enum.reject(fn line ->
          line == text || String.starts_with?(line, "#{node_id}--") ||
            String.ends_with?(line, "-->#{node_id}")
        end)
        |> Enum.reverse()
        |> Enum.join("\n")
        |> remove_always_links()
    end
  end

  defp find_branch_to(lines, id, target) do
    Enum.find_value(lines, fn line ->
      if String.starts_with?(line, "#{id}--#{target}-->") do
        String.trim_leading(line, "#{id}--#{target}-->")
      end
    end)
  end

  defp build_line(target, from, to), do: "#{from}--#{target}-->#{to}"

  defp from_and_to(line) do
    cond do
      String.contains?(line, "--True-->") ->
        [from, to] = String.split(line, "--True-->")
        {from, "True", to}

      String.contains?(line, "--False-->") ->
        [from, to] = String.split(line, "--False-->")
        {from, "False", to}

      true ->
        nil
    end
  end
end

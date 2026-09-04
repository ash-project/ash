defmodule Mix.Tasks.Compile.AshTypeInference do
  @moduledoc false
  use Mix.Task.Compiler

  @recursive true
  @manifest_version 3

  @impl true
  def run(args) do
    manifest_path = manifest_path()
    previous = read_manifest(manifest_path)
    entries = project_resources() |> Ash.Resource.TypeInference.witness_entries()
    changed = Enum.reject(entries, &(get_in(previous, [&1.key, :fingerprint]) == &1.fingerprint))

    changed_diagnostics =
      changed
      |> Task.async_stream(
        fn entry ->
          {_compiled, diagnostics} =
            Code.with_diagnostics(fn -> Code.compile_quoted(entry.definition) end)

          diagnostics =
            diagnostics
            |> Enum.map(&to_mix_diagnostic(&1, entry))
            |> Enum.uniq_by(&diagnostic_identity/1)

          {entry.key, diagnostics}
        end,
        max_concurrency: System.schedulers_online(),
        ordered: false,
        timeout: :infinity
      )
      |> Enum.flat_map(fn
        {:ok, result} -> [result]
        {:exit, reason} -> exit(reason)
      end)
      |> Map.new()

    current =
      Map.new(entries, fn entry ->
        diagnostics =
          Map.get_lazy(changed_diagnostics, entry.key, fn ->
            get_in(previous, [entry.key, :diagnostics]) || []
          end)

        {entry.key, %{fingerprint: entry.fingerprint, diagnostics: diagnostics}}
      end)

    diagnostics = current |> Map.values() |> Enum.flat_map(& &1.diagnostics)
    warnings_as_errors? = "--warnings-as-errors" in args

    diagnostics_to_print =
      if warnings_as_errors?,
        do: diagnostics,
        else: Map.values(changed_diagnostics) |> List.flatten()

    Enum.each(diagnostics_to_print, &print_diagnostic/1)
    write_manifest(manifest_path, current)

    errors? = Enum.any?(diagnostics, &(&1.severity == :error))
    warnings? = Enum.any?(diagnostics, &(&1.severity == :warning))

    cond do
      errors? or (warnings_as_errors? and warnings?) -> {:error, diagnostics}
      changed == [] -> {:noop, diagnostics}
      true -> {:ok, diagnostics}
    end
  end

  @impl true
  def diagnostics do
    manifest_path()
    |> read_manifest()
    |> Map.values()
    |> Enum.flat_map(& &1.diagnostics)
  end

  @impl true
  def manifests, do: [manifest_path()]

  @impl true
  def clean do
    File.rm(manifest_path())
    :ok
  end

  defp manifest_path,
    do: Path.join(Mix.Project.manifest_path(), "compile.ash_type_inference")

  defp diagnostic_identity(diagnostic) do
    {diagnostic.file, diagnostic_line(diagnostic.position), diagnostic.severity,
     diagnostic.message}
  end

  defp diagnostic_line({line, _column}), do: line
  defp diagnostic_line(line) when is_integer(line), do: line
  defp diagnostic_line(_position), do: nil

  defp print_diagnostic(diagnostic) do
    diagnostic
    |> Map.from_struct()
    |> Map.delete(:compiler_name)
    |> Code.print_diagnostic()
  end

  defp project_resources do
    Mix.Project.compile_path()
    |> Path.join("*.beam")
    |> Path.wildcard()
    |> Enum.flat_map(fn path ->
      case :beam_lib.info(String.to_charlist(path)) do
        info when is_list(info) -> [Keyword.fetch!(info, :module)]
        _ -> []
      end
    end)
    |> Enum.filter(fn module ->
      Code.ensure_loaded?(module) && Ash.Resource.Info.resource?(module)
    end)
  end

  defp read_manifest(path) do
    with {:ok, binary} <- File.read(path),
         {@manifest_version, values} when is_map(values) <- :erlang.binary_to_term(binary) do
      values
    else
      _ -> %{}
    end
  end

  defp write_manifest(path, values) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, :erlang.term_to_binary({@manifest_version, values}, [:compressed]))
  end

  @doc false
  def to_mix_diagnostic(diagnostic, entry) do
    diagnostic = improve_diagnostic(diagnostic, entry)

    struct!(
      Mix.Task.Compiler.Diagnostic,
      Map.take(diagnostic, [
        :file,
        :source,
        :severity,
        :message,
        :position,
        :span,
        :details,
        :stacktrace
      ])
      |> Map.put(:compiler_name, "Ash type inference")
    )
  end

  defp improve_diagnostic(diagnostic, %{key: {resource, :change, change_module}}) do
    case unknown_changeset_field(diagnostic.message) do
      nil ->
        add_action_context(diagnostic, resource, change_module)

      field ->
        actions = change_actions(resource, change_module)

        %{
          diagnostic
          | file: module_source(change_module) || diagnostic.file,
            message: """
            Unknown Ash.Changeset field :#{field}

            In:
              #{inspect(change_module)}.change/3

            Ash.Changeset has no field named :#{field}.
            #{format_action_inputs(actions)}
            """
        }
    end
  end

  defp improve_diagnostic(diagnostic, _entry), do: diagnostic

  defp unknown_changeset_field(message) do
    case Regex.run(
           ~r/(?:accessing|unknown key )\.([a-zA-Z_][a-zA-Z0-9_]*).*?expression:\s+changeset\.\1\b/s,
           message
         ) do
      [_, field] -> field
      _ -> nil
    end
  end

  defp add_action_context(diagnostic, resource, change_module) do
    context = format_action_inputs(change_actions(resource, change_module))
    %{diagnostic | message: diagnostic.message <> "\nAsh action context:\n" <> context <> "\n"}
  end

  defp change_actions(resource, change_module) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.filter(fn action ->
      resource
      |> Ash.Resource.Info.action_changes(action)
      |> Enum.any?(fn
        %{change: {^change_module, _opts}} -> true
        _ -> false
      end)
    end)
  end

  defp format_action_inputs([action]) do
    "Action arguments: #{inspect(Enum.map(action.arguments, & &1.name))}\n" <>
      "Accepted attributes: #{inspect(accepted_attributes(action))}"
  end

  defp format_action_inputs(actions) do
    Enum.map_join(actions, "\n", fn action ->
      "#{inspect(action.name)} — arguments: #{inspect(Enum.map(action.arguments, & &1.name))}; " <>
        "accepted attributes: #{inspect(accepted_attributes(action))}"
    end)
  end

  defp accepted_attributes(action), do: action |> Map.get(:accept, []) |> List.wrap()

  defp module_source(module) do
    module.module_info(:compile)
    |> Keyword.get(:source)
    |> case do
      nil -> nil
      source -> List.to_string(source)
    end
  rescue
    _ -> nil
  end
end

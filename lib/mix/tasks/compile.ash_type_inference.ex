defmodule Mix.Tasks.Compile.AshTypeInference do
  @moduledoc false
  use Mix.Task.Compiler

  @recursive true
  @manifest_version 4

  @impl true
  def run(args) do
    manifest_path = manifest_path()
    previous = read_manifest(manifest_path)
    entries = project_resources() |> Ash.Resource.TypeInference.witness_entries()

    changed =
      if "--force" in args or "--force-ash-type-inference" in args do
        entries
      else
        Enum.reject(entries, &(get_in(previous, [&1.key, :fingerprint]) == &1.fingerprint))
      end

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

    diagnostics =
      current
      |> Map.values()
      |> Enum.flat_map(& &1.diagnostics)
      |> Enum.uniq_by(&diagnostic_identity/1)

    warnings_as_errors? = "--warnings-as-errors" in args
    return_errors? = "--return-errors" in args
    errors? = Enum.any?(diagnostics, &(&1.severity == :error))
    warnings? = Enum.any?(diagnostics, &(&1.severity == :warning))
    failed? = errors? or (warnings_as_errors? and warnings?)

    diagnostics_to_print =
      cond do
        failed? and return_errors? -> []
        failed? -> diagnostics
        true -> Map.values(changed_diagnostics) |> List.flatten()
      end

    Enum.each(diagnostics_to_print, &print_diagnostic/1)
    write_manifest(manifest_path, current)

    cond do
      failed? and return_errors? -> {:error, diagnostics}
      failed? -> {:error, []}
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
    diagnostic = diagnostic |> remap_diagnostic(entry) |> improve_diagnostic(entry)

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

  defp remap_diagnostic(diagnostic, entry) do
    module = diagnostic_module(entry)
    file = module && module_source(module)

    if file do
      {original_line, column} = diagnostic_position(diagnostic.position)
      {name, arity, definition_line} = diagnostic_function(module, file, original_line, entry)
      line = if original_line > 0, do: original_line, else: definition_line

      diagnostic
      |> Map.put(:file, file)
      |> Map.put(:source, file)
      |> Map.put(:position, {line, column})
      |> Map.put(:stacktrace, [
        {module, name, arity,
         [file: String.to_charlist(file), line: line, column: column, no_parens: true]}
      ])
      |> Map.update!(:message, &String.replace(&1, ~r/# from: nofile(?=:|\n)/, "# from: #{file}"))
      |> remap_generated_function(entry, name, arity)
    else
      diagnostic
    end
  end

  defp diagnostic_module(%{key: {_resource, kind, module}})
       when kind in [:change, :validation, :preparation],
       do: module

  defp diagnostic_module(%{key: {resource, :inline_change}}), do: resource
  defp diagnostic_module(_entry), do: nil

  defp diagnostic_position({line, column}), do: {line, column}
  defp diagnostic_position(line) when is_integer(line), do: {line, 1}
  defp diagnostic_position(_position), do: {1, 1}

  defp diagnostic_function(_module, file, line, entry \\ %{}) do
    locations = source_locations(file)

    locations
    |> diagnostic_function_location(entry, line)
    |> then(fn {definition_line, name, arity} -> {name, arity, definition_line} end)
  end

  defp remap_generated_function(diagnostic, %{generated_functions: generated}, name, arity) do
    Enum.reduce(generated, diagnostic, fn {generated_name, generated_arity}, diagnostic ->
      Map.update!(diagnostic, :message, fn message ->
        message
        |> String.replace("#{generated_name}/#{generated_arity}", "#{name}/#{arity}")
        |> String.replace(Atom.to_string(generated_name), Atom.to_string(name))
      end)
    end)
  end

  defp remap_generated_function(diagnostic, _entry, _name, _arity), do: diagnostic

  defp diagnostic_function_location([], %{diagnostic_functions: [{name, arity} | _]}, line),
    do: {line, name, arity}

  defp diagnostic_function_location(locations, %{diagnostic_functions: functions}, line) do
    locations
    |> Enum.filter(fn {_definition_line, name, arity} -> {name, arity} in functions end)
    |> nearest_definition(line)
    |> then(&(&1 || {line, :unknown, 0}))
  end

  defp diagnostic_function_location(locations, _entry, line) do
    nearest_definition(locations, line) || {line, :unknown, 0}
  end

  defp nearest_definition(locations, line) do
    locations
    |> Enum.filter(fn {definition_line, _name, _arity} -> definition_line <= line end)
    |> Enum.max_by(&elem(&1, 0), fn -> List.first(locations) end)
  end

  defp source_locations(file) do
    with {:ok, source} <- File.read(file),
         {:ok, ast} <- Code.string_to_quoted(source, columns: true) do
      ast
      |> Macro.prewalk([], fn
        {visibility, metadata, [head, _body]} = node, locations
        when visibility in [:def, :defp] ->
          case source_function_head(head) do
            {name, arity} -> {node, [{metadata[:line] || 1, name, arity} | locations]}
            nil -> {node, locations}
          end

        node, locations ->
          {node, locations}
      end)
      |> elem(1)
    else
      _ -> []
    end
  end

  defp source_function_head({:when, _metadata, [head | _guards]}), do: source_function_head(head)

  defp source_function_head({name, _metadata, arguments})
       when is_atom(name) and is_list(arguments),
       do: {name, length(arguments)}

  defp source_function_head(_head), do: nil

  defp improve_diagnostic(diagnostic, %{key: {resource, :change, change_module}}) do
    case unknown_changeset_field(diagnostic.message) do
      nil ->
        add_action_context(diagnostic, resource, change_module)

      field ->
        actions = change_actions(resource, change_module)

        {name, arity, _line} =
          diagnostic_function(
            change_module,
            module_source(change_module),
            diagnostic_line(diagnostic.position) || 1
          )

        %{
          diagnostic
          | message: """
            Unknown Ash.Changeset field :#{field}

            In:
              #{inspect(change_module)}.#{name}/#{arity}

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

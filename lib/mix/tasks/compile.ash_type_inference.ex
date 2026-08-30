defmodule Mix.Tasks.Compile.AshTypeInference do
  @moduledoc false
  use Mix.Task.Compiler

  @recursive true
  @manifest_version 1

  @impl true
  def run(_args) do
    manifest_path = Path.join(Mix.Project.manifest_path(), "compile.ash_type_inference")
    previous = read_manifest(manifest_path)
    entries = project_resources() |> Ash.Resource.TypeInference.witness_entries()
    changed = Enum.reject(entries, &(previous[&1.key] == &1.fingerprint))

    changed
    |> Task.async_stream(&Code.compile_quoted(&1.definition),
      max_concurrency: System.schedulers_online(),
      ordered: false,
      timeout: :infinity
    )
    |> Enum.each(fn
      {:ok, _compiled} -> :ok
      {:exit, reason} -> exit(reason)
    end)

    write_manifest(manifest_path, Map.new(entries, &{&1.key, &1.fingerprint}))
    if changed == [], do: {:noop, []}, else: {:ok, []}
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
end

defmodule Ash.JsonApi.Includes.Includer do
  alias Ash.JsonApi.Request

  @spec get_includes(record_or_records :: struct | list(struct) | nil, Request.t()) ::
          {:ok, struct | list(struct), list(struct)}
  def get_incldues(nil, _) do
    {:ok, nil, []}
  end

  def get_includes(_record_or_records, %Request{includes: includes}) when includes == %{}, do: %{}

  def get_includes(records, %Request{includes: includes, resource: resource})
      when is_list(records) do
    include_keyword = includes_to_keyword(includes)

    with {:ok, preloaded} <- Ash.side_load(records, include_keyword, resource),
         {preloaded_with_linkage, includes_list} <- get_includes_list(preloaded, include_keyword) do
      {:ok, preloaded_with_linkage, includes_list}
    end
  end

  def get_includes(record, request) do
    case get_includes([record], request) do
      {:ok, [record], includes} ->
        {:ok, record, includes}

      other ->
        other
    end
  end

  defp get_includes_list(related, []), do: {related, []}

  defp get_includes_list(preloaded, include_keyword) do
    include_keyword
    |> Enum.reduce({preloaded, []}, fn {relationship, further},
                                       {preloaded_without_linkage, includes_list} ->
      {related, further_includes} =
        preloaded
        |> Enum.flat_map(fn record ->
          record
          |> Map.get(relationship, [])
          |> List.wrap()
        end)
        |> get_includes_list(further)

      preloaded_with_linkage =
        Enum.map(
          preloaded_without_linkage,
          &add_linkage(&1, relationship, related)
        )

      {preloaded_with_linkage, [related, further_includes, includes_list]}
    end)
    |> flatten_includes_list()
  end

  defp add_linkage(record, relationship, related) do
    # TODO: Structify linkage
    record
    |> Map.put_new(:__linkage__, %{})
    |> Map.update!(:__linkage__, fn linkage ->
      Map.put(linkage, relationship, Enum.map(related, & &1.id))
    end)
  end

  defp flatten_includes_list({related, includes_list}) do
    {related, List.flatten(includes_list)}
  end

  defp includes_to_keyword(includes) do
    Enum.reduce(includes, [], fn path, acc ->
      put_path(acc, path)
    end)
  end

  defp put_path(keyword, [key]) do
    atom_key = String.to_existing_atom(key)
    Keyword.put_new(keyword, atom_key, [])
  end

  defp put_path(keyword, [key | rest]) do
    atom_key = String.to_existing_atom(key)

    keyword
    |> Keyword.put_new(atom_key, [])
    |> Keyword.update!(atom_key, &put_path(&1, rest))
  end
end

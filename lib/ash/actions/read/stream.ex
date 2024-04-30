defmodule Ash.Actions.Read.Stream do
  @moduledoc false

  def run!(domain, query, opts) do
    query = Ash.Query.new(query)

    query =
      if query.action do
        query
      else
        Ash.Query.for_read(
          query,
          Ash.Resource.Info.primary_action!(query.resource, :read).name
        )
      end

    query
    |> stream_strategy!(opts[:stream_with], opts[:allow_stream_with])
    |> do_stream(query, domain, Keyword.drop(opts, [:stream_with, :allow_stream_with]))
  end

  defp do_stream(:keyset, query, domain, opts) do
    {batch_size, opts} =
      Keyword.pop(
        opts,
        :batch_size,
        query.action.pagination.default_limit || query.action.pagination.max_page_size || 250
      )

    query = Ash.Query.set_context(query, %{private: %{bypass_max_page_size?: true}})

    Stream.resource(
      fn -> nil end,
      fn
        false ->
          {:halt, nil}

        after_keyset ->
          keyset = if after_keyset != nil, do: [after: after_keyset], else: []
          page_opts = Keyword.merge(keyset, limit: batch_size)

          opts =
            Keyword.merge(opts, page: page_opts, domain: domain)

          case Ash.read!(query, opts) do
            %{more?: true, results: results} ->
              {results, List.last(results).__metadata__.keyset}

            %{results: results} ->
              {results, false}
          end
      end,
      & &1
    )
    |> take_query_limit(query)
  end

  defp do_stream(:offset, query, domain, opts) do
    if can_pagination_offset?(query) do
      stream_with_offset_pagination(query, domain, opts)
    else
      stream_with_limit_offset(query, domain, opts)
    end
  end

  defp do_stream(:full_read, query, domain, opts) do
    opts = Keyword.drop(opts, [:batch_size])

    Stream.resource(
      fn -> true end,
      fn
        false ->
          {:halt, false}

        true ->
          {Ash.read!(query, Keyword.put(opts, :domain, domain)), false}
      end,
      & &1
    )
  end

  defp stream_with_offset_pagination(query, domain, opts) do
    {limit, opts} =
      Keyword.pop(
        opts,
        :batch_size,
        query.action.pagination.default_limit || query.action.pagination.max_page_size || 250
      )

    query = Ash.Query.set_context(query, %{private: %{bypass_max_page_size?: true}})

    Stream.resource(
      fn -> 0 end,
      fn
        false ->
          {:halt, nil}

        offset ->
          page_opts = [limit: limit, offset: offset]

          opts =
            Keyword.put(opts, :page, page_opts)

          case Ash.read!(query, Keyword.put(opts, :domain, domain)) do
            %{more?: true, results: results} ->
              {results, offset + limit}

            %{results: results} ->
              {results, false}
          end
      end,
      & &1
    )
    |> take_query_limit(query)
  end

  defp stream_with_limit_offset(query, domain, opts) do
    {limit, opts} =
      Keyword.pop(
        opts,
        :batch_size,
        (query.action.pagination &&
           (query.action.pagination.default_limit ||
              query.action.pagination.max_page_size)) || 250
      )

    query = Ash.Query.set_context(query, %{private: %{bypass_max_page_size?: true}})

    Stream.resource(
      fn -> 0 end,
      fn
        false ->
          {:halt, nil}

        offset ->
          query =
            query
            |> Ash.Query.limit(limit)
            |> Ash.Query.offset(offset)

          results = Ash.read!(query, Keyword.put(opts, :domain, domain))

          if Enum.count(results) < limit do
            {results, false}
          else
            {results, offset + limit}
          end
      end,
      & &1
    )
    |> take_query_limit(query)
  end

  @doc false
  def stream_strategy!(query, chosen_strategy, allowed_strategy) do
    case stream_strategy(query, chosen_strategy, allowed_strategy) do
      {:error, error} ->
        raise error

      strategy ->
        strategy
    end
  end

  def stream_strategy(query, chosen_strategy, _) when not is_nil(chosen_strategy) do
    case chosen_strategy do
      :keyset ->
        if can_keyset?(query) do
          :keyset
        else
          {:error,
           Ash.Error.Invalid.NonStreamableAction.exception(
             resource: query.resource,
             action: query.action,
             types: [:keyset]
           )}
        end

      :offset ->
        if can_offset?(query) do
          :offset
        else
          {:error,
           Ash.Error.Invalid.NonStreamableAction.exception(
             resource: query.resource,
             action: query.action,
             types: [:offset]
           )}
        end

      :full_read ->
        :full_read
    end
  end

  def stream_strategy(query, nil, allowed_strategy) when not is_nil(allowed_strategy) do
    cond do
      can_keyset?(query) and allowed_strategy in [:keyset, :offset, :full_read] ->
        :keyset

      can_offset?(query) and allowed_strategy in [:offset, :full_read] ->
        :offset

      allowed_strategy == :full_read ->
        :full_read

      allowed_strategy == :keyset ->
        {:error,
         Ash.Error.Invalid.NonStreamableAction.exception(
           resource: query.resource,
           action: query.action,
           types: [:keyset]
         )}

      allowed_strategy == :offset ->
        Ash.Error.Invalid.NonStreamableAction.exception(
          resource: query.resource,
          action: query.action,
          types: [:keyset, :offset]
        )
    end
  end

  def stream_strategy(query, nil, :full_read) do
    if Ash.DataLayer.data_layer_can?(query.resource, :limit) &&
         Ash.DataLayer.data_layer_can?(query.resource, :offset) do
      :full_read
    else
      {:error,
       Ash.Error.Invalid.NonStreamableAction.exception(
         resource: query.resource,
         action: query.action,
         type: :keyset
       )}
    end
  end

  defp can_offset?(query) do
    not requires_keyset_pagination?(query) and
      (can_pagination_offset?(query) || can_limit_offset?(query))
  end

  defp can_pagination_offset?(query) do
    query.action.pagination && query.action.pagination.offset?
  end

  def requires_keyset_pagination?(query) do
    query.action.pagination && query.action.pagination.keyset? &&
      not query.action.pagination.offset? &&
      query.action.pagination.required?
  end

  defp can_limit_offset?(query) do
    Ash.DataLayer.data_layer_can?(query.resource, :limit) &&
      Ash.DataLayer.data_layer_can?(query.resource, :offset)
  end

  defp can_keyset?(query) do
    query.action.pagination && query.action.pagination.keyset?
  end

  defp take_query_limit(stream, query) do
    if query.limit do
      Stream.take(stream, query.limit)
    else
      stream
    end
  end
end

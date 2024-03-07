defmodule Ash.Helpers do
  @moduledoc false

  @spec try_compile(term) :: :ok
  def try_compile(module) when is_atom(module) do
    try do
      # This is to get the compiler to ensure that the resource is compiled
      # For some very strange reason, `Code.ensure_compiled/1` isn't enough
      module.ash_dsl_config()
    rescue
      _ ->
        :ok
    end

    Code.ensure_compiled!(module)
    :ok
  end

  def try_compile(_), do: :ok

  def get_action(resource, params, type, preset \\ nil) do
    case Keyword.fetch(params, :action) do
      {:ok, %_{} = action} ->
        {:ok, action}

      {:ok, nil} ->
        if preset do
          get_action(resource, Keyword.put(params, :action, preset), type)
        else
          get_action(resource, Keyword.delete(params, :action), type)
        end

      {:ok, action} ->
        case Ash.Resource.Info.action(resource, action, type) do
          nil ->
            {:error,
             Ash.Error.Invalid.NoSuchAction.exception(
               resource: resource,
               action: action,
               type: type
             )}

          action ->
            {:ok, action}
        end

      :error ->
        if preset do
          get_action(resource, Keyword.put(params, :action, preset), type)
        else
          case Ash.Resource.Info.primary_action(resource, type) do
            nil ->
              if Ash.Resource.Info.resource?(resource) do
                {:error,
                 Ash.Error.Invalid.NoPrimaryAction.exception(resource: resource, type: type)}
              else
                {:error, Ash.Error.Invalid.NoSuchResource.exception(resource: resource)}
              end

            action ->
              {:ok, action}
          end
        end
    end
  end

  def pagination_check(action, resource, opts) do
    if Keyword.get(opts, :page) && Keyword.get(opts, :page) != [] && !Map.get(action, :pagination) do
      {:error,
       Ash.Error.to_error_class(
         Ash.Error.Invalid.ActionRequiresPagination.exception(resource: resource, action: action)
       )}
    else
      {:ok, action}
    end
  end

  def unwrap_one({:error, error}) do
    {:error, error}
  end

  def unwrap_one({:ok, result, query}) do
    case unwrap_one({:ok, result}) do
      {:ok, result} ->
        {:ok, result, query}

      {:error, %Ash.Error.Invalid.MultipleResults{} = error} ->
        {:error, %{error | query: query}}

      {:error, error} ->
        {:error, error}
    end
  end

  def unwrap_one({:ok, result}) do
    case unwrap_one(result) do
      {:ok, result} ->
        {:ok, result}

      {:error, error} ->
        {:error, error}
    end
  end

  def unwrap_one(%{results: results}) do
    unwrap_one(results)
  end

  def unwrap_one([]), do: {:ok, nil}
  def unwrap_one([result]), do: {:ok, result}

  def unwrap_one([_ | _] = results) do
    error =
      Ash.Error.Invalid.MultipleResults.exception(
        count: Enum.count(results),
        at_least?: true
      )

    {:error, error}
  end

  def resource_from_data!(data, query, opts) do
    if opts[:resource] do
      opts[:resource]
    else
      case query do
        %Ash.Query{resource: resource} -> resource
        _ -> do_resource_from_data!(data)
      end
    end
  end

  defp do_resource_from_data!(%struct{rerun: {%Ash.Query{resource: resource}, _}})
       when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    resource
  end

  defp do_resource_from_data!(%struct{results: [%resource{} | _]} = data)
       when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!(%resource{} = data) do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!([%resource{} | _] = data) do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!(data) do
    raise_no_resource_error!(data)
  end

  defp raise_no_resource_error!(data) do
    raise ArgumentError,
      message: """
      Could not determine a resource from the provided input:

      #{inspect(data)}
      """
  end

  def domain!(%input_struct{domain: domain}, _opts)
      when input_struct in [Ash.Query, Ash.Changeset, Ash.ActionInput] and is_atom(domain) and
             not is_nil(domain) do
    domain
  end

  def domain!([record | _], opts) do
    domain!(record, opts)
  end

  def domain!(%page_struct{rerun: {query, _}}, opts)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    domain!(query, opts)
  end

  def domain!(%page_struct{results: results}, opts)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    domain!(results, opts)
  end

  def domain!(%{resource: resource}, opts) when not is_nil(resource) do
    domain!(resource, opts)
  end

  def domain!(nil, opts) do
    opts[:domain] ||
      raise(ArgumentError, "Could not determine domain. Please specify the `:domain` option.")
  end

  def domain!(%resource{}, opts) do
    if Ash.Resource.Info.resource?(resource) do
      domain!(resource, opts)
    else
      domain!(nil, opts)
    end
  end

  def domain!(resource, opts) do
    opts[:domain] ||
      Ash.Resource.Info.domain(resource) ||
      raise(
        ArgumentError,
        "Could not determine domain for #{inspect(resource)}. Please specify the `:domain` option or ensure the resource has a configured domain."
      )
  end

  def unwrap_or_raise!(first, destroy? \\ false)
  def unwrap_or_raise!(%Ash.BulkResult{} = bulk_result, _), do: bulk_result
  def unwrap_or_raise!(:ok, _), do: :ok
  def unwrap_or_raise!({:ok, result}, false), do: result
  def unwrap_or_raise!({:ok, _result}, true), do: :ok
  def unwrap_or_raise!({:ok, result, other}, _), do: {result, other}

  def unwrap_or_raise!({:error, error}, destroy?) when is_list(error) do
    unwrap_or_raise!({:error, Ash.Error.to_error_class(error)}, destroy?)
  end

  def unwrap_or_raise!({:error, error}, _) do
    exception = Ash.Error.to_error_class(error)

    case exception do
      %{stacktrace: %{stacktrace: stacktrace}} = exception ->
        reraise exception, stacktrace

      _ ->
        raise exception
    end
  end

  def implements_behaviour?(module, behaviour) do
    :attributes
    |> module.module_info()
    |> Enum.flat_map(fn
      {:behaviour, value} -> List.wrap(value)
      _ -> []
    end)
    |> Enum.any?(&(&1 == behaviour))
  rescue
    _ ->
      false
  end

  # sobelow_skip ["Misc.BinToTerm"]
  def non_executable_binary_to_term(binary, opts \\ []) when is_binary(binary) do
    term = :erlang.binary_to_term(binary, opts)
    non_executable_terms(term)
    term
  end

  defp non_executable_terms(list) when is_list(list) do
    non_executable_list(list)
  end

  defp non_executable_terms(tuple) when is_tuple(tuple) do
    non_executable_tuple(tuple, tuple_size(tuple))
  end

  defp non_executable_terms(map) when is_map(map) do
    folder = fn key, value, acc ->
      non_executable_terms(key)
      non_executable_terms(value)
      acc
    end

    :maps.fold(folder, map, map)
  end

  defp non_executable_terms(other)
       when is_atom(other) or is_number(other) or is_bitstring(other) or is_pid(other) or
              is_reference(other) do
    other
  end

  defp non_executable_terms(other) do
    raise ArgumentError,
          "cannot deserialize #{inspect(other)}, the term is not safe for deserialization"
  end

  defp non_executable_list([]), do: :ok

  defp non_executable_list([h | t]) when is_list(t) do
    non_executable_terms(h)
    non_executable_list(t)
  end

  defp non_executable_list([h | t]) do
    non_executable_terms(h)
    non_executable_terms(t)
  end

  defp non_executable_tuple(_tuple, 0), do: :ok

  defp non_executable_tuple(tuple, n) do
    non_executable_terms(:erlang.element(n, tuple))
    non_executable_tuple(tuple, n - 1)
  end

  @doc false
  def deep_merge_maps(left, right)
      when is_map(left) and is_map(right) and not is_struct(left) and not is_struct(right) do
    Map.merge(left, right, fn _, left, right ->
      deep_merge_maps(left, right)
    end)
  end

  def deep_merge_maps(_left, right), do: right
end

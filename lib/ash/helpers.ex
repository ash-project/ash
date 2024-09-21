defmodule Ash.Helpers do
  @moduledoc false

  require Logger

  @dialyzer {:nowarn_function, {:unwrap_or_raise!, 2}}

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

  def flatten_preserving_keywords(list) do
    if Keyword.keyword?(list) do
      [list]
    else
      list
      |> List.wrap()
      |> Enum.flat_map(fn item ->
        cond do
          Keyword.keyword?(item) ->
            [item]

          is_list(item) ->
            flatten_preserving_keywords(item)

          true ->
            [item]
        end
      end)
    end
  end

  defmacro expect_resource!(resource) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [resource: resource, formatted: formatted] do
      if !Ash.Resource.Info.resource?(resource) do
        raise ArgumentError,
              "Expected an `Ash.Resource` in #{formatted}, got: #{inspect(resource)}"
      end
    end
  end

  defmacro expect_resource_or_query!(resource_or_query) do
    formatted = format_caller(__CALLER__)

    quote generated: true,
          bind_quoted: [resource_or_query: resource_or_query, formatted: formatted] do
      case resource_or_query do
        %Ash.Query{} ->
          :ok

        other ->
          if !Ash.Resource.Info.resource?(other) do
            raise ArgumentError,
                  "Expected an `%Ash.Query{}` or an `Ash.Resource` in #{formatted}, got: #{inspect(other)}"
          end
      end
    end
  end

  defmacro expect_query!(query) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [query: query, formatted: formatted] do
      case query do
        %Ash.Query{} ->
          :ok

        other ->
          raise ArgumentError,
                "Expected an `%Ash.Query{}` in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  defmacro expect_changeset!(changeset) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [changeset: changeset, formatted: formatted] do
      case changeset do
        %Ash.Changeset{} ->
          :ok

        other ->
          raise ArgumentError,
                "Expected an `%Ash.Changeset{}` in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  defmacro expect_resource_or_record!(resource) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [resource: resource, formatted: formatted] do
      case resource do
        %resource{} = record ->
          if !Ash.Resource.Info.resource?(resource) do
            raise ArgumentError,
                  "Expected an `Ash.Resource` or a record in #{formatted}, got: #{inspect(record)}"
          end

        other ->
          if !Ash.Resource.Info.resource?(resource) do
            raise ArgumentError,
                  "Expected an `Ash.Resource` or a record in #{formatted}, got: #{inspect(other)}"
          end
      end
    end
  end

  defmacro expect_changeset_or_record!(changeset_or_record) do
    formatted = format_caller(__CALLER__)

    quote generated: true,
          bind_quoted: [changeset_or_record: changeset_or_record, formatted: formatted] do
      case changeset_or_record do
        %Ash.Changeset{} ->
          :ok

        %resource{} = record ->
          if !Ash.Resource.Info.resource?(resource) do
            raise ArgumentError,
                  "Expected an `Ash.Resource` or a record in #{formatted}, got: #{inspect(record)}"
          end

        other ->
          raise ArgumentError,
                "Expected an `Ash.Resource` or a record in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  defmacro expect_record!(record) do
    formatted = format_caller(__CALLER__)

    quote bind_quoted: [record: record, formatted: formatted] do
      case record do
        %resource{} = record ->
          if !Ash.Resource.Info.resource?(resource) do
            raise ArgumentError,
                  "Expected a record in #{formatted}, got: #{inspect(record)}"
          end

        other ->
          raise ArgumentError,
                "Expected a record in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  defmacro expect_options!(options) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [options: options, formatted: formatted] do
      case options do
        [] ->
          :ok

        [{atom, _} | _] when is_atom(atom) ->
          :ok

        other ->
          raise ArgumentError,
                "Expected a keyword list in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  defmacro expect_map_or_nil!(map_or_nil) do
    formatted = format_caller(__CALLER__)

    quote generated: true, bind_quoted: [map_or_nil: map_or_nil, formatted: formatted] do
      case map_or_nil do
        nil ->
          :ok

        map when is_map(map) ->
          :ok

        other ->
          raise ArgumentError,
                "Expected a keyword list in #{formatted}, got: #{inspect(other)}"
      end
    end
  end

  def resource_from_query_or_stream(domain, query_or_stream, opts) do
    resource =
      opts[:resource] ||
        case query_or_stream do
          [%resource{} | _] ->
            resource

          %Ash.Query{resource: resource} ->
            resource

          resource when is_atom(resource) ->
            if Ash.Resource.Info.resource?(resource) do
              resource
            end

          _ ->
            nil
        end

    if !resource do
      raise ArgumentError,
            "Could not determine resource for bulk action. Please provide the `resource` option if providing a stream of inputs."
    end

    Ash.Domain.Info.resource(domain, resource)
  end

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

  @doc """
  Returns {params, opts} from ambigous inputs.
  """
  def get_params_and_opts(params_or_opts, opts) do
    if opts == [] && Keyword.keyword?(params_or_opts) do
      {%{}, params_or_opts}
    else
      {params_or_opts, opts}
    end
  end

  def pagination_check(action, query, opts) do
    page = query.page || opts[:page]

    if page && page != [] && !Map.get(action, :pagination) do
      {:error,
       Ash.Error.to_error_class(
         Ash.Error.Invalid.ActionRequiresPagination.exception(
           resource: query.resource,
           action: action
         )
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

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error, query: query)}
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

  defmacro domain!(
             subject,
             opts,
             instructions \\ "Please specify the `:domain` option, or adjust the input."
           ) do
    formatted = format_caller(__CALLER__)

    quote generated: true,
          bind_quoted: [
            subject: subject,
            opts: opts,
            formatted: formatted,
            instructions: instructions
          ] do
      domain = Ash.Helpers.get_domain(subject, opts)

      if !domain do
        expanded =
          if not is_nil(subject) do
            "\n\n#{inspect(subject)}"
          end

        raise(
          ArgumentError,
          """
          Could not determine domain for input in #{formatted}. #{instructions}#{expanded}
          """
        )
      end

      domain
    end
  end

  defp format_caller(caller) do
    mod = caller.module
    {func, arity} = caller.function
    "`#{inspect(mod)}.#{func}/#{arity}`"
  end

  def get_domain(nil, nil) do
    nil
  end

  def get_domain(%input_struct{domain: domain}, _opts)
      when input_struct in [Ash.Query, Ash.Changeset, Ash.ActionInput] and not is_nil(domain) do
    domain
  end

  def get_domain(%input_struct{resource: resource}, opts)
      when input_struct in [Ash.Query, Ash.Changeset, Ash.ActionInput] do
    get_domain(resource, opts)
  end

  def get_domain([record | _], opts) do
    get_domain(record, opts)
  end

  def get_domain({%input_struct{} = input, _}, opts)
      when input_struct in [Ash.Query, Ash.Changeset, Ash.ActionInput] do
    get_domain(input, opts)
  end

  def get_domain({%input_struct{} = input, _, _}, opts)
      when input_struct in [Ash.Query, Ash.Changeset, Ash.ActionInput] do
    get_domain(input, opts)
  end

  def get_domain({%resource{}, _}, opts) do
    get_domain(resource, opts)
  end

  def get_domain({%resource{}, _, _}, opts) do
    get_domain(resource, opts)
  end

  def get_domain({resource, _}, opts) when is_atom(resource) do
    get_domain(resource, opts)
  end

  def get_domain({resource, _, _}, opts) when is_atom(resource) do
    get_domain(resource, opts)
  end

  def get_domain(%page_struct{rerun: {query, _}}, opts)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    get_domain(query, opts)
  end

  def get_domain(%page_struct{results: results}, opts)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    get_domain(results, opts)
  end

  def get_domain(%{resource: resource}, opts) when not is_nil(resource) do
    get_domain(resource, opts)
  end

  def get_domain(nil, opts) do
    cond do
      domain = opts[:domain] ->
        domain

      resource = opts[:resource] ->
        get_domain(resource, Keyword.delete(opts, :resource))

      true ->
        nil
    end
  end

  def get_domain(%resource{}, opts) do
    if Ash.Resource.Info.resource?(resource) do
      get_domain(resource, opts)
    else
      get_domain(nil, opts)
    end
  end

  def get_domain(resource, opts) do
    domain_from_resource(resource) || opts[:domain]
  end

  defp domain_from_resource(resource) do
    if Ash.Resource.Info.resource?(resource) || (is_map(resource) and not is_struct(resource)) do
      Ash.Resource.Info.domain(resource) ||
        Ash.Actions.Helpers.maybe_embedded_domain(resource)
    end
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

  @doc false
  if Application.compile_env(:ash, :show_sensitive?, false) do
    def redact(value), do: value
  else
    def redact(_value), do: "**redacted**"
  end

  @spec verify_stream_options(options :: Keyword.t()) :: :ok
  def verify_stream_options(options) do
    with true <- Keyword.get(options, :return_stream?, false),
         false <- Keyword.get(options, :return_notifications?, false),
         false <- Keyword.get(options, :return_records?, false),
         false <- Keyword.get(options, :return_errors?, false),
         false <- Keyword.get(options, :return_nothing?, false) do
      Logger.warning("""
      Bulk action was called with :return_stream? set to true, but no other :return_*? options were set.
      You probably want to set :return_notifications?, :return_records?, or :return_errors? as well.
      To disable this message, set :return_nothing? to true.\
      """)
    end

    :ok
  end
end

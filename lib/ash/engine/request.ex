defmodule Ash.Engine.Request do
  @fields_that_change_sometimes [
    :changeset,
    :is_fetched,
    :strict_check_completed?
  ]

  defstruct [
    :resource,
    :rules,
    :filter,
    :action_type,
    :dependencies,
    :bypass_strict_access?,
    :relationship,
    :fetcher,
    :source,
    :optional_state,
    :must_fetch?,
    :is_fetched,
    :state_key,
    :strict_check_completed?,
    :api,
    :changeset
  ]

  @type t :: %__MODULE__{
          action_type: atom,
          resource: Ash.resource(),
          rules: list(term),
          filter: Ash.Filter.t(),
          changeset: Ecto.Changeset.t(),
          dependencies: list(term),
          optional_state: list(term),
          is_fetched: (term -> boolean),
          fetcher: term,
          relationship: list(atom),
          bypass_strict_access?: boolean,
          strict_check_completed?: boolean,
          source: String.t(),
          must_fetch?: boolean,
          state_key: term,
          api: Ash.api()
        }

  def new(opts) do
    opts =
      opts
      |> Keyword.put_new(:relationship, [])
      |> Keyword.put_new(:rules, [])
      |> Keyword.put_new(:bypass_strict_access?, false)
      |> Keyword.update(:dependencies, [], &List.wrap/1)
      |> Keyword.update(:optional_state, [], &List.wrap/1)
      |> Keyword.put_new(:strict_check_completed?, false)
      |> Keyword.put_new(:is_fetched, fn _ -> true end)
      |> Keyword.put_new(:must_fetch?, false)
      |> Keyword.update!(:rules, fn steps ->
        Enum.map(steps, fn {step, fact} ->
          {step, Ash.Authorization.Clause.new(opts[:relationship] || [], opts[:resource], fact)}
        end)
      end)

    struct!(__MODULE__, opts)
  end

  def authorize_always(request) do
    %{
      request
      | rules: [
          authorize_if:
            Ash.Authorization.Clause.new(
              request.relationship,
              request.resource,
              {Ash.Authorization.Check.Static, result: true}
            )
        ]
    }
  end

  def can_strict_check?(%{changeset: changeset}) when is_function(changeset), do: false
  def can_strict_check?(%{filter: filter}) when is_function(filter), do: false
  def can_strict_check?(%{strict_check_completed?: false}), do: true
  def can_strict_check?(_), do: false

  def dependencies_met?(_state, %{dependencies: []}), do: true
  def dependencies_met?(_state, %{dependencies: nil}), do: true

  def dependencies_met?(state, %{dependencies: dependencies}) do
    Enum.all?(dependencies, fn dependency ->
      case fetch_nested_value(state, dependency) do
        {:ok, _} -> true
        _ -> false
      end
    end)
  end

  def unmet_dependencies(_state, %{dependencies: []}), do: []
  def unmet_dependencies(_state, %{dependencies: nil}), do: []

  def unmet_dependencies(state, %{dependencies: dependencies}) do
    Enum.reject(dependencies, fn dependency ->
      case fetch_nested_value(state, dependency) do
        {:ok, _} -> true
        _ -> false
      end
    end)
  end

  def contains_clause?(request, clause) do
    Enum.any?(request.rules, fn {_step, request_clause} ->
      clause == request_clause
    end)
  end

  def fetched?(_, %{is_fetched: boolean}) when is_boolean(boolean) do
    boolean
  end

  def fetched?(state, request) do
    case fetch_request_state(state, request) do
      {:ok, value} ->
        request.is_fetched.(value)

      :error ->
        false
    end
  end

  def depends_on?(request, other_request) do
    state_key(request) in other_request.dependencies
  end

  def state_key(%{state_key: state_key} = request) do
    List.wrap(state_key || Map.drop(request, @fields_that_change_sometimes))
  end

  def put_request_state(state, request, value) do
    key = state_key(request)

    put_nested_key(state, key, value)
  end

  def fetch_request_state(state, request) do
    key = state_key(request)

    fetch_nested_value(state, key)
  end

  def fetch(
        state,
        %{fetcher: fetcher, changeset: changeset} = request
      ) do
    fetcher_state =
      %{}
      |> add_dependent_state(state, request)
      |> add_optional_state(state, request)

    case fetcher.(changeset, fetcher_state) do
      {:ok, value} ->
        {:ok, put_request_state(state, request, value)}

      {:error, error} ->
        {:error, error}
    end
  end

  def dependent_fields_fetched?(%{changeset: changeset}) when is_function(changeset), do: false
  def dependent_fields_fetched?(%{filter: filter}) when is_function(filter), do: false
  def dependent_fields_fetched?(%{changeset: _}), do: true

  def fetch_dependent_fields(state, request) do
    fetcher_state =
      %{}
      |> add_dependent_state(state, request)
      |> add_optional_state(state, request)

    case fetch_changeset(fetcher_state, request) do
      {:ok, request} ->
        fetch_filter(fetcher_state, request)

      {:error, error} ->
        {:error, error}
    end
  end

  def fetch_nested_value(state, [key]) when is_map(state) do
    Map.fetch(state, key)
  end

  def fetch_nested_value(state, [key | rest]) when is_map(state) do
    case Map.fetch(state, key) do
      {:ok, value} -> fetch_nested_value(value, rest)
      :error -> :error
    end
  end

  def fetch_nested_value(state, key) when is_map(state) do
    Map.fetch(state, key)
  end

  defp add_dependent_state(arg, state, %{dependencies: dependencies}) do
    Enum.reduce(dependencies, arg, fn dependency, acc ->
      {:ok, value} = fetch_nested_value(state, dependency)
      put_nested_key(acc, dependency, value)
    end)
  end

  defp add_optional_state(arg, state, %{optional_state: optional_state}) do
    Enum.reduce(optional_state, arg, fn optional, arg ->
      case fetch_nested_value(state, optional) do
        {:ok, value} -> put_nested_key(arg, optional, value)
        :error -> arg
      end
    end)
  end

  defp fetch_changeset(state, %{dependencies: dependencies, changeset: changeset} = request)
       when is_function(changeset) do
    arg =
      Enum.reduce(dependencies, %{}, fn dependency, acc ->
        {:ok, value} = fetch_nested_value(state, dependency)
        put_nested_key(acc, dependency, value)
      end)

    case changeset.(arg) do
      %Ecto.Changeset{} = new_changeset ->
        {:ok, %{request | changeset: new_changeset}}

      {:ok, new_changeset} ->
        {:ok, %{request | changeset: new_changeset}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp fetch_changeset(_state, request), do: {:ok, request}

  defp fetch_filter(state, %{dependencies: dependencies, filter: filter} = request)
       when is_function(filter) do
    arg =
      Enum.reduce(dependencies, %{}, fn dependency, acc ->
        {:ok, value} = fetch_nested_value(state, dependency)
        put_nested_key(acc, dependency, value)
      end)

    case filter.(arg) do
      %Ash.Filter{} = new_filter ->
        {:ok, %{request | filter: new_filter}}

      {:ok, new_filter} ->
        {:ok, %{request | filter: new_filter}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp fetch_filter(_state, request), do: {:ok, request}

  defp put_nested_key(state, [key], value) do
    Map.put(state, key, value)
  end

  defp put_nested_key(state, [key | rest], value) do
    case Map.fetch(state, key) do
      {:ok, nested_state} when is_map(nested_state) ->
        Map.put(state, key, put_nested_key(nested_state, rest, value))

      :error ->
        Map.put(state, key, put_nested_key(%{}, rest, value))
    end
  end

  defp put_nested_key(state, key, value) do
    Map.put(state, key, value)
  end
end

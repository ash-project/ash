defmodule Ash.Authorization.Request do
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
          is_fetched: (term -> boolean),
          # TODO: fetcher is a function
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
      |> Keyword.put_new(:dependencies, [])
      |> Keyword.put_new(:strict_check_completed?, false)
      |> Keyword.put_new(:is_fetched, fn _ -> true end)
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

  def put_request_state(state, %{state_key: state_key} = request, value) do
    state_key = state_key || request

    key =
      state_key
      |> Kernel.||(request)
      |> List.wrap()

    put_nested_key(state, key, value)
  end

  def fetch_request_state(state, %{state_key: state_key} = request) do
    state_key = state_key || request

    key =
      state_key
      |> Kernel.||(request)
      |> List.wrap()

    fetch_nested_value(state, key)
  end

  def fetch(
        state,
        %{fetcher: fetcher, dependencies: dependencies, changeset: changeset} = request
      ) do
    fetcher_state =
      Enum.reduce(dependencies, %{}, fn dependency, acc ->
        {:ok, value} = fetch_nested_value(state, dependency)
        put_nested_key(acc, dependency, value)
      end)

    case fetcher.(changeset, fetcher_state) do
      {:ok, value} ->
        {:ok, put_request_state(state, request, value)}

      {:error, error} ->
        {:error, error}
    end
  end

  def changeset_fetched?(%{changeset: changeset}) when is_function(changeset), do: false
  def changeset_fetched?(%{changeset: _}), do: true

  def fetch_changeset(state, %{dependencies: dependencies, changeset: changeset} = request)
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

  def fetch_changeset(_state, request), do: {:ok, request}

  defp fetch_nested_value(state, [key]) when is_map(state) do
    Map.fetch(state, key)
  end

  defp fetch_nested_value(state, [key | rest]) when is_map(state) do
    case Map.fetch(state, key) do
      {:ok, value} -> fetch_nested_value(value, rest)
      :error -> :error
    end
  end

  defp fetch_nested_value(state, key) when is_map(state) do
    Map.fetch(state, key)
  end

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

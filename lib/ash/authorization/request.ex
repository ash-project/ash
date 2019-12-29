defmodule Ash.Authorization.Request do
  defstruct [
    :resource,
    :authorization_steps,
    :filter,
    :action_type,
    :bypass_strict_access?,
    :relationship,
    :fetcher,
    :source,
    :must_fetch?,
    :state_key,
    :api,
    :changeset
  ]

  @type t :: %__MODULE__{
          action_type: atom,
          resource: Ash.resource(),
          authorization_steps: list(term),
          filter: Ash.Filter.t(),
          changeset: Ecto.Changeset.t(),
          # TODO: fetcher is a function
          fetcher: term,
          relationship: list(atom),
          bypass_strict_access?: boolean,
          source: String.t(),
          must_fetch?: boolean,
          state_key: term,
          api: Ash.api()
        }

  def new(opts) do
    opts =
      opts
      |> Keyword.put_new(:relationship, [])
      |> Keyword.put_new(:authorization_steps, [])
      |> Keyword.put_new(:bypass_strict_access?, false)
      |> Keyword.update!(:authorization_steps, fn steps ->
        Enum.map(steps, fn {step, fact} ->
          {step, Ash.Authorization.Clause.new(opts[:relationship] || [], opts[:resource], fact)}
        end)
      end)

    struct!(__MODULE__, opts)
  end

  def contains_clause?(request, clause) do
    Enum.any?(request.authorization_steps, fn {_step, request_clause} ->
      clause == request_clause
    end)
  end

  def fetched?(state, request) do
    case fetch_request_state(state, request) do
      {:ok, _} -> true
      :error -> false
    end
  end

  def put_request_state(state, %{state_key: state_key} = request, value) do
    state_key = state_key || request
    Map.put(state, state_key, value)
  end

  def fetch_request_state(state, %{state_key: state_key} = request) do
    state_key = state_key || request

    Map.fetch(state, state_key)
  end
end

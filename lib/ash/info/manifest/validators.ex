# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Error.NonPublicAccept do
  @moduledoc """
  Raised by `Ash.Info.Manifest.Generator` when an action entrypoint's accept list
  includes one or more non-public attributes.

  An API specification describes a public surface; accepting a non-public
  attribute via a public entrypoint would silently expose data the resource
  author marked `public?: false`.
  """
  defexception [:resource, :action, :attributes, :message]

  @impl true
  def exception(opts) do
    resource = Keyword.fetch!(opts, :resource)
    action = Keyword.fetch!(opts, :action)
    attributes = Keyword.fetch!(opts, :attributes)

    message =
      "Action #{inspect(action)} on #{inspect(resource)} is exposed as an API entrypoint, " <>
        "but its accept list includes non-public attribute(s): #{inspect(attributes)}. " <>
        "Only `public?: true` attributes may appear in the accept list of a public entrypoint. " <>
        "Either mark the attribute(s) as `public? true`, remove them from the action's accept list, " <>
        "or do not expose the action as an entrypoint."

    %__MODULE__{
      resource: resource,
      action: action,
      attributes: attributes,
      message: message
    }
  end
end

defmodule Ash.Info.Manifest.Validators do
  @moduledoc """
  Validation helpers for resources and actions that are exposed via a
  public-facing API (e.g. AshTypescript RPC, AshJsonApi routes, AshGraphql
  mutations).

  These checks describe rules that hold for any public API surface and are
  enforced by `Ash.Info.Manifest.Generator` when building a spec, so every
  extension that consumes the spec gets the same guarantees.
  """

  alias Ash.Info.Manifest.Error.NonPublicAccept

  @doc """
  Returns `:ok` if every attribute in `action`'s accept list is `public?: true`
  on `resource`. Returns `{:error, non_public_attrs}` otherwise.

  `action` may be an Ash action struct, an `%Ash.Info.Manifest.Action{}`, or any
  map with an `:accept` key. An empty or absent accept list is `:ok`.
  """
  @spec validate_accept_public(atom(), map()) :: :ok | {:error, [atom()]}
  def validate_accept_public(resource, action) when is_atom(resource) and is_map(action) do
    accept_list = Map.get(action, :accept) || []

    non_public =
      Enum.reject(accept_list, fn attr_name ->
        Ash.Resource.Info.public_attribute(resource, attr_name) != nil
      end)

    case non_public do
      [] -> :ok
      attrs -> {:error, attrs}
    end
  end

  @doc """
  Raises `Ash.Info.Manifest.Error.NonPublicAccept` if `action`'s accept list contains
  any non-public attributes on `resource`. Used by `Ash.Info.Manifest.Generator` to
  enforce the public-surface invariant when building entrypoints.
  """
  @spec validate_entrypoint!(atom(), map()) :: :ok
  def validate_entrypoint!(resource, action) when is_atom(resource) and is_map(action) do
    case validate_accept_public(resource, action) do
      :ok ->
        :ok

      {:error, attrs} ->
        raise NonPublicAccept,
          resource: resource,
          action: Map.get(action, :name),
          attributes: attrs
    end
  end
end

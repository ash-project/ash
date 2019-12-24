defmodule Ash.Authorization.Authorizer do
  @moduledoc """
  Authorization in Ash is done via declaring `authorization_steps` for actions,
  and in the case of stateful actions, via declaring `authoriation_steps` on attributes
  and relationships.

  In the case of `read` actions
  """
  @type result :: :authorized | :forbidden | :undecided

  def authorize(_user, _context, requests) do
    IO.inspect(requests)
    :authorized
  end
end

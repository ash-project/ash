defmodule Ash.Authorization.Authorizer do
  @moduledoc """
  Authorization in Ash is done via declaring `authorization_steps` for
  """
  @type result :: :authorized | :forbidden | :undecided

  def authorize(_user, _context, requests) do
    IO.inspect(requests)
    :authorized
  end
end

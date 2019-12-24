defmodule Ash.Authorization.Authorizer do
  @type result :: :authorized | :forbidden | :undecided

  def authorize(_user, requests) do
    IO.inspect(requests)
    :authorized
  end
end

defmodule Ash.Authorization.Authorizer do
  @type result :: :authorized | :forbidden | :undecided

  def authorize(_user, _requests) do
    :authorized
  end
end

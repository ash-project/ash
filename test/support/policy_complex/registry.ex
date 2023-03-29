defmodule Ash.Test.Support.PolicyComplex.Registry do
  @moduledoc false
  use Ash.Registry

  alias Ash.Test.Support.PolicyComplex, as: Complex

  entries do
    entry(Complex.User)
    entry(Complex.FriendLink)
    entry(Complex.Post)
    entry(Complex.Comment)
    entry(Complex.Bio)
  end
end

defmodule Ash.Test.Support.PolicyComplex.Api do
  @moduledoc false
  use Ash.Api

  resources do
    resource(Ash.Test.Support.PolicyComplex.User)
    resource(Ash.Test.Support.PolicyComplex.FriendLink)
    resource(Ash.Test.Support.PolicyComplex.Post)
    resource(Ash.Test.Support.PolicyComplex.Comment)
    resource(Ash.Test.Support.PolicyComplex.Bio)
  end
end

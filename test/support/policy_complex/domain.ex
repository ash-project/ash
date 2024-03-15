defmodule Ash.Test.Support.PolicyComplex.Domain do
  @moduledoc false
  use Ash.Domain,
    extensions: [Ash.Policy.Authorizer]

  resources do
    resource(Ash.Test.Support.PolicyComplex.User)
    resource(Ash.Test.Support.PolicyComplex.FriendLink)
    resource(Ash.Test.Support.PolicyComplex.Post)
    resource(Ash.Test.Support.PolicyComplex.Comment)
    resource(Ash.Test.Support.PolicyComplex.Bio)
  end

  policies do
    policy always() do
      authorize_unless actor_attribute_equals(:forbidden_by_domain, true)
    end
  end
end

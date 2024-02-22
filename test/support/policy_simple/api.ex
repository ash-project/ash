defmodule Ash.Test.Support.PolicySimple.Api do
  @moduledoc false
  use Ash.Api

  resources do
    resource(Ash.Test.Support.PolicySimple.User)
    resource(Ash.Test.Support.PolicySimple.Organization)
    resource(Ash.Test.Support.PolicySimple.Post)
    resource(Ash.Test.Support.PolicySimple.Car)
    resource(Ash.Test.Support.PolicySimple.CarUser)
    resource(Ash.Test.Support.PolicySimple.Trip)
    resource(Ash.Test.Support.PolicySimple.Tweet)
  end
end

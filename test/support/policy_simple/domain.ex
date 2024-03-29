defmodule Ash.Test.Support.PolicySimple.Domain do
  @moduledoc false
  use Ash.Domain

  resources do
    resource(Ash.Test.Support.PolicySimple.User)
    resource(Ash.Test.Support.PolicySimple.Organization)
    resource(Ash.Test.Support.PolicySimple.Post)
    resource(Ash.Test.Support.PolicySimple.Car)
    resource(Ash.Test.Support.PolicySimple.CarUser)
    resource(Ash.Test.Support.PolicySimple.Trip)
    resource(Ash.Test.Support.PolicySimple.Tweet)
    resource(Ash.Test.Support.PolicySimple.Foo)
  end
end

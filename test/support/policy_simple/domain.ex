# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

    resource(Ash.Test.Support.PolicySimple.Context) do
      define :update_context, action: :update, args: [:name]
    end

    resource(Ash.Test.Support.PolicySimple.Always)
    resource(Ash.Test.Support.PolicySimple.TwoFilters)
  end
end

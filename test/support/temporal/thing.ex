# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.Thing do
  @moduledoc false
  use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]

    # Generic action that surfaces the `as_of` threaded into its context, so we can
    # assert it flowed from opts (just like tenant does).
    action :reveal_as_of, :term do
      run fn _input, context -> {:ok, context.as_of} end
    end

    # Read action that reports the `as_of` threaded onto the query (it rides in the
    # shared context, the same channel multitenancy uses) back to the caller.
    read :capture_as_of do
      prepare fn query, _context ->
        send(self(), {:captured_as_of, query.context[:shared][:as_of]})
        query
      end
    end
  end

  code_interface do
    define :create_thing, action: :create
    define :read_things, action: :read
    define :capture_as_of, action: :capture_as_of
    define :reveal_as_of, action: :reveal_as_of
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string, public?: true

    create_timestamp :inserted_at
  end
end

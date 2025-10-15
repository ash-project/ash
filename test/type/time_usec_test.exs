# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.TimeUsecTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :time_a, :time_usec do
        public?(true)
      end

      attribute :time_b, :time_usec, allow_nil?: false, public?: true
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        time_a: ~T[09:53:47.984771],
        time_b: ~T[12:38:44.106332]
      })
      |> Ash.create!()

    assert post.time_a == ~T[09:53:47.984771]
    assert post.time_b == ~T[12:38:44.106332]
  end
end

# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.StreamReturnQueryTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, offset?: true, required?: false
      end

      read :offset_only do
        pagination offset?: true, required?: false
      end

      read :no_pagination
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end
  end

  test "`return_query?` is rejected, because a stream runs one read per batch" do
    for opts <- [
          [],
          [action: :offset_only],
          [action: :no_pagination, allow_stream_with: :offset],
          [action: :no_pagination, stream_with: :full_read]
        ] do
      assert_raise Spark.Options.ValidationError, ~r/return_query\?/, fn ->
        Post |> Ash.stream!(Keyword.put(opts, :return_query?, true)) |> Enum.to_list()
      end
    end
  end
end

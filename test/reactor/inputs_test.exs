# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorInputTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      defaults create: :*
    end
  end

  test "when there is no input transform, the and valid keys are input, it works" do
    defmodule ValidatedValidKeysReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :title

      create :create_post, Post, :create do
        inputs(%{title: input(:title)})
      end
    end

    assert {:ok, _post} =
             Reactor.run(ValidatedValidKeysReactor, %{
               title: "10 reasons you'll love Ash actions!"
             })
  end

  test "when there is no input transform, and invalid keys are input, it fails to compile" do
    assert_raise(Spark.Error.DslError, ~r/sub_title.*doesn't exist/, fn ->
      defmodule ValidatedInvalidKeysReactor do
        @moduledoc false
        use Reactor, extensions: [Ash.Reactor]

        input :sub_title

        create :create_post, Post, :create do
          inputs(%{sub_title: input(:sub_title)})
        end
      end
    end)
  end

  test "when there is an input transform, keys are not validated" do
    defmodule UnvalidatedInvalidKeysReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :title
      input :sub_title

      create :create_post, Post, :create do
        inputs %{
          title: input(:title),
          sub_title: input(:sub_title)
        } do
          transform &%{title: "#{&1.title}: #{&1.sub_title}"}
        end
      end
    end

    assert {:ok, post} =
             Reactor.run(UnvalidatedInvalidKeysReactor, %{
               title: "Dr StrangeDesign",
               sub_title: "Or how I learned to quit worrying and love declarative design"
             })

    assert post.title ==
             "Dr StrangeDesign: Or how I learned to quit worrying and love declarative design"
  end
end

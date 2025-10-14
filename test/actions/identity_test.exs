# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.IdentityTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    def testing_identities, do: true

    identities do
      identity :unique_title, [:title] do
        eager_check_with(Domain)
        field_names([:title, :slug])
      end

      identity :unique_url, [:url] do
        pre_check_with(Domain)
      end

      identity :unique_uniq_nil, [:uniq_nil] do
        eager_check_with(Domain)
        nils_distinct?(false)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    calculations do
      calculate :lower_title, :string, expr(string_downcase(title))
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false, public?: true)
      attribute(:url, :string, public?: true)
      attribute(:uniq_nil, :string, public?: true)
    end
  end

  describe "eager_check_with" do
    test "will check for an identity mismatch at validation" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "fred", uniq_nil: "foo"}, domain: Domain)
      |> Ash.create!()

      assert %{
               valid?: false,
               errors: [
                 %Ash.Error.Changes.InvalidChanges{
                   fields: [:title, :slug],
                   message: "has already been taken"
                 }
               ]
             } = Ash.Changeset.for_create(Post, :create, %{title: "fred"})
    end

    test "honors nils_distinct?" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "a"}, domain: Domain)
      |> Ash.create!()

      assert %{
               valid?: false,
               errors: [
                 %Ash.Error.Changes.InvalidChanges{
                   fields: [:uniq_nil],
                   message: "has already been taken"
                 }
               ]
             } = Ash.Changeset.for_create(Post, :create, %{title: "fred"})
    end
  end

  describe "pre_check?" do
    test "will check for an identity mismatch prior to submission" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "fred", url: "google.com", uniq_nil: "foo"},
        domain: Domain
      )
      |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/url: has already been taken/, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{title: "george", url: "google.com"})
        |> Ash.create!()
      end
    end
  end
end

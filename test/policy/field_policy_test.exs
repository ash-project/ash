defmodule Ash.Test.Policy.FieldPolicyTest do
  @doc false
  use ExUnit.Case

  require Ash.Query

  alias Ash.Test.Support.PolicyField.{Api, Ticket, User}

  setup do
    rep = Api.create!(Ash.Changeset.new(User, %{role: :representative}))
    user = Api.create!(Ash.Changeset.new(User, %{role: :user}))
    other_user = Api.create!(Ash.Changeset.new(User, %{role: :user}))

    [
      user: user,
      representative: rep,
      ticket:
        Api.create!(Ash.Changeset.new(Ticket, %{representative_id: rep.id, reporter_id: user.id})),
      other_ticket:
        Api.create!(
          Ash.Changeset.new(Ticket, %{representative_id: rep.id, reporter_id: other_user.id})
        )
    ]
  end

  describe "introspection" do
    test "introspection returns field policies" do
      assert [%Ash.Policy.FieldPolicy{}] = Ash.Policy.Info.field_policies(User)
    end
  end

  describe "rendering fields" do
    test "when reading as a user that can see the field, its value is displayed", %{
      representative: representative
    } do
      assert :representative ==
               User
               |> Ash.Query.for_read(:read, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Api.read_one!(authorize?: true, actor: representative)
               |> Map.get(:role)
    end

    test "when reading as a user that cant see the field, its value is not displayed", %{
      representative: representative,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :role, type: :attribute} ==
               User
               |> Ash.Query.for_read(:read, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Api.read_one!(authorize?: true, actor: user)
               |> Map.get(:role)
    end

    test "when loading as a user that cant see the field, its value is not displayed", %{
      representative: representative,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :role, type: :attribute} ==
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Api.read_one!(authorize?: true, actor: user)
               |> Api.load!(:role, authorize?: true)
               |> Map.get(:role)
    end

    test "when reading as a user that cant see the field with a `relates_to_actor_via` check, the value is not displayed",
         %{
           user: user,
           ticket: ticket,
           other_ticket: other_ticket
         } do
      Application.put_env(:foo, :bar, true)

      assert nil ==
               Ticket
               |> Ash.Query.select(:status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Api.read_one!()
               |> Map.get(:status)

      assert %Ash.ForbiddenField{} =
               Ticket
               |> Ash.Query.select(:status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^other_ticket.id)
               |> Api.read_one!()
               |> Map.get(:status)
    end
  end

  describe "filters" do
    test "filters are replaced with the appropriate field policies", %{
      representative: representative,
      user: user
    } do
      assert [] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter_input(role: :representative)
               |> Api.read!(authorize?: true)

      assert [_] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter_input(role: :representative)
               |> Api.read!(authorize?: true)
    end
  end
end

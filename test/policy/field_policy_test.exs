defmodule Ash.Test.Policy.FieldPolicyTest do
  @doc false
  use ExUnit.Case

  require Ash.Query

  alias Ash.Test.Support.PolicyField.{Api, Ticket, User}

  setup do
    rep = Api.create!(Ash.Changeset.new(User, %{role: :representative, points: 4}))
    user = Api.create!(Ash.Changeset.new(User, %{role: :user, points: 3}))
    admin = Api.create!(Ash.Changeset.new(User, %{role: :admin, points: 2}))
    other_user = Api.create!(Ash.Changeset.new(User, %{role: :user, points: 1}))

    [
      user: user,
      admin: admin,
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
      assert [
               %Ash.Policy.FieldPolicy{bypass?: true},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false}
             ] =
               Ash.Policy.Info.field_policies(User)
    end
  end

  describe "rendering fields" do
    test "when creating as a user that cannot see the field, its value is not displayed", %{
      representative: rep,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :internal_status, type: :attribute} ==
               Ticket
               |> Ash.Changeset.for_create(
                 :create,
                 %{representative_id: rep.id, reporter_id: user.id, internal_status: :new},
                 authorize?: true,
                 actor: user
               )
               |> Api.create!(authorize?: true, actor: user)
               |> Map.get(:internal_status)
    end

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

    test "bypasses take priority over subsequent policies", %{
      representative: representative,
      admin: admin
    } do
      assert :representative ==
               User
               |> Ash.Query.for_read(:read, authorize?: true, actor: admin)
               |> Ash.Query.filter(id == ^representative.id)
               |> Api.read_one!(authorize?: true, actor: admin)
               |> Map.get(:role)
    end

    test "can load a resource with a forbidden aggregate", %{
      representative: representative
    } do
      assert %Ash.ForbiddenField{field: :ticket_count, type: :aggregate} ==
               User
               |> Ash.Query.for_read(:read, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.Query.load([:ticket_count])
               |> Api.read_one!(authorize?: true, actor: representative)
               |> Map.get(:ticket_count)
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

    test "reading is allowed through a relationship",
         %{representative: representative} do
      # someone who is allowed because it's accessed through the ticket
      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Api.read!(authorize?: true)

      assert is_number(ticket.reporter.ticket_count)
      assert ticket.reporter.ticket_count > 0

      # can't read the value when reading the resource directly
      assert [user] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.load([:ticket_count])
               |> Ash.Query.limit(1)
               |> Api.read!(authorize?: true)

      assert user.ticket_count == %Ash.ForbiddenField{
               field: :ticket_count,
               type: :aggregate
             }
    end

    test "reading is allowed through a multi level relationship",
         %{user: user} do
      assert [ticket] =
               Ticket
               |> Ash.Query.select([:internal_status])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(reporter_id == ^user.id)
               |> Ash.Query.load(reporter: [:tickets])
               |> Api.read!(authorize?: true)

      assert ticket.internal_status == %Ash.ForbiddenField{
               field: :internal_status,
               type: :attribute
             }

      nested_ticket = ticket.reporter.tickets |> List.first()

      assert nested_ticket.internal_status == nil
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

    test "it's possible to filter on values that are only allowed to be accessed from a parent",
         %{
           representative: representative,
           user: user
         } do
      # someone who is allowed because it's accessed through the ticket
      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Api.read!(authorize?: true)

      assert is_number(ticket.reporter.ticket_count)
      assert ticket.reporter.ticket_count > 0

      # someone who isn't allowed to see the field
      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Api.read!(authorize?: true)

      assert ticket.reporter.ticket_count == %Ash.ForbiddenField{
               field: :ticket_count,
               type: :aggregate
             }
    end
  end

  describe "sorts" do
    test "sorts are replaced with the appropriate field policies", %{
      user: %{id: user_id} = user
    } do
      assert [%{id: ^user_id} | _] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort_input(points: :asc_nils_last)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Api.read!()

      assert [_, _, _, %{id: ^user_id}] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort_input(points: :asc_nils_first)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Api.read!()

      assert [_, _, %{id: ^user_id}, _] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort(points: :asc_nils_first)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Api.read!()
    end
  end
end

# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.AggregateFieldPolicyTest do
  @moduledoc """
  With `authorize_fields?: true`, field policies apply to the field an aggregate
  reads, regardless of how it is requested. Aggregates return raw values (not
  records that field-policy redaction runs on), so without this a `list`/`first`/
  `min`/`max`/`sum`/`avg` over a field-policy-protected field would leak it.
  """
  use ExUnit.Case

  alias Ash.Test.Support.PolicyField.{Ticket, User}

  setup do
    rep =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :representative, points: 4}),
        authorize?: false
      )

    reporter =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :user, points: 3}),
        authorize?: false
      )

    outsider =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :user, points: 1}),
        authorize?: false
      )

    admin =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :admin, points: 2}),
        authorize?: false
      )

    Ash.create!(
      Ash.Changeset.for_create(Ticket, :create, %{
        name: "t1",
        status: "SECRET-STATUS",
        representative_id: rep.id,
        reporter_id: reporter.id
      }),
      authorize?: false
    )

    [outsider: outsider, admin: admin]
  end

  describe "authorize_fields?: true" do
    test "outsider cannot read status via the normal fields path", %{outsider: outsider} do
      [ticket] = Ash.read!(Ticket, actor: outsider, authorize?: true)
      assert %Ash.ForbiddenField{field: :status} = ticket.status
    end

    for kind <- [:list, :first, :max, :min] do
      test "#{kind} aggregate does not leak status to an outsider", %{outsider: outsider} do
        {:ok, %{agg: value}} =
          Ash.aggregate(Ticket, {:agg, unquote(kind), field: :status},
            actor: outsider,
            authorize?: true,
            authorize_fields?: true
          )

        refute "SECRET-STATUS" in List.wrap(value),
               "leaked field-policy-protected status via #{unquote(kind)}: #{inspect(value)}"
      end
    end

    test "an admin (field-policy bypass) still sees the value", %{admin: admin} do
      {:ok, %{agg: value}} =
        Ash.aggregate(Ticket, {:agg, :list, field: :status},
          actor: admin,
          authorize?: true,
          authorize_fields?: true
        )

      assert "SECRET-STATUS" in List.wrap(value)
    end

    test "aggregating over a related aggregate a user cannot see is forbidden", %{
      outsider: outsider
    } do
      assert {:error, %Ash.Error.Forbidden{}} =
               Ash.aggregate(User, {:agg, :list, field: :ticket_count},
                 actor: outsider,
                 authorize?: true,
                 authorize_fields?: true
               )
    end

    test "an admin can aggregate over the related aggregate", %{admin: admin} do
      assert {:ok, %{agg: values}} =
               Ash.aggregate(User, {:agg, :list, field: :ticket_count},
                 actor: admin,
                 authorize?: true,
                 authorize_fields?: true
               )

      assert is_list(values)
    end
  end

  describe "default (opt-out)" do
    test "authorize?: true without authorize_fields? preserves existing behavior", %{
      outsider: outsider
    } do
      {:ok, %{agg: value}} =
        Ash.aggregate(Ticket, {:agg, :list, field: :status}, actor: outsider, authorize?: true)

      assert "SECRET-STATUS" in List.wrap(value)
    end

    test "authorize?: false is unaffected", %{outsider: outsider} do
      {:ok, %{agg: value}} =
        Ash.aggregate(Ticket, {:agg, :list, field: :status},
          actor: outsider,
          authorize?: false,
          authorize_fields?: true
        )

      assert "SECRET-STATUS" in List.wrap(value)
    end
  end
end

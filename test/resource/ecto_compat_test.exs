# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.EctoCompatTest do
  @moduledoc false
  use ExUnit.Case, async: true

  # Verifies that Ash-generated Ecto schemas properly register timestamp
  # attributes with Ecto's autogenerate system, so that Repo.insert/1 and
  # Repo.update/1 can auto-populate them without going through Ash's
  # changeset pipeline.
  #
  # See: https://github.com/ash-project/ash/issues/769

  describe "timestamp autogenerate registration" do
    test "create_timestamp fields appear in __schema__(:autogenerate_fields)" do
      defmodule CreateTimestampResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          create_timestamp :inserted_at
        end
      end

      autogen_fields = CreateTimestampResource.__schema__(:autogenerate_fields)
      assert :inserted_at in autogen_fields
    end

    test "update_timestamp fields appear in __schema__(:autogenerate_fields)" do
      defmodule UpdateTimestampResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          update_timestamp :updated_at
        end
      end

      autogen_fields = UpdateTimestampResource.__schema__(:autogenerate_fields)
      assert :updated_at in autogen_fields
    end

    test "both timestamps are registered together for autogenerate on insert" do
      defmodule BothTimestampsResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          create_timestamp :inserted_at
          update_timestamp :updated_at
        end
      end

      autogen_fields = BothTimestampsResource.__schema__(:autogenerate_fields)
      assert :inserted_at in autogen_fields
      assert :updated_at in autogen_fields
    end

    test "update_timestamp fields are registered in autoupdate" do
      defmodule AutoupdateResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          create_timestamp :inserted_at
          update_timestamp :updated_at
        end
      end

      # :autoupdate returns a list of {[fields], {mod, fun, args}} tuples.
      # update_timestamp fields should be registered here.
      autoupdate = AutoupdateResource.__schema__(:autoupdate)
      autoupdate_fields = Enum.flat_map(autoupdate, &elem(&1, 0))

      assert :updated_at in autoupdate_fields
      refute :inserted_at in autoupdate_fields
    end

    test "timestamps of the same type are grouped (match_other_defaults)" do
      defmodule GroupedTimestampsResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          create_timestamp :inserted_at
          update_timestamp :updated_at
        end
      end

      # Both timestamps default to Ash.Type.UtcDatetimeUsec, so they should
      # be grouped into a single {[fields], mfa} tuple for autogenerate.
      # This ensures Ecto calls the MFA once and assigns the same value to
      # both fields — matching Ash's match_other_defaults? behavior.
      autogenerate = GroupedTimestampsResource.__schema__(:autogenerate)

      grouped_entry =
        Enum.find(autogenerate, fn {fields, _mfa} ->
          :inserted_at in fields and :updated_at in fields
        end)

      assert grouped_entry != nil,
             "Expected inserted_at and updated_at to be grouped in a single autogenerate entry, " <>
               "got: #{inspect(autogenerate)}"
    end

    test "resource with no timestamps has empty autogenerate_fields" do
      defmodule NoTimestampResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :name, :string
        end
      end

      autogen_fields = NoTimestampResource.__schema__(:autogenerate_fields)
      assert autogen_fields == []
    end
  end
end

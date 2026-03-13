# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ValidateThroughRelationshipsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  alias Ash.Test.Domain, as: Domain

  defmodule ThroughTarget do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]
    end
  end

  defmodule ThroughIntermediate do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]
    end

    relationships do
      belongs_to :target, ThroughTarget, public?: true
    end
  end

  describe "valid through paths" do
    test "has_many with valid through path compiles without error" do
      defmodule ValidThroughSource do
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        actions do
          defaults [:read]
        end

        relationships do
          has_many :intermediates, ThroughIntermediate, destination_attribute: :target_id

          has_many :targets, ThroughTarget, through: [:intermediates, :target]
        end
      end
    end

    test "has_one with valid through path compiles without error" do
      defmodule ValidHasOneThroughSource do
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        actions do
          defaults [:read]
        end

        relationships do
          has_many :intermediates, ThroughIntermediate, destination_attribute: :target_id

          has_one :first_target, ThroughTarget, through: [:intermediates, :target]
        end
      end
    end
  end

  describe "invalid through paths" do
    test "emits error when through path references non-existent relationship" do
      output =
        capture_io(:stderr, fn ->
          defmodule BadPathSource do
            use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

            attributes do
              uuid_primary_key :id
            end

            actions do
              defaults [:read]
            end

            relationships do
              has_many :bad_through, ThroughTarget, through: [:nonexistent, :target]
            end
          end
        end)

      assert output =~ "relationship `nonexistent` does not exist"
    end

    test "emits error when intermediate relationship in through path does not exist" do
      output =
        capture_io(:stderr, fn ->
          defmodule BadIntermediatePathSource do
            use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

            attributes do
              uuid_primary_key :id
            end

            actions do
              defaults [:read]
            end

            relationships do
              has_many :intermediates, ThroughIntermediate, destination_attribute: :target_id

              has_many :bad_through, ThroughTarget, through: [:intermediates, :missing]
            end
          end
        end)

      assert output =~ "relationship `missing` does not exist"
      assert output =~ inspect(ThroughIntermediate)
    end

    test "emits error when through path final destination does not match declared destination" do
      defmodule WrongDestModule do
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        actions do
          defaults [:read]
        end
      end

      output =
        capture_io(:stderr, fn ->
          defmodule MismatchDestSource do
            use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

            attributes do
              uuid_primary_key :id
            end

            actions do
              defaults [:read]
            end

            relationships do
              has_many :intermediates, ThroughIntermediate, destination_attribute: :target_id

              # intermediates -> target resolves to ThroughTarget, not WrongDestModule
              has_many :wrong_dest, WrongDestModule, through: [:intermediates, :target]
            end
          end
        end)

      assert output =~ "declared destination"
      assert output =~ inspect(WrongDestModule)
    end
  end
end

# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.VerifyExtensionsAreNotMisplacedTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule PlainExtension do
    @moduledoc false
    use Spark.Dsl.Extension
  end

  test "raises error when an authorizer is added to extensions instead of authorizers" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.DataLayer.Ets,
            extensions: [Ash.Policy.Authorizer]

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    assert output =~
             "implements the Ash.Authorizer behaviour but was added to `extensions` instead of `authorizers`"
  end

  test "raises error when a notifier is added to extensions instead of notifiers" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.DataLayer.Ets,
            extensions: [Ash.Notifier.PubSub]

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    assert output =~
             "implements the Ash.Notifier behaviour but was added to `extensions` instead of `notifiers`"
  end

  test "does not raise when an authorizer is correctly added to authorizers" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.DataLayer.Ets,
            authorizers: [Ash.Policy.Authorizer]

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    refute output =~ "instead of `authorizers`"
  end

  test "does not raise when a notifier is correctly added to notifiers" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.DataLayer.Ets,
            notifiers: [Ash.Notifier.PubSub]

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    refute output =~ "instead of `notifiers`"
  end

  test "does not raise for extensions that do not implement authorizer or notifier" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.DataLayer.Ets,
            extensions: [PlainExtension]

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    refute output =~ "instead of"
  end
end

# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Transformers.RequireStringLengthCountConfigTest do
  @moduledoc false
  use ExUnit.Case, async: false

  setup do
    previous = Application.get_env(:ash, :default_string_length_count)
    Application.delete_env(:ash, :default_string_length_count)

    on_exit(fn ->
      Application.put_env(:ash, :default_string_length_count, previous)
    end)

    :ok
  end

  test "resources without string attributes compile when the config is not set" do
    defmodule NoStringAttributes do
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: :embedded

      attributes do
        attribute :value, :term, public?: true
        attribute :count, :integer, public?: true
      end
    end

    assert Ash.Resource.Info.attribute(NoStringAttributes, :value)
  end

  test "resources with a string attribute raise when the config is not set" do
    assert_raise Spark.Error.DslError,
                 ~r/`config :ash, :default_string_length_count` is not set/,
                 fn ->
                   defmodule HasStringAttribute do
                     use Ash.Resource, domain: Ash.Test.Domain, data_layer: :embedded

                     attributes do
                       attribute :name, :string, public?: true
                     end
                   end
                 end
  end

  test "resources with a ci_string attribute raise when the config is not set" do
    assert_raise Spark.Error.DslError,
                 ~r/`config :ash, :default_string_length_count` is not set/,
                 fn ->
                   defmodule HasCiStringAttribute do
                     use Ash.Resource, domain: Ash.Test.Domain, data_layer: :embedded

                     attributes do
                       attribute :name, :ci_string, public?: true
                     end
                   end
                 end
  end

  test "resources with an array of strings attribute raise when the config is not set" do
    assert_raise Spark.Error.DslError,
                 ~r/`config :ash, :default_string_length_count` is not set/,
                 fn ->
                   defmodule HasStringArrayAttribute do
                     use Ash.Resource, domain: Ash.Test.Domain, data_layer: :embedded

                     attributes do
                       attribute :tags, {:array, :string}, public?: true
                     end
                   end
                 end
  end
end

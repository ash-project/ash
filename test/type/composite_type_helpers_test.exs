# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.CompositeTypeHelpersTest do
  use ExUnit.Case, async: true

  alias Ash.Error.Changes.InvalidAttribute
  alias Ash.Type.CompositeTypeHelpers

  describe "convert_errors_to_invalid_attributes/1" do
    test "keeps the field of keyword errors listed next to exceptions" do
      exception = InvalidAttribute.exception(field: :nested, message: "rejected")

      assert {:error, [%InvalidAttribute{field: :count, message: "too small"}, ^exception]} =
               CompositeTypeHelpers.convert_errors_to_invalid_attributes([
                 [field: :count, message: "too small"],
                 exception
               ])
    end

    test "passes the path of nested errors on" do
      assert {:error, %InvalidAttribute{field: :inner, path: [:outer]}} =
               CompositeTypeHelpers.convert_errors_to_invalid_attributes([
                 [field: :inner, path: [:outer], message: "too small"]
               ])
    end
  end
end

# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ValidateRelationshipAttributesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Comment do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule ManualCommentRelationship do
    def load(_records, _opts, _context) do
      raise "Should not get here"
    end
  end

  defmodule Tag do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule MissingDestJoinResource do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :post_id, :uuid, primary_key?: true, allow_nil?: false
    end
  end

  test "relationship with missing destination_attribute raises error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule PostMissingDestAttribute do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          relationships do
            has_many :comments, Comment
          end
        end
      end)

    assert String.contains?(
             output,
             "Relationship `comments` expects destination field `post_missing_dest_attribute_id` to be defined on Ash.Test.Resource.ValidateRelationshipAttributesTest.Comment"
           )

    assert String.contains?(
             output,
             "relationships -> comments"
           )
  end

  test "manual relationship with mismatched source_attribute raises error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule PostMissingSourceAttributeManual do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :not_id
          end

          relationships do
            has_many :comments, Comment do
              manual ManualCommentRelationship
            end
          end
        end
      end)

    assert String.contains?(
             output,
             "Relationship `comments` expects source attribute `id` to be defined."
           )

    assert String.contains?(
             output,
             "relationships -> comments"
           )
  end

  test "many_to_many with missing source_attribute_on_join_resource raises error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule PostMissingJoinDestinationAttribute do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          relationships do
            many_to_many :tags, Tag do
              through MissingDestJoinResource
              source_attribute_on_join_resource :post_id
              destination_attribute_on_join_resource :tag_id
            end
          end
        end
      end)

    assert String.contains?(
             output,
             "Relationship `tags` expects destination attribute on join resource `tag_id` to be defined on Ash.Test.Resource.ValidateRelationshipAttributesTest.MissingDestJoinResource"
           )

    assert String.contains?(
             output,
             "relationships -> tags"
           )
  end

  test "relationship with no_attributes and missing destination_attribute does not raise error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule PostMissingDestAttributeNoAttributes do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          relationships do
            has_many :comments, Comment do
              no_attributes? true
            end
          end
        end
      end)

    assert not String.contains?(
             output,
             "Relationship `comments` expects destination field `post_missing_dest_attribute_no_attributes_id` to be defined on Ash.Test.Resource.ValidateRelationshipAttributesTest.Comment"
           )
  end

  test "relationship with validate_destination_attribute? false and missing destination_attribute does not raise error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule PostMissingDestAttributeNoValidateDestAttr do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          relationships do
            has_many :comments, Comment do
              validate_destination_attribute? false
            end
          end
        end
      end)

    assert not String.contains?(
             output,
             "Relationship `comments` expects destination field `post_missing_dest_attribute_no_validate_dest_attr_id` to be defined on Ash.Test.Resource.ValidateRelationshipAttributesTest.Comment"
           )
  end
end

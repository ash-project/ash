defmodule Ash.Test.ReactorUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
      attribute :sub_title, :string
      attribute :published, :boolean, default: false
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      update :undo_update_post do
        argument :changeset, :struct do
          constraints instance_of: Ash.Changeset
        end

        change fn changeset, _ ->
          original_changeset = Ash.Changeset.get_argument(changeset, :changeset)

          [:title, :sub_title, :published]
          |> Enum.reduce(changeset, fn key, changeset ->
            original_value = Ash.Changeset.get_data(original_changeset, key)
            Ash.Changeset.change_attribute(changeset, key, original_value)
          end)
        end
      end
    end

    code_interface do
      define_for Ash.Test.ReactorUpdateTest.Api
      define :create
      define :get, action: :read, get_by: :id
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Post
    end
  end

  test "it can update a post" do
    defmodule SimpleUpdatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_api Api
      end

      input :post

      update :publish_post, Post, :update do
        initial(input(:post))
        inputs(%{published: value(true)})
      end
    end

    {:ok, %{published: false} = original_post} =
      Post.create(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(SimpleUpdatePostReactor, %{post: original_post}, %{}, async?: false)

    assert post.published
  end

  test "it defaults to the primary action when the action is not supplied" do
    defmodule InferredActionNameUpdatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_api Api
      end

      input :post
      input :new_title

      update :update_post, Post do
        inputs(%{title: input(:new_title)})
        initial(input(:post))
      end
    end

    {:ok, original_post} =
      Post.create(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(
               InferredActionNameUpdatePostReactor,
               %{post: original_post, new_title: "New Title"},
               %{},
               async?: false
             )

    assert post.title == "New Title"
  end

  test "it merges multiple `inputs` entities together" do
    defmodule MergedInputsCreatePostReactor do
      @moduledoc false
      use Ash.Reactor, extensions: [Ash.Reactor]

      ash do
        default_api Api
      end

      input :post
      input :new_title
      input :new_sub_title

      update :update_post, Post, :update do
        initial(input(:post))
        inputs(%{title: input(:new_title)})
        inputs(%{sub_title: input(:new_sub_title)})
      end
    end

    {:ok, original_post} =
      Post.create(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(
               MergedInputsCreatePostReactor,
               %{post: original_post, new_title: "New Title", new_sub_title: "New Sub-title"},
               %{},
               async?: false
             )

    assert post.title == "New Title"
    assert post.sub_title == "New Sub-title"
  end

  test "it can undo the update on error" do
    defmodule UndoingUpdateReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_api Api
      end

      input :post
      input :new_title

      update :update_post, Post, :update do
        initial(input(:post))
        inputs(%{title: input(:new_title)})
        undo :always
        undo_action(:undo_update_post)
      end

      step :fail do
        wait_for :update_post

        run fn _, _ ->
          raise "hell"
        end
      end
    end

    {:ok, post} = Post.create(%{title: "Title"})

    assert {:error, _} =
             Reactor.run(
               UndoingUpdateReactor,
               %{
                 post: post,
                 new_title: "New title"
               },
               %{},
               async?: false
             )

    post_run_post = Post.get!(post.id)
    assert post_run_post.title == "Title"
  end
end

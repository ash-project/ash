defmodule Ash.Examples.Post do
  use Ash.Resource

  resource "posts" do
    actions do
      get true
    end

    attributes do
      attribute :contents, :string
    end

    # associations do
    #   belongs_to :author, Ash.Examples.Author

    #   has_many :comments, Ash.Examples.Comment
    # end
  end
end

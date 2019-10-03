defmodule Ash.Repo.Migrations.CreatePosts do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add(:contents, :text)
    end
  end
end

defmodule Ash.Repo.Migrations.CreatePokemon do
  use Ecto.Migration

  def change do
    create table(:pokemon) do
      add(:name, :text)
    end
  end
end

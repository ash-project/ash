# Translated Fields

## Introduction

Some websites may need to have database fields in multiple languages and only show certain fields pending a locale. We can create a calculation to handle this quite easily.

```elixir
defmodule App.Calculations.LocalizeName do
  use Ash.Resource.Calculation

  @impl true
  def init(opts), do: {:ok, opts}

  @impl true
  def load(_query, _opts, _context), do: []

  @impl true
  def expression(opts, context) do
    field = to_string(opts[:field])
    en = (field <> "_en") |> String.to_existing_atom()
    cn = (field <> "_zh_CN") |> String.to_existing_atom()
    hk = (field <> "_zh_HK") |> String.to_existing_atom()
    actor = Map.get(context, :actor)

    locale =
      case context do
        %{source_context: %{locale: locale}} when is_atom(locale) or is_binary(locale) ->
          locale

        _ ->
          nil
      end

    cond do
      not is_nil(actor) ->
        expr(
          cond do
            type(^actor(:locale), :string) == :zh_CN && not is_nil(^ref(cn)) ->
              ^ref(cn)

            type(^actor(:locale), :string) == :zh_HK && not is_nil(^ref(hk)) ->
              ^ref(hk)

            true ->
              ^ref(en)
          end
        )

      not is_nil(locale) ->
        expr(
          cond do
            type(^locale, :string) == :zh_CN && not is_nil(^ref(cn)) ->
              ^ref(cn)

            type(^locale, :string) == :zh_HK && not is_nil(^ref(hk)) ->
              ^ref(hk)

            true ->
              ^ref(en)
          end
        )

      true ->
        expr(^ref(en))
    end
  end
end

defmodule App.Tickets.Ticket do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    domain: App.Tickets.Domain

  postgres do
    repo App.Repo
    table "tickets"
  end

  attributes do
    uuid_primary_key :id
    attribute :name_en, :string, allow_nil?: false, public?: true
    attribute :name_zh_CN, :string, allow_nil?: true, public?: true
    attribute :name_zh_HK, :string, allow_nil?: true, public?: true
    timestamps()
  end

  calculations do
    calculate :name, :string, {App.Calculations.LocalizeName, [field: :name]}
  end

  actions do
    defaults [:create, :read, :update, :destroy]
    default_accept :*
  end
end
```

The above code will automatically choose the right name field based on the current locale passed in as an argument, or the current actor's locale that's specified. If none are passed in, then it will show the English name specified by the `name_en`, which is required.
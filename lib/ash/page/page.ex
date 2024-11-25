defmodule Ash.Page do
  @moduledoc "Types for Ash pages"
  @type page :: Ash.Page.Keyset.t() | Ash.Page.Offset.t()

  @type type :: :offset | :keyset

  @doc false
  # This is a custom validator for an options schema
  @spec page_opts(page() | false | nil | Keyword.t()) :: {:ok, page()} | {:error, String.t()}
  def page_opts(term) when term in [false, nil], do: {:ok, term}

  def page_opts(page_opts) do
    if page_opts[:after] || page_opts[:before] do
      validate_or_error(page_opts, Ash.Page.Keyset.Opts)
    else
      if page_opts[:offset] do
        validate_or_error(page_opts, Ash.Page.Offset.Opts)
      else
        validate_or_error(page_opts, Ash.Page.Keyset.Opts)
      end
    end
  end

  defp validate_or_error(opts, mod) do
    case mod.validate(opts) do
      {:ok, value} -> {:ok, mod.to_options(value)}
      {:error, error} -> {:error, Exception.message(error)}
    end
  end
end

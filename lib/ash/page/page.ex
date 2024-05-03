defmodule Ash.Page do
  @moduledoc "Types for Ash pages"
  @type page :: Ash.Page.Keyset.t() | Ash.Page.Offset.t()

  @type type :: :offset | :keyset

  @doc false
  # This is a custom validator for an options schema
  def page_opts(page_opts) do
    if page_opts in [false, nil] do
      {:ok, page_opts}
    else
      if page_opts[:after] || page_opts[:before] do
        validate_or_error(page_opts, Ash.Page.Keyset.page_opts())
      else
        if page_opts[:offset] do
          validate_or_error(page_opts, Ash.Page.Offset.page_opts())
        else
          validate_or_error(page_opts, Ash.Page.Keyset.page_opts())
        end
      end
    end
  end

  defp validate_or_error(opts, schema) do
    case Spark.Options.validate(opts, schema) do
      {:ok, value} -> {:ok, value}
      {:error, error} -> {:error, Exception.message(error)}
    end
  end
end

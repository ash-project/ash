defmodule Ash.Test.Resource.ApiTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        unquote(body)
      end
    end
  end

  defmacrop defapi(opts \\ [], do: body) do
    quote do
      defmodule Api do
        @moduledoc false
        use Ash.Api, unquote(opts)

        unquote(body)
      end
    end
  end
end

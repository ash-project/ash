defmodule Ash.Test.Resource.ApiTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post", primary_key: false

        unquote(body)
      end
    end
  end

  defmacrop defapi(opts \\ [], do: body) do
    quote do
      defmodule Api do
        use Ash.Api, unquote(opts)

        unquote(body)
      end
    end
  end
end

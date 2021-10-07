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
end

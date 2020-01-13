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

  describe "representation" do
    test "if `interface?: false` is set, interface functions are not defined" do
      defapi(interface?: false) do
      end

      interface_funcs = [
        get!: 3,
        get!: 3,
        read!: 2,
        read: 2,
        create!: 2,
        create: 2,
        update: 3,
        update!: 3,
        destroy: 3,
        destroy: 3
      ]

      for {func, arity} <- interface_funcs do
        refute :erlang.function_exported(Api, func, arity)
      end
    end
  end

  describe "validation" do
    test "it fails if `interface?` is not a boolean" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` interface? must be boolean",
        fn ->
          defapi(interface?: 10) do
          end
        end
      )
    end

    test "it fails if `max_page_size` is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` max_page_size must be integer",
        fn ->
          defapi(max_page_size: "ten") do
          end
        end
      )
    end

    test "it fails if `max_page_size` is zero" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` max_page_size failed constraint: must be greater than zero",
        fn ->
          defapi(max_page_size: 0) do
          end
        end
      )
    end

    test "it fails if `default_page_size` is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` default_page_size must be integer",
        fn ->
          defapi(default_page_size: "ten") do
          end
        end
      )
    end

    test "it fails if `default_page_size` is zero" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` default_page_size failed constraint: must be greater than zero",
        fn ->
          defapi(default_page_size: 0) do
          end
        end
      )
    end
  end
end

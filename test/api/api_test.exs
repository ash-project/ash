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
        "`use Ash.Test.Resource.ApiTest.Api, ...` interface? must be of type :boolean",
        fn ->
          defapi(interface?: 10) do
          end
        end
      )
    end

    test "it fails if `max_page_size` is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "`use Ash.Test.Resource.ApiTest.Api, ...` max_page_size must be of type :integer",
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
        "`use Ash.Test.Resource.ApiTest.Api, ...` default_page_size must be of type :integer",
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

    test "it fails if the parallel_side_load supervisor is not an atom" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option supervisor at parallel_side_load must be of type :atom",
        fn ->
          defapi do
            parallel_side_load(supervisor: "foo")
          end
        end
      )
    end

    test "it fails if the max_concurrency is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option max_concurrency at parallel_side_load must be of type :integer",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, max_concurrency: "foo")
          end
        end
      )
    end

    test "it fails if max_concurrency is zero" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option max_concurrency at parallel_side_load failed constraint: must be greater than zero",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, max_concurrency: 0)
          end
        end
      )
    end

    test "it fails if the timeout is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option timeout at parallel_side_load must be of type :integer",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, timeout: "foo")
          end
        end
      )
    end

    test "it fails if timeout is negative" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option timeout at parallel_side_load failed constraint: must be positive",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, timeout: -1)
          end
        end
      )
    end

    test "it fails if the shutdown is not an integer" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option shutdown at parallel_side_load must be of type :integer",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, shutdown: "foo")
          end
        end
      )
    end

    test "it fails if shutdown is negative" do
      assert_raise(
        Ash.Error.ApiDslError,
        "option shutdown at parallel_side_load failed constraint: must be positive",
        fn ->
          defapi do
            parallel_side_load(supervisor: :foo, shutdown: -1)
          end
        end
      )
    end
  end
end

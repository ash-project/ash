defmodule Ash.Test.ErrorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule TestError do
    use Ash.Error.Exception
    def_ash_error([:some_field])
  end

  defmodule TestResource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  describe "to_error_class" do
    test "returns exception if it is a map/struct with class: :special" do
      assert Ash.Error.to_error_class(%{class: :special}, []) == %{class: :special}
    end

    test "returns exception if it is a map/struct with class: :special wrapped in a list" do
      assert Ash.Error.to_error_class([%{class: :special}], []) == %{class: :special}
    end

    test "returns exception if it is a map/struct with class: :special wrapped in an Invalid error" do
      err = Ash.Error.Invalid.exception(errors: [%{class: :special}])

      assert Ash.Error.to_error_class(err, []) == %{class: :special}
    end

    test "returns chosen error if the value argument is a list of values" do
      values = ["foo", "bar"]

      result = Ash.Error.to_error_class(values, [])

      assert match?(%Ash.Error.Unknown{}, result)

      # given the test arrangement, each error in the error class should
      # * be an Ash.Error.Unknown.UnknownError
      # * have a .class == :unknown
      # * have a .error that is a distinct element of the uniq subset of the values provided

      assert same_elements?(Enum.map(result.errors, fn err -> err.error end), values)

      for err <- result.errors do
        assert match?(%Ash.Error.Unknown.UnknownError{}, err)
        assert err.class == :unknown
      end
    end

    test "returns chosen error if the value argument is a list of errors" do
      err1 = Ash.Error.Unknown.UnknownError.exception(error: :an_error)
      err2 = Ash.Error.Invalid.exception(errors: [:more, :errors])

      result = Ash.Error.to_error_class([err1, err2], [])

      # the parent error will be of the invalid class because it take precedence over unknown
      assert match?(%Ash.Error.Invalid{}, result)

      # the parent error's errors field gets prepended to the list of other errors
      assert same_elements?(result.errors, [:more, :errors, err1])
    end

    test "has a context field populated when there is a single error" do
      test_error = TestError.exception([])

      err = Ash.Error.to_error_class(test_error, error_context: "some context")

      assert err.error_context == ["some context"]
    end

    test "has a context field populated when there is a list of errors" do
      test_error1 = TestError.exception(some_field: :a)
      test_error2 = TestError.exception(some_field: :b)

      err = Ash.Error.to_error_class([test_error1, test_error2], error_context: "some context")

      assert err.error_context == ["some context"]
    end

    test "accumulates error_context field in child errors" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
      error2 = Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")

      error_class =
        Ash.Error.to_error_class([error1, error2], error_context: "some higher context")

      child_error_1 = Enum.find(error_class.errors, fn err -> err.error == "whoops!" end)
      assert child_error_1.error_context == ["some higher context", "some context"]

      child_error_2 = Enum.find(error_class.errors, fn err -> err.error == "whoops, again!!" end)
      assert child_error_2.error_context == ["some higher context", "some other context"]
    end

    test "accumulates error_context field in child errors who have no error_context of their own" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
      error2 = Ash.Error.to_ash_error("whoops, again!!", nil)

      error_class =
        Ash.Error.to_error_class([error1, error2], error_context: "some higher context")

      child_error_1 = Enum.find(error_class.errors, fn err -> err.error == "whoops!" end)
      assert child_error_1.error_context == ["some higher context", "some context"]

      child_error_2 = Enum.find(error_class.errors, fn err -> err.error == "whoops, again!!" end)
      assert child_error_2.error_context == ["some higher context"]
    end

    test "leaves child error contexts unchanged if no error_context field provided" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
      error2 = Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")

      error_class = Ash.Error.to_error_class([error1, error2])

      child_error_1 = Enum.find(error_class.errors, fn err -> err.error == "whoops!" end)
      assert child_error_1.error_context == ["some context"]

      child_error_2 = Enum.find(error_class.errors, fn err -> err.error == "whoops, again!!" end)
      assert child_error_2.error_context == ["some other context"]
    end

    test "error message contains error context breadcrumbs" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
      error2 = Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")

      error_class =
        Ash.Error.to_error_class([error1, error2], error_context: "some higher context")

      error_message = Ash.Error.Unknown.message(error_class)

      assert error_message =~ "Context: some higher context > some context"
      assert error_message =~ "Context: some higher context > some other context"
    end

    test "error message still renders when there's no error context" do
      error1 = Ash.Error.to_ash_error("whoops!")
      error2 = Ash.Error.to_ash_error("whoops, again!!")

      error_class = Ash.Error.to_error_class([error1, error2])

      error_message = Ash.Error.Unknown.message(error_class)

      assert error_message =~ "Unknown Error\n\n* whoops!"
    end

    test "has a context field populated in changeset" do
      test_error = TestError.exception([])

      cs = Ash.Changeset.for_create(TestResource, :create)

      err = Ash.Error.to_error_class(test_error, changeset: cs, error_context: "some context")

      assert err.error_context == ["some context"]

      [cs_error] = err.changeset.errors
      assert cs_error.error_context == ["some context"]
    end

    test "a changeset can be passed in directly" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")

      error2 =
        Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")

      cs = Ash.Changeset.for_create(TestResource, :create) |> Map.put(:errors, [error1, error2])

      Ash.Test.assert_has_error(cs, Ash.Error.Unknown, fn err ->
        err.error == "whoops!"
      end)

      Ash.Test.refute_has_error(cs, Ash.Error.Unknown, fn err ->
        err.error == "yay!"
      end)

      assert clean(Ash.Error.to_error_class(cs)) ==
               clean(Ash.Error.to_error_class([error1, error2], changeset: cs))
    end

    test "accumulates error_context field in changeset's copy of error hierarchy" do
      error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
      error2 = Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")

      cs = Ash.Changeset.for_create(TestResource, :create)

      error_class =
        Ash.Error.to_error_class([error1, error2],
          changeset: cs,
          error_context: "some higher context"
        )

      cs_child_error_1 =
        Enum.find(error_class.changeset.errors, fn err -> err.error == "whoops!" end)

      cs_child_error_2 =
        Enum.find(error_class.changeset.errors, fn err -> err.error == "whoops, again!!" end)

      assert cs_child_error_1.error_context == ["some higher context", "some context"]
      assert cs_child_error_2.error_context == ["some higher context", "some other context"]
    end
  end

  describe "to_ash_error" do
    test "populates error_context field" do
      error = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")

      assert error.error_context == ["some context"]
    end
  end

  defp same_elements?(xs, ys) when is_list(xs) and is_list(ys) do
    Enum.sort(clean(xs)) == Enum.sort(clean(ys))
  end

  defp same_elements?(_, _), do: false

  defp clean(list) when is_list(list), do: Enum.map(list, &clean/1)

  defp clean(%{stacktrace: _} = value) do
    %{value | stacktrace: nil}
  end

  defp clean(other), do: other
end

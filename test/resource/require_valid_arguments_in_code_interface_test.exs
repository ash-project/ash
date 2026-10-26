# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Resource.RequireValidArgumentsInCodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "fails if one of the code_interface arguments is not a valid attribute or argument" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define :read, args: [:oops, :bar]
          end

          attributes do
            attribute :foo, :string
          end

          actions do
            read :read do
              argument :bar, :string
            end
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":oops")
  end

  test "fails if one of the arguments is not an accepted attribute" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define :update, args: [:foo, :bar]
          end

          attributes do
            attribute :foo, :string
          end

          actions do
            update :update do
              argument :bar, :string
              # Do not accept :foo
              accept []
            end
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the attributes") and
             String.contains?(output, ":foo")
  end

  test "passes if the arguments are valid" do
    defposts do
      code_interface do
        define :read, args: [:bar]
        define :update, args: [:foo, :bar]
      end

      attributes do
        attribute :foo, :string
      end

      actions do
        read :read do
          argument :bar, :string
        end

        update :update do
          argument :bar, :string
          accept [:foo]
        end
      end
    end
  end

  test "define_calculation fails if an arg is not a calculation argument or a field" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define_calculation :full_name, args: [:_record, :product_variant_id]
          end

          calculations do
            calculate :full_name, :string, expr("x")
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":product_variant_id")
  end

  test "define_calculation fails if an {:arg, _} is not a calculation argument" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define_calculation :display, args: [{:arg, :foo}]
          end

          attributes do
            attribute :foo, :string
          end

          calculations do
            calculate :display, :string, expr(foo)
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":foo")
  end

  test "define_calculation fails if a {:ref, _} is not a field on the resource" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define_calculation :display, args: [{:ref, :bar}]
          end

          attributes do
            attribute :foo, :string
          end

          calculations do
            calculate :display, :string, expr(foo <> ^arg(:bar)) do
              argument :bar, :string
            end
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":bar")
  end

  test "define_calculation passes if the args are valid" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          code_interface do
            define_calculation :display_record, calculation: :display, args: [:_record, :bar]
            define_calculation :display_bare, calculation: :display, args: [:foo, :bar]

            define_calculation :display_tagged,
              calculation: :display,
              args: [{:ref, :foo}, {:arg, :bar}]

            define_calculation :display_optional,
              calculation: :display,
              args: [:foo, {:optional, :bar}]

            define_calculation :display_optional_default,
              calculation: :display,
              args: [:foo, {:optional, :bar, "!"}]
          end

          attributes do
            attribute :foo, :string
          end

          calculations do
            calculate :display, :string, expr(foo <> ^arg(:bar)) do
              argument :bar, :string, default: ""
            end
          end
        end
      end)

    refute String.contains?(output, "Cannot accept")
  end

  test "define_calculation fails if a domain calculation interface arg is invalid" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule CalcInterfacePost do
          @moduledoc false
          use Ash.Resource,
            domain: Resource.RequireValidArgumentsInCodeInterfaceTest.CalcInterfaceDomain,
            data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
            attribute :foo, :string
          end

          calculations do
            calculate :display, :string, expr(foo)
          end
        end

        defmodule CalcInterfaceDomain do
          @moduledoc false
          use Ash.Domain, validate_config_inclusion?: false

          resources do
            resource Resource.RequireValidArgumentsInCodeInterfaceTest.CalcInterfacePost do
              define_calculation :display, args: [:oops]
            end
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":oops")
  end
end

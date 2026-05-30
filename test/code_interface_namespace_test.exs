# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.CodeInterfaceNamespaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ResourceNamespacedUser do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept [:name]
      defaults [:read, :create, :update, :destroy]
    end

    code_interface do
      namespace Iface

      define :create_user, action: :create
      define :read_users, action: :read
      define :read_alt, action: :read, namespace: Alt
    end
  end

  defmodule DomainNamespacedUser do
    @moduledoc false
    use Ash.Resource, domain: __MODULE__.Holder, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept [:name]
      defaults [:read, :create, :update, :destroy]
    end

    code_interface do
      define? false
    end
  end

  defmodule HostDomain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      resource Ash.Test.CodeInterfaceNamespaceTest.DomainNamespacedUser do
        namespace Iface
        define :create_user, action: :create
        define :read_users, action: :read
        define :read_alt, action: :read, namespace: Alt
      end
    end
  end

  describe "resource-side namespace splitting" do
    test "block-level namespace generates on host.Namespace" do
      target = Module.concat(ResourceNamespacedUser, Iface)
      assert function_exported?(target, :create_user, 2)
      assert function_exported?(target, :read_users, 0)
      refute function_exported?(ResourceNamespacedUser, :create_user, 2)
      refute function_exported?(ResourceNamespacedUser, :read_users, 0)
    end

    test "per-define namespace overrides the block default" do
      alt = Module.concat(ResourceNamespacedUser, Alt)
      assert function_exported?(alt, :read_alt, 0)
      refute function_exported?(Module.concat(ResourceNamespacedUser, Iface), :read_alt, 0)
    end
  end

  describe "domain-side namespace splitting" do
    test "block-level namespace on `resource` generates on Domain.Namespace" do
      target = Module.concat(HostDomain, Iface)
      assert function_exported?(target, :create_user, 2)
      assert function_exported?(target, :read_users, 0)
      refute function_exported?(HostDomain, :create_user, 2)
      refute function_exported?(HostDomain, :read_users, 0)
    end

    test "per-define namespace overrides block default" do
      assert function_exported?(Module.concat(HostDomain, Alt), :read_alt, 0)
      refute function_exported?(Module.concat(HostDomain, Iface), :read_alt, 0)
    end
  end

  defmodule WillCollide.Squatter do
    @moduledoc false
    def hi, do: :hi
  end

  describe "collision detection" do
    test "raises when target module already exists" do
      assert_raise RuntimeError, ~r/already defined/, fn ->
        defmodule WillCollide do
          @moduledoc false
          use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

          ets do
            private?(true)
          end

          attributes do
            uuid_primary_key :id
          end

          actions do
            defaults [:read]
          end

          code_interface do
            define :read_them, action: :read, namespace: Squatter
          end
        end
      end
    end
  end
end

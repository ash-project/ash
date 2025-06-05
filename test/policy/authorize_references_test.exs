defmodule Ash.Test.Policy.AuthorizeReferencesTest do
  @moduledoc false
  use ExUnit.Case
  require Ash.Query

  # Module-based calculations
  defmodule UserSecretCodeCalculation do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _) do
      [secret_info: [:secret_code]]
    end

    @impl true
    def calculate(records, _, _) do
      Enum.map(records, fn record ->
        case record.secret_info do
          %Ash.ForbiddenField{} ->
            nil

          %Ash.NotLoaded{} ->
            nil

          nil ->
            nil

          secret_info ->
            secret_info.secret_code
        end
      end)
    end
  end

  defmodule UserSecretCodeSafeCalculation do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _) do
      [secret_info: [:secret_code]]
    end

    @impl true
    def calculate(records, _, _context) do
      # Check if calculation has authorize_references? set
      # We'll need to access this from the calculation definition
      # For now, hardcode since this is the "safe" version
      authorize_references? = true

      Enum.map(records, fn record ->
        case record.secret_info do
          %Ash.ForbiddenField{} ->
            if authorize_references? do
              nil
            else
              # This shouldn't happen in practice, but for completeness
              raise "Forbidden field encountered"
            end

          # Handle case where relationship isn't loaded
          %Ash.NotLoaded{} ->
            nil

          nil ->
            nil

          secret_info ->
            secret_info.secret_code
        end
      end)
    end
  end

  defmodule ProfileSummaryCalculation do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _) do
      [:bio, :user, secret_info: [:secret_code]]
    end

    @impl true
    def calculate(records, _, _context) do
      # Hardcode for now since this is the "safe" version
      authorize_references? = true

      Enum.map(records, fn record ->
        user_name =
          case record.user do
            %Ash.ForbiddenField{} -> nil
            %Ash.NotLoaded{} -> nil
            nil -> nil
            user -> user.first_name
          end

        secret_code =
          case record.secret_info do
            %Ash.ForbiddenField{} ->
              if authorize_references?, do: nil, else: raise("Forbidden field")

            # Handle case where relationship isn't loaded
            %Ash.NotLoaded{} ->
              nil

            nil ->
              nil

            secret_info ->
              secret_info.secret_code
          end

        if authorize_references? && (is_nil(user_name) || is_nil(secret_code)) do
          nil
        else
          "Bio: #{record.bio} | User: #{user_name} | Secret: #{secret_code || ""}"
        end
      end)
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      policy always() do
        authorize_if always()
      end
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:first_name, :string, public?: true)
      attribute(:last_name, :string, public?: true)
      attribute(:is_admin, :boolean, default: false, public?: true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  defmodule SecretInfo do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      # Only admins can see secret info
      policy action_type(:read) do
        authorize_if actor_attribute_equals(:is_admin, true)
      end
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:secret_code, :string, public?: true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :user, User, public?: true
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      policy always() do
        authorize_if always()
      end
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:bio, :string, public?: true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :user, User, public?: true

      has_one :secret_info, SecretInfo,
        destination_attribute: :user_id,
        source_attribute: :user_id,
        public?: true
    end

    calculations do
      # Calculation that references user fields (should always work)
      calculate :user_full_name, :string, expr(user.first_name <> " " <> user.last_name) do
        public?(true)
      end

      # Calculation that references secret info WITHOUT authorize_references
      calculate :user_secret_code, :string, expr(secret_info.secret_code) do
        public?(true)
      end

      # Calculation that references secret info WITH authorize_references
      calculate :user_secret_code_safe, :string, expr(secret_info.secret_code) do
        public?(true)
        authorize_references?(true)
      end

      # More complex calculation with nested references
      calculate :profile_summary,
                :string,
                expr(
                  "Bio: " <>
                    bio <>
                    " | User: " <> user.first_name <> " | Secret: " <> secret_info.secret_code
                ) do
        public?(true)
        authorize_references?(true)
      end

      # Module-based calculations (equivalent to expression ones)
      # Calculation that references secret info WITHOUT authorize_references (module)
      calculate :user_secret_code_module, :string, UserSecretCodeCalculation do
        public?(true)
      end

      # Calculation that references secret info WITH authorize_references (module)
      calculate :user_secret_code_safe_module, :string, UserSecretCodeSafeCalculation do
        public?(true)
        authorize_references?(true)
      end

      # More complex calculation with nested references (module)
      calculate :profile_summary_module, :string, ProfileSummaryCalculation do
        public?(true)
        authorize_references?(true)
      end
    end
  end

  defmodule UserWithFieldPolicies do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :sensitive_field do
        authorize_if actor_attribute_equals(:is_admin, true)
      end

      field_policy :admin_only_field do
        authorize_if actor_attribute_equals(:is_admin, true)
      end

      field_policy [:name, :email, :is_admin] do
        authorize_if always()
      end
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, public?: true)
      attribute(:email, :string, public?: true)
      attribute(:sensitive_field, :string, public?: true)
      attribute(:admin_only_field, :string, public?: true)
      attribute(:is_admin, :boolean, default: false, public?: true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      has_one :field_policy_profile, FieldPolicyProfile,
        destination_attribute: :user_id,
        public?: true
    end
  end

  defmodule FieldPolicyProfile do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      policy always() do
        authorize_if always()
      end
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:bio, :string, public?: true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :user, UserWithFieldPolicies, public?: true
    end

    calculations do
      # Calculation referencing field with field policy WITHOUT authorize_references
      calculate :user_sensitive_info, :string, expr(user.sensitive_field) do
        public?(true)
      end

      # Calculation referencing field with field policy WITH authorize_references
      calculate :user_sensitive_info_safe, :string, expr(user.sensitive_field) do
        public?(true)
        authorize_references?(true)
      end

      # Calculation referencing admin-only field WITH authorize_references
      calculate :user_admin_info_safe, :string, expr(user.admin_only_field) do
        public?(true)
        authorize_references?(true)
      end

      # Complex calculation mixing allowed and restricted fields WITH authorize_references
      calculate :user_summary_safe,
                :string,
                expr(
                  "Name: " <>
                    user.name <>
                    " | Sensitive: " <>
                    user.sensitive_field <> " | Admin: " <> user.admin_only_field
                ) do
        public?(true)
        authorize_references?(true)
      end

      # Calculation referencing multiple fields, some with field policies
      calculate :user_contact_info, :string, expr(user.name <> " - " <> user.email) do
        public?(true)
        authorize_references?(true)
      end
    end
  end

  setup do
    # Create an admin user
    admin_user =
      User
      |> Ash.Changeset.for_create(:create, %{
        first_name: "Admin",
        last_name: "User",
        is_admin: true
      })
      |> Ash.create!(authorize?: false)

    # Create a regular user
    regular_user =
      User
      |> Ash.Changeset.for_create(:create, %{
        first_name: "Regular",
        last_name: "User",
        is_admin: false
      })
      |> Ash.create!(authorize?: false)

    # Create secret info for both users
    admin_secret =
      SecretInfo
      |> Ash.Changeset.for_create(:create, %{
        user_id: admin_user.id,
        secret_code: "ADMIN123"
      })
      |> Ash.create!(authorize?: false)

    regular_secret =
      SecretInfo
      |> Ash.Changeset.for_create(:create, %{
        user_id: regular_user.id,
        secret_code: "REGULAR456"
      })
      |> Ash.create!(authorize?: false)

    # Create profiles for both users
    admin_profile =
      Profile
      |> Ash.Changeset.for_create(:create, %{
        user_id: admin_user.id,
        bio: "I am an admin"
      })
      |> Ash.create!(authorize?: false)

    regular_profile =
      Profile
      |> Ash.Changeset.for_create(:create, %{
        user_id: regular_user.id,
        bio: "I am a regular user"
      })
      |> Ash.create!(authorize?: false)

    # Create users with field policies
    admin_user_fp =
      UserWithFieldPolicies
      |> Ash.Changeset.for_create(:create, %{
        name: "Admin User",
        email: "admin@example.com",
        sensitive_field: "admin_sensitive_data",
        admin_only_field: "admin_only_data",
        is_admin: true
      })
      |> Ash.create!(authorize?: false)

    regular_user_fp =
      UserWithFieldPolicies
      |> Ash.Changeset.for_create(:create, %{
        name: "Regular User",
        email: "regular@example.com",
        sensitive_field: "regular_sensitive_data",
        admin_only_field: "regular_admin_data",
        is_admin: false
      })
      |> Ash.create!(authorize?: false)

    # Create profiles for field policy users
    admin_fp_profile =
      FieldPolicyProfile
      |> Ash.Changeset.for_create(:create, %{
        user_id: admin_user_fp.id,
        bio: "Admin with field policies"
      })
      |> Ash.create!(authorize?: false)

    regular_fp_profile =
      FieldPolicyProfile
      |> Ash.Changeset.for_create(:create, %{
        user_id: regular_user_fp.id,
        bio: "Regular user with field policies"
      })
      |> Ash.create!(authorize?: false)

    %{
      admin_user: admin_user,
      regular_user: regular_user,
      admin_secret: admin_secret,
      regular_secret: regular_secret,
      admin_profile: admin_profile,
      regular_profile: regular_profile,
      admin_user_fp: admin_user_fp,
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile,
      regular_fp_profile: regular_fp_profile
    }
  end

  describe "calculations with authorize_references?: false (default)" do
    test "calculation referencing unauthorized data returns the value", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # When a regular user tries to load a calculation that references secret info
      # without authorize_references, it should return the actual value (no authorization check)
      result =
        admin_profile
        |> Ash.load!(:user_secret_code, actor: regular_user, authorize?: true)

      assert result.user_secret_code == "ADMIN123"
    end

    test "calculation referencing authorized data works", %{
      admin_user: admin_user,
      admin_profile: admin_profile
    } do
      # Admin can see their own secret info
      result =
        admin_profile
        |> Ash.load!(:user_secret_code, actor: admin_user, authorize?: true)

      assert result.user_secret_code == "ADMIN123"
    end
  end

  describe "calculations with authorize_references?: true" do
    test "calculation returns nil when referencing unauthorized data", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # When a regular user tries to load a calculation that references secret info
      # with authorize_references, it should return nil instead of raising
      result =
        admin_profile
        |> Ash.load!(:user_secret_code_safe, actor: regular_user, authorize?: true)

      assert is_nil(result.user_secret_code_safe)
    end

    test "calculation returns value when referencing authorized data", %{
      admin_user: admin_user,
      admin_profile: admin_profile
    } do
      # Admin can see their own secret info
      result =
        admin_profile
        |> Ash.load!(:user_secret_code_safe, actor: admin_user, authorize?: true)

      assert result.user_secret_code_safe == "ADMIN123"
    end

    test "complex calculation with mixed authorized/unauthorized references", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # Regular user can see user fields but not secret info
      result =
        admin_profile
        |> Ash.load!(:profile_summary, actor: regular_user, authorize?: true)

      # The secret part should be nil, but other parts should work
      # This behavior might need refinement - should the whole expression be nil
      # or should unauthorized parts be replaced with nil?
      assert result.profile_summary == "Bio: I am an admin | User: Admin | Secret: "
    end

    test "calculation with only authorized references works normally", %{
      regular_user: regular_user,
      regular_profile: regular_profile
    } do
      # User full name doesn't reference any restricted data
      result =
        regular_profile
        |> Ash.load!(:user_full_name, actor: regular_user, authorize?: true)

      assert result.user_full_name == "Regular User"
    end
  end

  describe "reading multiple records with authorize_references?" do
    test "calculations respect authorize_references when loading multiple records", %{
      regular_user: regular_user,
      admin_profile: admin_profile,
      regular_profile: regular_profile
    } do
      # Load multiple profiles with calculations
      profiles =
        Profile
        |> Ash.Query.load([:user_secret_code_safe, :user_full_name])
        |> Ash.read!(actor: regular_user, authorize?: true)

      admin_result = Enum.find(profiles, &(&1.id == admin_profile.id))
      regular_result = Enum.find(profiles, &(&1.id == regular_profile.id))

      # Admin's secret should be nil for regular user
      assert is_nil(admin_result.user_secret_code_safe)
      assert admin_result.user_full_name == "Admin User"

      # Regular user's own secret should also be nil (they can't see SecretInfo)
      assert is_nil(regular_result.user_secret_code_safe)
      assert regular_result.user_full_name == "Regular User"
    end
  end

  describe "module-based calculations with authorize_references?: false (default)" do
    test "calculation referencing unauthorized data returns the value", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # When a regular user tries to load a module calculation that references secret info
      # without authorize_references, it should return the actual value (no authorization check)
      Logger.configure(level: :debug)

      result =
        admin_profile
        |> Ash.load!(:user_secret_code_module, actor: regular_user, authorize?: true)

      assert result.user_secret_code_module == "ADMIN123"
    end

    test "calculation referencing authorized data works", %{
      admin_user: admin_user,
      admin_profile: admin_profile
    } do
      # Admin can see their own secret info
      result =
        admin_profile
        |> Ash.load!(:user_secret_code_module, actor: admin_user, authorize?: true)

      assert result.user_secret_code_module == "ADMIN123"
    end
  end

  describe "module-based calculations with authorize_references?: true" do
    test "calculation returns nil when referencing unauthorized data", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # When a regular user tries to load a module calculation that references secret info
      # with authorize_references, it should return nil instead of raising
      result =
        admin_profile
        |> Ash.load!(:user_secret_code_safe_module, actor: regular_user, authorize?: true)

      assert is_nil(result.user_secret_code_safe_module)
    end

    test "calculation returns value when referencing authorized data", %{
      admin_user: admin_user,
      admin_profile: admin_profile
    } do
      # Admin can see their own secret info
      result =
        admin_profile
        |> Ash.load!(:user_secret_code_safe_module, actor: admin_user, authorize?: true)

      assert result.user_secret_code_safe_module == "ADMIN123"
    end

    test "complex calculation with mixed authorized/unauthorized references", %{
      regular_user: regular_user,
      admin_profile: admin_profile
    } do
      # Regular user can see user fields but not secret info
      result =
        admin_profile
        |> Ash.load!(:profile_summary_module, actor: regular_user, authorize?: true)

      # When there are forbidden references, the whole calculation should return nil
      assert is_nil(result.profile_summary_module)
    end

    test "calculation with only authorized references works normally", %{
      regular_user: regular_user,
      regular_profile: regular_profile
    } do
      # User full name doesn't reference any restricted data
      result =
        regular_profile
        |> Ash.load!(:user_full_name, actor: regular_user, authorize?: true)

      assert result.user_full_name == "Regular User"
    end
  end

  describe "reading multiple records with authorize_references? (module-based)" do
    test "calculations respect authorize_references when loading multiple records", %{
      regular_user: regular_user,
      admin_profile: admin_profile,
      regular_profile: regular_profile
    } do
      # Load multiple profiles with module calculations
      profiles =
        Profile
        |> Ash.Query.load([:user_secret_code_safe_module, :user_full_name])
        |> Ash.read!(actor: regular_user, authorize?: true)

      admin_result = Enum.find(profiles, &(&1.id == admin_profile.id))
      regular_result = Enum.find(profiles, &(&1.id == regular_profile.id))

      # Admin's secret should be nil for regular user
      assert is_nil(admin_result.user_secret_code_safe_module)
      assert admin_result.user_full_name == "Admin User"

      # Regular user's own secret should also be nil (they can't see SecretInfo)
      assert is_nil(regular_result.user_secret_code_safe_module)
      assert regular_result.user_full_name == "Regular User"
    end
  end

  describe "filtering with expression calculations referencing unauthorized data" do
    test "filter with expression calculation works when authorize? false", %{
      admin_profile: admin_profile
    } do
      # Without authorization, filter should work normally
      profiles =
        Profile
        |> Ash.Query.filter(user_secret_code_safe == "ADMIN123")
        |> Ash.read!(authorize?: false)

      assert length(profiles) == 1
      assert hd(profiles).id == admin_profile.id
    end

    test "filter with expression calculation treats value as nil when authorize?: true & authorize_references?: true",
         %{regular_user: regular_user} do
      # With authorization and authorize_references, the expression should be treated as nil
      # so no records should match the filter
      profiles =
        Profile
        |> Ash.Query.filter(user_secret_code_safe == "ADMIN123")
        |> Ash.read!(actor: regular_user, authorize?: true)

      # Should return no results because user_secret_code_safe is treated as nil
      assert profiles == []
    end

    test "filter with expression calculation for nil works when authorize?: true & authorize_references?: true",
         %{
           regular_user: regular_user,
           admin_profile: admin_profile,
           regular_profile: regular_profile
         } do
      # When filtering for nil, should match all records since unauthorized refs become nil
      profiles =
        Profile
        |> Ash.Query.filter(is_nil(user_secret_code_safe))
        |> Ash.read!(actor: regular_user, authorize?: true)

      # Should return both profiles since user_secret_code_safe is treated as nil for both
      assert length(profiles) == 2
      profile_ids = Enum.map(profiles, & &1.id) |> Enum.sort()
      expected_ids = [admin_profile.id, regular_profile.id] |> Enum.sort()
      assert profile_ids == expected_ids
    end
  end

  describe "field policies with authorize_references?: false (default)" do
    @tag :field_policies
    test "calculation referencing field with field policy returns the value", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # When a regular user tries to load a calculation that references a field with field policy
      # without authorize_references, it should return the actual value (no field policy check)
      result =
        admin_fp_profile
        |> Ash.load!(:user_sensitive_info, actor: regular_user_fp, authorize?: true)

      assert result.user_sensitive_info == "admin_sensitive_data"
    end

    @tag :field_policies
    test "calculation referencing allowed field works", %{
      admin_user_fp: admin_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Admin can see sensitive fields
      result =
        admin_fp_profile
        |> Ash.load!(:user_sensitive_info, actor: admin_user_fp, authorize?: true)

      assert result.user_sensitive_info == "admin_sensitive_data"
    end
  end

  describe "field policies with authorize_references?: true" do
    @tag :field_policies
    test "calculation returns nil when referencing field denied by field policy", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # When a regular user tries to load a calculation that references a field with field policy
      # with authorize_references, it should return nil if field policy denies access
      result =
        admin_fp_profile
        |> Ash.load!(:user_sensitive_info_safe, actor: regular_user_fp, authorize?: true)

      assert is_nil(result.user_sensitive_info_safe)
    end

    @tag :field_policies
    test "calculation returns value when field policy allows access", %{
      admin_user_fp: admin_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Admin can see sensitive fields
      result =
        admin_fp_profile
        |> Ash.load!(:user_sensitive_info_safe, actor: admin_user_fp, authorize?: true)

      assert result.user_sensitive_info_safe == "admin_sensitive_data"
    end

    @tag :field_policies
    test "calculation returns nil for admin-only field when accessed by regular user", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Regular user cannot see admin-only fields
      result =
        admin_fp_profile
        |> Ash.load!(:user_admin_info_safe, actor: regular_user_fp, authorize?: true)

      assert is_nil(result.user_admin_info_safe)
    end

    @tag :field_policies
    test "calculation returns value for admin-only field when accessed by admin", %{
      admin_user_fp: admin_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Admin can see admin-only fields
      result =
        admin_fp_profile
        |> Ash.load!(:user_admin_info_safe, actor: admin_user_fp, authorize?: true)

      assert result.user_admin_info_safe == "admin_only_data"
    end

    @tag :field_policies
    test "complex calculation with mixed field policies returns nil when any field is denied", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Regular user can see name but not sensitive_field or admin_only_field
      result =
        admin_fp_profile
        |> Ash.load!(:user_summary_safe, actor: regular_user_fp, authorize?: true)

      # When there are forbidden field references, the whole calculation should return nil
      assert is_nil(result.user_summary_safe)
    end

    @tag :field_policies
    test "calculation with only allowed field references works normally", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Name and email don't have field policies restricting them
      result =
        admin_fp_profile
        |> Ash.load!(:user_contact_info, actor: regular_user_fp, authorize?: true)

      assert result.user_contact_info == "Admin User - admin@example.com"
    end

    @tag :field_policies
    test "regular user accessing their own restricted fields still gets denied", %{
      regular_user_fp: regular_user_fp,
      regular_fp_profile: regular_fp_profile
    } do
      # Even accessing their own data, regular user cannot see sensitive fields
      result =
        regular_fp_profile
        |> Ash.load!(:user_sensitive_info_safe, actor: regular_user_fp, authorize?: true)

      assert is_nil(result.user_sensitive_info_safe)
    end
  end

  describe "reading multiple records with field policies and authorize_references?" do
    @tag :field_policies
    test "calculations respect field policies when loading multiple records", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile,
      regular_fp_profile: regular_fp_profile
    } do
      # Load multiple profiles with calculations that reference fields with policies
      profiles =
        FieldPolicyProfile
        |> Ash.Query.load([:user_sensitive_info_safe, :user_contact_info])
        |> Ash.read!(actor: regular_user_fp, authorize?: true)

      admin_result = Enum.find(profiles, &(&1.id == admin_fp_profile.id))
      regular_result = Enum.find(profiles, &(&1.id == regular_fp_profile.id))

      # Sensitive info should be nil for regular user for both profiles
      assert is_nil(admin_result.user_sensitive_info_safe)
      assert is_nil(regular_result.user_sensitive_info_safe)

      # Contact info should work for both (no field policies)
      assert admin_result.user_contact_info == "Admin User - admin@example.com"
      assert regular_result.user_contact_info == "Regular User - regular@example.com"
    end

    @tag :field_policies
    test "admin can see field policy restricted data in calculations", %{
      admin_user_fp: admin_user_fp,
      admin_fp_profile: admin_fp_profile,
      regular_fp_profile: regular_fp_profile
    } do
      # Admin should be able to see sensitive data in calculations
      profiles =
        FieldPolicyProfile
        |> Ash.Query.load([:user_sensitive_info_safe, :user_admin_info_safe])
        |> Ash.read!(actor: admin_user_fp, authorize?: true)

      admin_result = Enum.find(profiles, &(&1.id == admin_fp_profile.id))
      regular_result = Enum.find(profiles, &(&1.id == regular_fp_profile.id))

      # Admin can see sensitive fields for all users
      assert admin_result.user_sensitive_info_safe == "admin_sensitive_data"
      assert regular_result.user_sensitive_info_safe == "regular_sensitive_data"

      # Admin can see admin-only fields for all users
      assert admin_result.user_admin_info_safe == "admin_only_data"
      assert regular_result.user_admin_info_safe == "regular_admin_data"
    end
  end

  describe "filtering with calculations referencing fields with field policies" do
    @tag :field_policies
    test "filter with calculation referencing restricted field works when authorize? false", %{
      admin_fp_profile: admin_fp_profile
    } do
      # Without authorization, filter should work normally
      profiles =
        FieldPolicyProfile
        |> Ash.Query.filter(user_sensitive_info_safe == "admin_sensitive_data")
        |> Ash.read!(authorize?: false)

      assert length(profiles) == 1
      assert hd(profiles).id == admin_fp_profile.id
    end

    @tag :field_policies
    test "filter with calculation referencing restricted field treats value as nil when authorize?: true & authorize_references?: true",
         %{regular_user_fp: regular_user_fp} do
      # With authorization and authorize_references, the calculation should be treated as nil
      # so no records should match the filter
      profiles =
        FieldPolicyProfile
        |> Ash.Query.filter(user_sensitive_info_safe == "admin_sensitive_data")
        |> Ash.read!(actor: regular_user_fp, authorize?: true)

      # Should return no results because user_sensitive_info_safe is treated as nil
      assert profiles == []
    end

    @tag :field_policies
    test "filter for nil matches records with field policy restricted calculations", %{
      regular_user_fp: regular_user_fp,
      admin_fp_profile: admin_fp_profile,
      regular_fp_profile: regular_fp_profile
    } do
      # When filtering for nil, should match all records since field policy refs become nil
      profiles =
        FieldPolicyProfile
        |> Ash.Query.filter(is_nil(user_sensitive_info_safe))
        |> Ash.read!(actor: regular_user_fp, authorize?: true)

      # Should return both profiles since user_sensitive_info_safe is treated as nil for regular user
      assert length(profiles) == 2
      profile_ids = Enum.map(profiles, & &1.id) |> Enum.sort()
      expected_ids = [admin_fp_profile.id, regular_fp_profile.id] |> Enum.sort()
      assert profile_ids == expected_ids
    end

    @tag :field_policies
    test "admin can filter on field policy restricted calculations", %{
      admin_user_fp: admin_user_fp,
      admin_fp_profile: admin_fp_profile
    } do
      # Admin should be able to filter on sensitive data
      profiles =
        FieldPolicyProfile
        |> Ash.Query.filter(user_sensitive_info_safe == "admin_sensitive_data")
        |> Ash.read!(actor: admin_user_fp, authorize?: true)

      assert length(profiles) == 1
      assert hd(profiles).id == admin_fp_profile.id
    end
  end
end

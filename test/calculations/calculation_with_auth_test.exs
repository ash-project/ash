# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.CalculationWithAuthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule Name do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [:first_name, :last_name, :show_last_name]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           first_name: first_name,
           show_last_name: true,
           last_name: last_name
         } ->
           "#{first_name} #{last_name}"

         %{
           first_name: first_name,
           show_last_name: false
         } ->
           first_name
       end)}
    end
  end

  defmodule SelfName do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [self: [:first_name, :last_name, :show_last_name]]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           self: %{
             first_name: first_name,
             show_last_name: true,
             last_name: last_name
           }
         } ->
           "#{first_name} #{last_name}"

         %{
           self: %{
             first_name: first_name,
             show_last_name: false
           }
         } ->
           first_name
       end)}
    end
  end

  defmodule SelfSelfName do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [self: [self: [:first_name, :last_name, :show_last_name]]]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           self: %{
             self: %{
               first_name: first_name,
               show_last_name: true,
               last_name: last_name
             }
           }
         } ->
           "#{first_name} #{last_name}"

         %{
           self: %{
             self: %{
               first_name: first_name,
               show_last_name: false
             }
           }
         } ->
           first_name
       end)}
    end
  end

  defmodule EmbeddedNotAllowed do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [embedded: [:not_allowed]]

    @impl true
    def calculate(records, _, _) do
      Enum.map(records, fn record ->
        record.embedded && record.embedded.not_allowed
      end)
    end
  end

  defmodule Embedded do
    use Ash.Resource,
      data_layer: :embedded,
      authorizers: [Ash.Policy.Authorizer]

    resource do
      require_primary_key? false
    end

    attributes do
      attribute :allowed, :string, public?: true
      attribute :not_allowed, :string, public?: true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :allowed do
        authorize_if always()
      end

      field_policy :not_allowed do
        forbid_if always()
      end
    end
  end

  defmodule Resource do
    use Ash.Resource,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :first_name, :string
      attribute :last_name, :string

      attribute :show_last_name, :boolean, public?: true
      attribute :embedded, Embedded
    end

    code_interface do
      domain Domain

      define :create
      define :read
    end

    calculations do
      calculate :name, :string, Name, public?: true
      calculate :self_name, :string, SelfName, public?: true
      calculate :self_self_name, :string, SelfSelfName, public?: true
      calculate :embedded_not_allowed, :string, EmbeddedNotAllowed, public?: true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :show_last_name do
        forbid_if always()
      end

      field_policy [:name, :self_name, :self_self_name, :embedded_not_allowed] do
        authorize_if always()
      end
    end

    actions do
      defaults [:read, create: [:first_name, :last_name, :show_last_name, :embedded]]
    end

    relationships do
      has_one :self, __MODULE__ do
        source_attribute :id
        destination_attribute :id
      end
    end
  end

  test "can calculate even if calculate depends on forbidden field" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{name: "Homer", show_last_name: %Ash.ForbiddenField{}}] =
      Resource.read!(%{}, load: [:name, :show_last_name], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationship" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_name: "Homer"}] =
      Resource.read!(%{}, load: [:self_name, self: [:show_last_name]], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationsip, and the field will show as forbidden in the final data" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_name: "Homer", self: %{show_last_name: %Ash.ForbiddenField{}}}] =
      Resource.read!(%{}, load: [:self_name, self: [:show_last_name]], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationsip two levels deep, and the field will show as forbidden in the final data" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_self_name: "Homer", self: %{self: %{show_last_name: %Ash.ForbiddenField{}}}}] =
      Resource.read!(%{},
        load: [:self_self_name, self: [self: [:show_last_name]]],
        authorize?: true
      )
  end

  test "can calculate even if calculate depends on forbidden field from an embedded attribute" do
    Resource.create!(%{
      first_name: "Homer",
      last_name: "Simpson",
      show_last_name: false,
      embedded: %{allowed: "shown", not_allowed: "hidden"}
    })

    [%{embedded_not_allowed: "hidden"}] =
      Resource.read!(%{}, load: [:embedded_not_allowed], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from an embedded field, and the field will show as forbidden in the final data" do
    Resource.create!(%{
      first_name: "Homer",
      last_name: "Simpson",
      show_last_name: false,
      embedded: %{allowed: "shown", not_allowed: "hidden"}
    })

    [%{embedded_not_allowed: "hidden", embedded: %{not_allowed: %Ash.ForbiddenField{}}}] =
      Resource.read!(%{}, load: [:embedded_not_allowed], authorize?: true)
  end

  defmodule Org do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :public, :boolean, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule GatedAuthor do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      belongs_to :org, Org, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if expr(org.public == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :visible, :boolean, public?: true, default: true
    end

    relationships do
      has_many :edited_posts, Ash.Test.Resource.CalculationWithAuthTest.Post,
        destination_attribute: :editor_id,
        public?: true

      has_many :edited_posts_with_named_author, Ash.Test.Resource.CalculationWithAuthTest.Post do
        destination_attribute :editor_id
        filter expr(not is_nil(author_name))
        public? true
      end

      has_many :edited_posts_with_named_author_through_calculation,
               Ash.Test.Resource.CalculationWithAuthTest.Post do
        destination_attribute :editor_id
        filter expr(author_name_or_none != "none")
        public? true
      end

      has_one :first_edited_post_sorted_by_author_name,
              Ash.Test.Resource.CalculationWithAuthTest.Post do
        destination_attribute :editor_id
        from_many? true
        sort author_name: :asc_nils_last
        public? true
      end
    end

    aggregates do
      list :edited_post_author_names, :edited_posts, :author_name do
        sort title: :asc
        include_nil? true
        public? true
      end

      count :count_of_edited_posts_with_named_author, :edited_posts_with_named_author do
        public? true
      end

      count :count_of_edited_posts_with_gated_author, :edited_posts do
        filter expr(not is_nil(gated_author_name))
        public? true
      end

      list :edited_post_trusted_author_names, :edited_posts, :trusted_author_name do
        sort title: :asc
        include_nil? true
        public? true
      end
    end

    calculations do
      calculate :edited_a_post_by_g2,
                :boolean,
                expr(exists(edited_posts, gated_author_name == "g2")) do
        public? true
      end
    end

    policies do
      policy action_type(:read) do
        authorize_if expr(visible == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
      attribute :visible, :boolean, public?: true, default: true
    end

    relationships do
      belongs_to :post, Ash.Test.Resource.CalculationWithAuthTest.Post, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if expr(visible == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule SecretAuthor do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :secret, :string, public?: true
      attribute :show_secret, :boolean, public?: true, default: false
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :secret do
        authorize_if expr(show_secret == true)
      end

      field_policy :* do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule AuthorName do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [author: [:name]]

    @impl true
    def calculate(records, _opts, _context) do
      {:ok, Enum.map(records, &(&1.author && &1.author.name))}
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    relationships do
      belongs_to :author, Author, public?: true
      belongs_to :editor, Author, public?: true
      belongs_to :secret_author, SecretAuthor, public?: true
      belongs_to :gated_author, GatedAuthor, public?: true
      has_many :comments, Comment, public?: true
    end

    calculations do
      calculate :author_name, :string, expr(author.name) do
        public? true
        authorize? true
      end

      calculate :trusted_author_name, :string, expr(author.name) do
        public? true
      end

      calculate :gated_author_name, :string, expr(gated_author.name) do
        public? true
        authorize? true
      end

      calculate :author_name_or_none,
                :string,
                expr(if(is_nil(author_name), "none", author_name)) do
        public? true
      end

      calculate :has_comments, :boolean, expr(exists(comments, true)) do
        public? true
        authorize? true
      end

      calculate :trusted_has_comments, :boolean, expr(exists(comments, true)) do
        public? true
      end

      calculate :loaded_author_name, :string, AuthorName do
        public? true
        authorize? true
      end

      calculate :has_unrelated_comments,
                :boolean,
                expr(
                  exists(Ash.Test.Resource.CalculationWithAuthTest.Comment, post_id == parent(id))
                ) do
        public? true
        authorize? true
      end

      calculate :has_comment_posts, :boolean, expr(exists(comments.post, true)) do
        public? true
        authorize? true
      end

      calculate :trusted_has_comment_posts, :boolean, expr(exists(comments.post, true)) do
        public? true
      end

      calculate :has_comment_on_self, :boolean, expr(exists(comments.post, id == parent(id))) do
        public? true
        authorize? true
      end

      calculate :secret, :string, expr(secret_author.secret) do
        public? true
        authorize? true
      end

      calculate :trusted_loaded_author_name, :string, AuthorName do
        public? true
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defp create_posts(_) do
    hidden_a = Ash.create!(Author, %{name: "a", visible: false}, authorize?: false)
    visible_b = Ash.create!(Author, %{name: "b", visible: true}, authorize?: false)
    hidden_c = Ash.create!(Author, %{name: "c", visible: false}, authorize?: false)

    secret_a = Ash.create!(SecretAuthor, %{secret: "a", show_secret: false}, authorize?: false)
    secret_b = Ash.create!(SecretAuthor, %{secret: "b", show_secret: true}, authorize?: false)
    secret_c = Ash.create!(SecretAuthor, %{secret: "c", show_secret: false}, authorize?: false)

    public_org = Ash.create!(Org, %{public: true}, authorize?: false)
    private_org = Ash.create!(Org, %{public: false}, authorize?: false)
    g1 = Ash.create!(GatedAuthor, %{name: "g1", org_id: public_org.id}, authorize?: false)
    g2 = Ash.create!(GatedAuthor, %{name: "g2", org_id: private_org.id}, authorize?: false)

    post_1 =
      Ash.create!(
        Post,
        %{
          title: "1",
          gated_author_id: g1.id,
          author_id: hidden_c.id,
          editor_id: visible_b.id,
          secret_author_id: secret_c.id
        },
        authorize?: false
      )

    post_2 =
      Ash.create!(
        Post,
        %{
          title: "2",
          gated_author_id: g2.id,
          author_id: visible_b.id,
          editor_id: visible_b.id,
          secret_author_id: secret_b.id
        },
        authorize?: false
      )

    post_3 =
      Ash.create!(
        Post,
        %{
          title: "3",
          author_id: hidden_a.id,
          editor_id: visible_b.id,
          secret_author_id: secret_a.id
        },
        authorize?: false
      )

    Ash.create!(Comment, %{body: "hidden", visible: false, post_id: post_1.id}, authorize?: false)

    Ash.create!(Comment, %{body: "shown", visible: true, post_id: post_2.id}, authorize?: false)

    %{post_1: post_1, post_2: post_2, post_3: post_3}
  end

  defp titles(posts), do: Enum.map(posts, & &1.title)

  describe "sort_input on a related field" do
    setup :create_posts

    test "does not order by related records the actor cannot read" do
      # Without authorization, hidden authors "a" and "c" would order post 3 before post 1.
      assert ["2", "1", "3"] =
               Post
               |> Ash.Query.sort_input("author.name,title")
               |> Ash.read!(authorize?: true)
               |> titles()

      assert ["1", "3", "2"] =
               Post
               |> Ash.Query.sort_input("-author.name,title")
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "does not order by related fields hidden by field policies" do
      assert ["2", "1", "3"] =
               Post
               |> Ash.Query.sort_input("secret_author.secret,title")
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "rejects sorting through to-many relationships" do
      assert %Ash.Query{
               errors: [%Ash.Error.Query.UnsortableField{field: :comments, reason: :to_many}]
             } = Ash.Query.sort_input(Post, "comments.body")

      assert %Ash.Query{
               errors: [%Ash.Error.Query.UnsortableField{field: :comments, reason: :to_many}]
             } = Ash.Query.sort(Post, "comments.body")
    end

    test "still sorts on related data when not authorizing" do
      assert ["3", "2", "1"] =
               Post
               |> Ash.Query.sort_input("author.name")
               |> Ash.read!(authorize?: false)
               |> titles()
    end
  end

  describe "calculations with authorize? true" do
    setup :create_posts

    test "related references resolve to nil when the related record is not readable" do
      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:author_name)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.author_name)
    end

    test "calculations without authorize? are unchanged" do
      assert ["c", "b", "a"] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:trusted_author_name)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.trusted_author_name)
    end

    test "related references are not authorized when the query is not authorized" do
      assert ["c", "b", "a"] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:author_name)
               |> Ash.read!(authorize?: false)
               |> Enum.map(& &1.author_name)
    end

    test "sorting on the calculation uses the authorized value" do
      assert ["2", "1", "3"] =
               Post
               |> Ash.Query.sort(author_name: :asc, title: :asc)
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "filtering on the calculation uses the authorized value" do
      assert [] =
               Post
               |> Ash.Query.filter(author_name == "a")
               |> Ash.read!(authorize?: true)

      assert ["1", "3"] =
               Post
               |> Ash.Query.filter(is_nil(author_name))
               |> Ash.Query.sort(:title)
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "exists only sees readable related records" do
      assert [false, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:has_comments)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.has_comments)

      assert [true, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:trusted_has_comments)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.trusted_has_comments)
    end

    test "unrelated exists only sees readable records" do
      assert [false, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:has_unrelated_comments)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.has_unrelated_comments)
    end

    test "exists through multiple relationships only sees readable records" do
      assert [false, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:has_comment_posts)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.has_comment_posts)

      assert [true, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:trusted_has_comment_posts)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.trusted_has_comment_posts)

      assert [false, true, false] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:has_comment_on_self)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.has_comment_on_self)
    end

    test "fields hidden by field policies resolve to nil" do
      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:secret)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.secret)
    end

    test "filtering on exists only sees readable related records" do
      assert ["2"] =
               Post
               |> Ash.Query.filter(has_comments)
               |> Ash.read!(authorize?: true)
               |> titles()
    end
  end

  describe "aggregates over calculations with authorize? true" do
    setup :create_posts

    test "use the authorized value" do
      assert [%{edited_post_author_names: [nil, "b", nil]}] =
               Author
               |> Ash.Query.load(:edited_post_author_names)
               |> Ash.read!(authorize?: true)

      assert [%{edited_post_trusted_author_names: ["c", "b", "a"]}] =
               Author
               |> Ash.Query.load(:edited_post_trusted_author_names)
               |> Ash.read!(authorize?: true)
    end
  end

  describe "relationship definitions" do
    setup :create_posts

    test "run calculations in their filters unauthorized" do
      assert [%{edited_posts_with_named_author: posts}] =
               Author
               |> Ash.Query.load(:edited_posts_with_named_author)
               |> Ash.read!(authorize?: true)

      assert ["1", "2", "3"] = posts |> Enum.map(& &1.title) |> Enum.sort()

      assert [%{count_of_edited_posts_with_named_author: 3}] =
               Author
               |> Ash.Query.load(:count_of_edited_posts_with_named_author)
               |> Ash.read!(authorize?: true)
    end

    test "run calculations used by calculations in their filters unauthorized" do
      assert [%{edited_posts_with_named_author_through_calculation: posts}] =
               Author
               |> Ash.Query.load(:edited_posts_with_named_author_through_calculation)
               |> Ash.read!(authorize?: true)

      assert ["1", "2", "3"] = posts |> Enum.map(& &1.title) |> Enum.sort()
    end

    test "run calculations in their sorts unauthorized" do
      assert [%{first_edited_post_sorted_by_author_name: %{title: "3"}}] =
               Author
               |> Ash.Query.load(:first_edited_post_sorted_by_author_name)
               |> Ash.read!(authorize?: true)
    end
  end

  describe "calculations with authorize? true that load related data" do
    setup :create_posts

    test "relationships they load are authorized" do
      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:loaded_author_name)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.loaded_author_name)
    end

    test "authorized and unauthorized loads of the same relationship are kept apart" do
      posts =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.Query.load([:loaded_author_name, :trusted_loaded_author_name])
        |> Ash.read!(authorize?: true)

      assert [nil, "b", nil] = Enum.map(posts, & &1.loaded_author_name)
      assert ["c", "b", "a"] = Enum.map(posts, & &1.trusted_loaded_author_name)
    end

    test "authorized loads are kept apart from the query's own loads" do
      posts =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.Query.load([:loaded_author_name, :author])
        |> Ash.read!(authorize?: true)

      assert [nil, "b", nil] = Enum.map(posts, & &1.loaded_author_name)
      assert [nil, "b", nil] = Enum.map(posts, &(&1.author && &1.author.name))
    end

    test "relationships they load are not authorized when the query is not authorized" do
      assert ["c", "b", "a"] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:loaded_author_name)
               |> Ash.read!(authorize?: false)
               |> Enum.map(& &1.loaded_author_name)
    end

    test "relationships that are already loaded are not reused" do
      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:author)
               |> Ash.read!(authorize?: false)
               |> Ash.load!([:loaded_author_name], authorize?: true, reuse_values?: true)
               |> Enum.map(& &1.loaded_author_name)
    end

    test "works with Ash.load" do
      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.read!(authorize?: false)
               |> Ash.load!([:loaded_author_name], authorize?: true)
               |> Enum.map(& &1.loaded_author_name)
    end
  end

  describe "authorized calculations whose related read policies use other relationships" do
    setup :create_posts

    test "loading" do
      assert ["g1", nil, nil] =
               Post
               |> Ash.Query.sort(:title)
               |> Ash.Query.load(:gated_author_name)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.gated_author_name)
    end

    test "filtering" do
      assert ["1"] =
               Post
               |> Ash.Query.filter(not is_nil(gated_author_name))
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "sort_input" do
      assert ["2", "3", "1"] =
               Post
               |> Ash.Query.sort_input("-gated_author.name,title")
               |> Ash.read!(authorize?: true)
               |> titles()
    end

    test "aggregate filters" do
      assert [%{count_of_edited_posts_with_gated_author: 1}] =
               Author
               |> Ash.Query.load(:count_of_edited_posts_with_gated_author)
               |> Ash.read!(authorize?: true)
    end

    test "exists in calculations" do
      assert [%{edited_a_post_by_g2: false}] =
               Author
               |> Ash.Query.load(:edited_a_post_by_g2)
               |> Ash.read!(authorize?: true)
    end
  end

  describe "query calculations with authorize? true" do
    setup :create_posts

    test "related references only see readable related records" do
      calculate = fn query, opts ->
        Ash.Query.calculate(
          query,
          :adhoc_author_name,
          :string,
          Ash.Expr.expr(author.name),
          %{},
          [],
          %{},
          opts
        )
      end

      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> calculate.(authorize?: true)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.calculations.adhoc_author_name)

      assert ["c", "b", "a"] =
               Post
               |> Ash.Query.sort(:title)
               |> calculate.([])
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.calculations.adhoc_author_name)
    end

    test "relationships they load are authorized" do
      calculate = fn query, opts ->
        Ash.Query.calculate(
          query,
          :adhoc_loaded_author_name,
          :string,
          AuthorName,
          %{},
          [],
          %{},
          opts
        )
      end

      assert [nil, "b", nil] =
               Post
               |> Ash.Query.sort(:title)
               |> calculate.(authorize?: true)
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.calculations.adhoc_loaded_author_name)

      assert ["c", "b", "a"] =
               Post
               |> Ash.Query.sort(:title)
               |> calculate.([])
               |> Ash.read!(authorize?: true)
               |> Enum.map(& &1.calculations.adhoc_loaded_author_name)
    end
  end
end

defmodule Ash.Resource.Dsl do
  @attribute %Spark.Dsl.Entity{
    name: :attribute,
    describe: """
    Declares an attribute on the resource.
    """,
    links: [],
    examples: [
      """
      attribute :name, :string do
        allow_nil? false
      end
      """
    ],
    transform: {Ash.Resource.Attribute, :transform, []},
    target: Ash.Resource.Attribute,
    args: [:name, :type],
    modules: [:type],
    schema: Ash.Resource.Attribute.attribute_schema()
  }

  @create_timestamp %Spark.Dsl.Entity{
    name: :create_timestamp,
    describe: """
    Declares a non-writable attribute with a create default of `&DateTime.utc_now/0`
    """,
    examples: [
      "create_timestamp :inserted_at"
    ],
    links: [],
    target: Ash.Resource.Attribute,
    args: [:name],
    schema: Ash.Resource.Attribute.create_timestamp_schema(),
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @update_timestamp %Spark.Dsl.Entity{
    name: :update_timestamp,
    describe: """
    Declares a non-writable attribute with a create and update default of `&DateTime.utc_now/0`
    """,
    links: [],
    examples: [
      "update_timestamp :inserted_at"
    ],
    target: Ash.Resource.Attribute,
    schema: Ash.Resource.Attribute.update_timestamp_schema(),
    args: [:name],
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @integer_primary_key %Spark.Dsl.Entity{
    name: :integer_primary_key,
    describe: """
    Declares a generated, non writable, non-nil, primary key column of type integer.

    Generated integer primary keys must be supported by the data layer.
    """,
    examples: [
      "integer_primary_key :id"
    ],
    links: [],
    args: [:name],
    target: Ash.Resource.Attribute,
    schema: Ash.Resource.Attribute.integer_primary_key_schema(),
    auto_set_fields: [allow_nil?: false],
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @uuid_primary_key %Spark.Dsl.Entity{
    name: :uuid_primary_key,
    describe: """
    Declares a non writable, non-nil, primary key column of type uuid, which defaults to `Ash.UUID.generate/0`.
    """,
    examples: [
      "uuid_primary_key :id"
    ],
    links: [],
    args: [:name],
    target: Ash.Resource.Attribute,
    schema: Ash.Resource.Attribute.uuid_primary_key_schema(),
    auto_set_fields: [allow_nil?: false],
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @attributes %Spark.Dsl.Section{
    name: :attributes,
    describe: """
    A section for declaring attributes on the resource.
    """,
    imports: [Ash.Resource.Attribute.Helpers],
    links: [],
    examples: [
      """
      attributes do
        uuid_primary_key :id

        attribute :first_name, :string do
          allow_nil? false
        end

        attribute :last_name, :string do
          allow_nil? false
        end

        attribute :email, :string do
          allow_nil? false

          constraints [
            match: ~r/^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$/
          ]
        end

        attribute :type, :atom do
          constraints [
            one_of: [:admin, :teacher, :student]
          ]
        end

        create_timestamp :inserted_at
        update_timestamp :updated_at
      end
      """
    ],
    entities: [
      @attribute,
      @create_timestamp,
      @update_timestamp,
      @integer_primary_key,
      @uuid_primary_key
    ]
  }

  @has_one %Spark.Dsl.Entity{
    name: :has_one,
    describe: """
    Declares a has_one relationship. In a relational database, the foreign key would be on the *other* table.

    Generally speaking, a `has_one` also implies that the destination table is unique on that foreign key.
    """,
    examples: [
      """
      # In a resource called `Word`
      has_one :dictionary_entry, DictionaryEntry do
        source_attribute :text
        destination_attribute :word_text
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Relationships"
      ]
    ],
    modules: [:destination],
    target: Ash.Resource.Relationships.HasOne,
    schema: Ash.Resource.Relationships.HasOne.opt_schema(),
    args: [:name, :destination]
  }

  @has_many %Spark.Dsl.Entity{
    name: :has_many,
    describe: """
    Declares a has_many relationship. There can be any number of related entities.
    """,
    examples: [
      """
      # In a resource called `Word`
      has_many :definitions, DictionaryDefinition do
        source_attribute :text
        destination_attribute :word_text
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Relationships"
      ]
    ],
    target: Ash.Resource.Relationships.HasMany,
    modules: [:destination],
    schema: Ash.Resource.Relationships.HasMany.opt_schema(),
    args: [:name, :destination]
  }

  @many_to_many %Spark.Dsl.Entity{
    name: :many_to_many,
    describe: """
    Declares a many_to_many relationship. Many to many relationships require a join resource.

    A join resource is a resource that consists of a relationship to the source and destination of the many to many.
    """,
    links: [
      guides: [
        "ash:guide:Relationships"
      ]
    ],
    examples: [
      """
      # In a resource called `Word`
      many_to_many :books, Book do
        through BookWord
        source_attribute :text
        source_attribute_on_join_resource :word_text
        destination_attribute :id
        destination_attribute_on_join_resource :book_id
      end

      # And in `BookWord` (the join resource)
      belongs_to :book, Book, primary_key?: true, allow_nil?: false
      belongs_to :word, Word, primary_key?: true, allow_nil?: false
      """
    ],
    modules: [:destination, :through],
    target: Ash.Resource.Relationships.ManyToMany,
    schema: Ash.Resource.Relationships.ManyToMany.opt_schema(),
    transform: {Ash.Resource.Relationships.ManyToMany, :transform, []},
    args: [:name, :destination]
  }

  @belongs_to %Spark.Dsl.Entity{
    name: :belongs_to,
    describe: """
    Declares a belongs_to relationship. In a relational database, the foreign key would be on the *source* table.

    This creates a field on the resource with the corresponding name and type, unless `define_attribute?: false` is provided.
    """,
    links: [
      guides: [
        "ash:guide:Relationships"
      ]
    ],
    examples: [
      """
      # In a resource called `Word`
      belongs_to :dictionary_entry, DictionaryEntry do
        source_attribute :text,
        destination_attribute :word_text
      end
      """
    ],
    modules: [:destination],
    target: Ash.Resource.Relationships.BelongsTo,
    schema: Ash.Resource.Relationships.BelongsTo.opt_schema(),
    args: [:name, :destination]
  }

  @relationships %Spark.Dsl.Section{
    name: :relationships,
    describe: """
    A section for declaring relationships on the resource.

    Relationships are a core component of resource oriented design. Many components of Ash
    will use these relationships. A simple use case is loading relationships (done via the `Ash.Query.load/2`).
    """,
    links: [
      guides: [
        "ash:guide:Relationships"
      ]
    ],
    examples: [
      """
      relationships do
        belongs_to :post, MyApp.Post do
          primary_key? true
        end

        belongs_to :category, MyApp.Category do
          primary_key? true
        end
      end
      """,
      """
      relationships do
        belongs_to :author, MyApp.Author

        many_to_many :categories, MyApp.Category do
          through MyApp.PostCategory
          destination_attribute_on_join_resource :category_id
          source_attribute_on_join_resource :post_id
        end
      end
      """,
      """
      relationships do
        has_many :posts, MyApp.Post do
          destination_attribute :author_id
        end

        has_many :composite_key_posts, MyApp.CompositeKeyPost do
          destination_attribute :author_id
        end
      end
      """
    ],
    imports: [
      Ash.Filter.TemplateHelpers
    ],
    entities: [
      @has_one,
      @has_many,
      @many_to_many,
      @belongs_to
    ]
  }

  @action_change %Spark.Dsl.Entity{
    name: :change,
    describe: """
    A change to be applied to the changeset.
    """,
    links: [
      guides: [
        "ash:guide:Actions"
      ],
      modules: [
        "ash:module:Ash.Resource.Change"
      ]
    ],
    examples: [
      "change relate_actor(:reporter)",
      "change {MyCustomChange, :foo}"
    ],
    no_depend_modules: [:change],
    target: Ash.Resource.Change,
    schema: Ash.Resource.Change.action_schema(),
    args: [:change]
  }

  @action_argument %Spark.Dsl.Entity{
    name: :argument,
    describe: """
    Declares an argument on the action
    """,
    links: [],
    examples: [
      "argument :password_confirmation, :string"
    ],
    modules: [:type],
    target: Ash.Resource.Actions.Argument,
    args: [:name, :type],
    transform: {Ash.Type, :set_type_transformation, []},
    schema: Ash.Resource.Actions.Argument.schema()
  }

  @metadata %Spark.Dsl.Entity{
    name: :metadata,
    describe: """
    A special kind of attribute that is only added to specific actions. Nothing sets this value, it must be set in a custom
    change via `Ash.Resource.Info.put_metadata/3`.
    """,
    links: [],
    examples: [
      """
      metadata :api_token, :string, allow_nil?: false
      """,
      """
      metadata :operation_id, :string, allow_nil?: false
      """
    ],
    target: Ash.Resource.Actions.Metadata,
    args: [:name, :type],
    schema: Ash.Resource.Actions.Metadata.schema(),
    transform: {Ash.Type, :set_type_transformation, []}
  }

  @change %Spark.Dsl.Entity{
    name: :change,
    links: [
      guides: [
        "ash:guide:Actions"
      ],
      modules: [
        "ash:module:Ash.Resource.Change"
      ]
    ],
    describe: """
    A change to be applied to the changeset.
    """,
    examples: [
      "change relate_actor(:reporter)",
      "change {MyCustomChange, :foo}"
    ],
    no_depend_modules: [:change],
    target: Ash.Resource.Change,
    schema: Ash.Resource.Change.schema(),
    args: [:change]
  }

  @validate %Spark.Dsl.Entity{
    name: :validate,
    describe: """
    Declares a validation for creates and updates.
    """,
    examples: [
      "validate {Mod, [foo: :bar]}",
      "validate at_least_one_of_present([:first_name, :last_name])"
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ],
      modules: [
        "ash:module:Ash.Resource.Change"
      ]
    ],
    target: Ash.Resource.Validation,
    schema: Ash.Resource.Validation.opt_schema(),
    no_depend_modules: [:validation],
    transform: {Ash.Resource.Validation, :transform, []},
    args: [:validation]
  }

  @action_validate %Spark.Dsl.Entity{
    name: :validate,
    describe: """
    Declares a validation to be applied to the changeset.
    """,
    examples: [
      "validate changing(:email)"
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ],
      modules: [
        "ash:module:Ash.Resource.Validation"
      ]
    ],
    target: Ash.Resource.Validation,
    schema: Ash.Resource.Validation.action_schema(),
    no_depend_modules: [:validation],
    transform: {Ash.Resource.Validation, :transform, []},
    args: [:validation]
  }

  @create %Spark.Dsl.Entity{
    name: :create,
    describe: """
    Declares a `create` action. For calling this action, see the `Ash.Api` documentation.
    """,
    examples: [
      """
      create :register do
        primary? true
      end
      """
    ],
    imports: [
      Ash.Resource.Change.Builtins,
      Ash.Resource.Validation.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    target: Ash.Resource.Actions.Create,
    schema: Ash.Resource.Actions.Create.opt_schema(),
    no_depend_modules: [:touches_resources],
    entities: [
      changes: [
        @action_change,
        @action_validate
      ],
      arguments: [
        @action_argument
      ],
      metadata: [
        @metadata
      ]
    ],
    args: [:name]
  }

  @prepare %Spark.Dsl.Entity{
    name: :prepare,
    describe: """
    Declares a preparation, which can be used to prepare a query for a read action.
    """,
    examples: [
      """
      prepare build(sort: [:foo, :bar])
      """
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    target: Ash.Resource.Preparation,
    schema: Ash.Resource.Preparation.schema(),
    no_depend_modules: [:preparation],
    args: [:preparation]
  }

  @pagination %Spark.Dsl.Entity{
    name: :pagination,
    describe: """
    Adds pagination options to a resource
    """,
    target: Ash.Resource.Actions.Read.Pagination,
    schema: Ash.Resource.Actions.Read.pagination_schema(),
    transform: {Ash.Resource.Actions.Read.Pagination, :transform, []},
    links: [
      guides: [
        "ash:guide:Pagination"
      ]
    ]
  }

  @read %Spark.Dsl.Entity{
    name: :read,
    describe: """
    Declares a `read` action. For calling this action, see the `Ash.Api` documentation.
    """,
    examples: [
      """
      read :read_all do
        primary? true
      end
      """
    ],
    imports: [
      Ash.Resource.Preparation.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    target: Ash.Resource.Actions.Read,
    schema: Ash.Resource.Actions.Read.opt_schema(),
    transform: {Ash.Resource.Actions.Read, :transform, []},
    no_depend_modules: [:touches_resources],
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    entities: [
      arguments: [
        @action_argument
      ],
      preparations: [
        @prepare
      ],
      pagination: [
        @pagination
      ]
    ],
    args: [:name]
  }

  @update %Spark.Dsl.Entity{
    name: :update,
    describe: """
    Declares a `update` action. For calling this action, see the `Ash.Api` documentation.
    """,
    imports: [
      Ash.Resource.Change.Builtins,
      Ash.Resource.Validation.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    examples: [
      "update :flag_for_review, primary?: true"
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    entities: [
      changes: [
        @action_change,
        @action_validate
      ],
      metadata: [
        @metadata
      ],
      arguments: [
        @action_argument
      ]
    ],
    no_depend_modules: [:touches_resources],
    target: Ash.Resource.Actions.Update,
    schema: Ash.Resource.Actions.Update.opt_schema(),
    args: [:name]
  }

  @destroy %Spark.Dsl.Entity{
    name: :destroy,
    describe: """
    Declares a `destroy` action. For calling this action, see the `Ash.Api` documentation.
    """,
    examples: [
      """
      destroy :soft_delete do
        primary? true
      end
      """
    ],
    imports: [
      Ash.Resource.Change.Builtins,
      Ash.Resource.Validation.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    no_depend_modules: [:touches_resources],
    entities: [
      changes: [
        @action_change,
        @action_validate
      ],
      metadata: [
        @metadata
      ],
      arguments: [
        @action_argument
      ]
    ],
    target: Ash.Resource.Actions.Destroy,
    schema: Ash.Resource.Actions.Destroy.opt_schema(),
    args: [:name]
  }

  @actions %Spark.Dsl.Section{
    name: :actions,
    describe: """
    A section for declaring resource actions.

    All manipulation of data through the underlying data layer happens through actions.
    There are four types of action: `create`, `read`, `update`, and `destroy`. You may
    recognize these from the acronym `CRUD`. You can have multiple actions of the same
    type, as long as they have different names. This is the primary mechanism for customizing
    your resources to conform to your business logic. It is normal and expected to have
    multiple actions of each type in a large application.
    """,
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    schema: [
      defaults: [
        type: {:list, {:in, [:create, :read, :update, :destroy]}},
        doc: """
        Creates a simple action of each specified type, with the same name as the type.
        These actions will be the primary actions, unless you've declared a different action
        of the same type that is explicitly set as primary.

        By default, resources have no default actions. Embedded resources, however, have a default
        of all resource types.
        """,
        links: []
      ],
      default_accept: [
        type: {:list, :atom},
        doc:
          "A default value for the `accept` option for each action. Defaults to all public attributes.",
        links: []
      ]
    ],
    examples: [
      """
      actions do
        create :signup do
          argument :password, :string
          argument :password_confirmation, :string
          validate confirm(:password, :password_confirmation)
          change {MyApp.HashPassword, []} # A custom implemented Change
        end

        read :me do
          # An action that auto filters to only return the user for the current user
          filter [id: actor(:id)]
        end

        update :update do
          accept [:first_name, :last_name]
        end

        destroy do
          change set_attribute(:deleted_at, &DateTime.utc_now/0)
          # This tells it that even though this is a delete action, it
          # should be treated like an update because `deleted_at` is set.
          # This should be coupled with a `base_filter` on the resource
          # or with the read actions having a `filter` for `is_nil: :deleted_at`
          soft? true
        end
      end
      """
    ],
    entities: [
      @create,
      @read,
      @update,
      @destroy
    ]
  }

  @identity %Spark.Dsl.Entity{
    name: :identity,
    describe: """
    Represents a unique constraint on the resource.
    """,
    examples: [
      "identity :name, [:name]",
      "identity :full_name, [:first_name, :last_name]"
    ],
    links: [
      guides: [
        "ash:guide:Identities"
      ]
    ],
    target: Ash.Resource.Identity,
    schema: Ash.Resource.Identity.schema(),
    no_depend_modules: [:pre_check_with, :eager_check_with],
    args: [:name, :keys]
  }

  @identities %Spark.Dsl.Section{
    name: :identities,
    links: [],
    describe: """
    Unique identifiers for the resource
    """,
    examples: [
      """
      identities do
        identity :full_name, [:first_name, :last_name]
        identity :email, [:email]
      end
      """
    ],
    entities: [
      @identity
    ]
  }

  @resource %Spark.Dsl.Section{
    name: :resource,
    describe: """
    General Resource Configuration
    """,
    examples: [
      """
      resource do
        description "A description of this resource"
        base_filter [is_nil: :deleted_at]
      end
      """
    ],
    links: [],
    imports: [Ash.Filter.TemplateHelpers],
    modules: [:simple_notifiers],
    schema: [
      description: [
        type: :string,
        doc:
          "A human readable description of the resource, to be used in generated documentation",
        links: []
      ],
      base_filter: [
        type: :any,
        doc: "A filter statement to be applied to any queries on the resource",
        links: []
      ],
      default_context: [
        type: :any,
        doc: "Default context to apply to any queries/changesets generated for this resource.",
        links: []
      ],
      trace_name: [
        type: :string,
        doc: "The name to use in traces. Defaults to the short_name stringified",
        links: [
          guides: [
            "ash:guide:Instrumentation"
          ]
        ]
      ],
      short_name: [
        type: :atom,
        doc: "A short identifier for the resource, which should be unique.",
        links: [
          guides: [
            "ash:guide:Instrumentation"
          ]
        ]
      ],
      simple_notifiers: [
        type: {:list, :module},
        doc:
          "A list of notifiers that require no DSL. Can be used to avoid compile time dependencies on notifiers"
      ]
    ]
  }

  @define %Spark.Dsl.Entity{
    name: :define,
    describe: """
    Defines a function on the Api with the corresponding name and arguments.
    """,
    examples: [
      "define :get_user_by_id, action: :get_by_id, args: [:id], get?: true"
    ],
    target: Ash.Resource.Interface,
    schema: Ash.Resource.Interface.schema(),
    links: [
      guides: [
        "ash:guide:Code Interface"
      ]
    ],
    transform: {Ash.Resource.Interface, :transform, []},
    args: [:name]
  }

  @code_interface %Spark.Dsl.Section{
    name: :code_interface,
    describe: """
    Functions that will be defined on the Api module to interact with this resource.
    """,
    examples: [
      """
      code_interface do
        define_for MyApp.Api
        define :create_user, action: :create
        define :get_user_by_id, action: :get_by_id, args: [:id], get?: true
      end
      """
    ],
    schema: [
      define_for: [
        type: {:spark, Ash.Api},
        doc:
          "Defines the code interface on the resource module directly, using the provided Api.",
        default: false,
        links: []
      ]
    ],
    links: [
      guides: [
        "ash:guide:Code Interface"
      ]
    ],
    entities: [
      @define
    ]
  }

  @validations %Spark.Dsl.Section{
    name: :validations,
    describe: """
    Declare validations prior to performing actions against the resource
    """,
    links: [
      guides: ["ash:guide:Actions"]
    ],
    imports: [Ash.Resource.Validation.Builtins],
    examples: [
      """
      validations do
        validate {Mod, [foo: :bar]}
        validate at_least_one_of_present([:first_name, :last_name])
      end
      """
    ],
    entities: [
      @validate
    ]
  }

  @changes %Spark.Dsl.Section{
    name: :changes,
    describe: """
    Declare changes that occur on create/update/destroy actions against the resource
    """,
    imports: [
      Ash.Resource.Validation.Builtins,
      Ash.Resource.Change.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    links: [
      guides: [
        "ash:guide:Actions"
      ],
      modules: [
        "ash:module:Ash.Resource.Change"
      ]
    ],
    examples: [
      """
      changes do
        change {Mod, [foo: :bar]}
        change set_context(%{some: :context})
      end
      """
    ],
    entities: [
      @change
    ]
  }

  @preparations %Spark.Dsl.Section{
    name: :preparations,
    describe: """
    Declare preparations that occur on all read actions for a given resource
    """,
    links: [
      guides: [
        "ash:guide:Actions"
      ]
    ],
    imports: [Ash.Resource.Preparation.Builtins, Ash.Resource.Validation.Builtins],
    examples: [
      """
      preparations do
        prepare {Mod, [foo: :bar]}
        prepare set_context(%{some: :context})
      end
      """
    ],
    entities: [
      @prepare
    ]
  }

  @count %Spark.Dsl.Entity{
    name: :count,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named count aggregate on the resource

    Supports `filter`, but not `sort` (because that wouldn't affect the count)
    """,
    examples: [
      """
      count :assigned_ticket_count, :assigned_tickets do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path],
    schema: Keyword.delete(Ash.Resource.Aggregate.schema(), :sort),
    auto_set_fields: [kind: :count]
  }

  @first %Spark.Dsl.Entity{
    name: :first,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `first` aggregate on the resource

    First aggregates return the first value of the related record
    that matches. Supports both `filter` and `sort`.
    """,
    examples: [
      """
      first :first_assigned_ticket_subject, :assigned_tickets, :subject do
        filter [active: true]
        sort [:subject]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Ash.Resource.Aggregate.schema(),
    auto_set_fields: [kind: :first]
  }

  @max %Spark.Dsl.Entity{
    name: :max,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `max` aggregate on the resource

    Supports `filter`, but not `sort` (because that wouldn't affect the max)
    """,
    examples: [
      """
      max :first_assigned_ticket_subject, :assigned_tickets, :severity do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Ash.Resource.Aggregate.schema() |> Keyword.delete(:sort),
    auto_set_fields: [kind: :max]
  }

  @min %Spark.Dsl.Entity{
    name: :min,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `min` aggregate on the resource

    Supports `filter`, but not `sort` (because that wouldn't affect the min)
    """,
    examples: [
      """
      min :first_assigned_ticket_subject, :assigned_tickets, :severity do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Ash.Resource.Aggregate.schema() |> Keyword.delete(:sort),
    auto_set_fields: [kind: :min]
  }

  @sum %Spark.Dsl.Entity{
    name: :sum,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `sum` aggregate on the resource

    Supports `filter`, but not `sort` (because that wouldn't affect the sum)
    """,
    examples: [
      """
      sum :assigned_ticket_price_sum, :assigned_tickets, :price do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Keyword.delete(Ash.Resource.Aggregate.schema(), :sort),
    auto_set_fields: [kind: :sum]
  }

  @avg %Spark.Dsl.Entity{
    name: :avg,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `avg` aggregate on the resource

    Supports `filter`, but not `sort` (because that wouldn't affect the avg)
    """,
    examples: [
      """
      avg :assigned_ticket_price_sum, :assigned_tickets, :price do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Keyword.delete(Ash.Resource.Aggregate.schema(), :sort),
    auto_set_fields: [kind: :avg]
  }

  @custom %Spark.Dsl.Entity{
    name: :custom,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `custom` aggregate on the resource

    Supports `filter` and `sort`.

    Custom aggregates provide an `implementation` which must implement data layer specific callbacks.

    See the relevant data layer documentation for more.
    """,
    examples: [
      """
      custom :author_names, :authors, :string do
        implementation {StringAgg, delimiter: ","}
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :type],
    schema:
      Ash.Resource.Aggregate.schema()
      |> Keyword.put(:type,
        type: :module,
        required: true,
        doc: "The type of the value returned by the aggregate"
      )
      |> Keyword.put(:implementation,
        type: {:spark_behaviour, Ash.Resource.Aggregate.CustomAggregate},
        required: true,
        doc: "The module that implements the relevant data layer callbacks"
      ),
    auto_set_fields: [kind: :custom]
  }

  @list %Spark.Dsl.Entity{
    name: :list,
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    describe: """
    Declares a named `list` aggregate on the resource.

    A list aggregate selects the list of all values for the given field
    and relationship combination.
    """,
    examples: [
      """
      list :assigned_ticket_prices, :assigned_tickets, :price do
        filter [active: true]
      end
      """
    ],
    target: Ash.Resource.Aggregate,
    args: [:name, :relationship_path, :field],
    schema: Ash.Resource.Aggregate.schema(),
    auto_set_fields: [kind: :list]
  }

  @aggregates %Spark.Dsl.Section{
    name: :aggregates,
    describe: """
    Declare named aggregates on the resource.

    These are aggregates that can be loaded only by name using `Ash.Query.load/2`.
    They are also available as top level fields on the resource.
    """,
    examples: [
      """
      aggregates do
        count :assigned_ticket_count, :reported_tickets do
          filter [active: true]
        end
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Aggregates"
      ]
    ],
    imports: [Ash.Filter.TemplateHelpers],
    entities: [
      @count,
      @first,
      @sum,
      @list,
      @max,
      @min,
      @avg,
      @custom
    ]
  }

  @argument %Spark.Dsl.Entity{
    name: :argument,
    describe: """
    An argument to be passed into the calculation's arguments map
    """,
    examples: [
      """
      argument :params, :map do
        default %{}
      end
      """,
      """
      argument :retries, :integer do
        allow_nil? false
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Calculations"
      ]
    ],
    target: Ash.Resource.Calculation.Argument,
    args: [:name, :type],
    schema: Ash.Resource.Calculation.Argument.schema(),
    transform: {Ash.Type, :set_type_transformation, []}
  }

  @calculate %Spark.Dsl.Entity{
    name: :calculate,
    describe: """
    Declares a named calculation on the resource.

    Takes a module that must adopt the `Ash.Calculation` behaviour. See that module
    for more information.

    To ensure that the necessary fields are selected:

    1.) Specifying the `select` option on a calculation in the resource.
    2.) Define a `select/2` callback in the calculation module
    3.) Set `always_select?` on the attribute in question
    """,
    examples: [
      {
        "`Ash.Calculation` implementation example:",
        "calculate :full_name, :string, {MyApp.FullName, keys: [:first_name, :last_name]}, select: [:first_name, :last_name]"
      },
      {
        "`expr/1` example:",
        "calculate :full_name, :string, expr(first_name <> \" \" <> last_name "
      }
    ],
    links: [
      guides: [
        "ash:guide:Calculation"
      ]
    ],
    target: Ash.Resource.Calculation,
    no_depend_modules: [:calculation],
    args: [:name, :type, :calculation],
    entities: [
      arguments: [@argument]
    ],
    transform: {Ash.Type, :set_type_transformation, []},
    schema: Ash.Resource.Calculation.schema()
  }

  @calculations %Spark.Dsl.Section{
    name: :calculations,
    links: [
      guides: [
        "ash:guide:Calculations"
      ]
    ],
    describe: """
    Declare named calculations on the resource.

    These are calculations that can be loaded only by name using `Ash.Query.load/2`.
    They are also available as top level fields on the resource.
    """,
    examples: [
      """
      calculations do
        calculate :full_name, :string, MyApp.MyResource.FullName
      end
      """
    ],
    imports: [Ash.Resource.Calculation.Builtins, Ash.Filter.TemplateHelpers],
    entities: [
      @calculate
    ]
  }

  @multitenancy %Spark.Dsl.Section{
    name: :multitenancy,
    describe: """
    Options for configuring the multitenancy behavior of a resource.

    To specify a tenant, use `Ash.Query.set_tenant/2` or
    `Ash.Changeset.set_tenant/2` before passing it to an operation.
    """,
    examples: [
      """
      multitenancy do
        strategy :attribute
        attribute :organization_id
        global? true
      end
      """
    ],
    links: [
      guides: [
        "ash:guide:Multitenancy"
      ]
    ],
    schema: [
      strategy: [
        type: {:in, [:context, :attribute]},
        default: :context,
        doc: """
        Determine if multitenancy is performed with attribute filters or using data layer features.
        """,
        links: []
      ],
      attribute: [
        type: :atom,
        doc: """
        If using the `attribute` strategy, the attribute to use, e.g `org_id`
        """,
        links: []
      ],
      global?: [
        type: :boolean,
        doc: """
        Whether or not the data also exists outside of each tenant.
        """,
        default: false,
        links: []
      ],
      parse_attribute: [
        type: :mfa,
        doc:
          "An mfa ({module, function, args}) pointing to a function that takes a tenant and returns the attribute value",
        default: {__MODULE__, :identity, []},
        links: []
      ]
    ]
  }

  @sections [
    @attributes,
    @relationships,
    @actions,
    @code_interface,
    @resource,
    @identities,
    @changes,
    @preparations,
    @validations,
    @aggregates,
    @calculations,
    @multitenancy
  ]

  @transformers [
    Ash.Resource.Transformers.ValidateManagedRelationshipOpts,
    Ash.Resource.Transformers.RequireUniqueActionNames,
    Ash.Resource.Transformers.SetRelationshipSource,
    Ash.Resource.Transformers.BelongsToAttribute,
    Ash.Resource.Transformers.BelongsToSourceField,
    Ash.Resource.Transformers.HasDestinationField,
    Ash.Resource.Transformers.CreateJoinRelationship,
    Ash.Resource.Transformers.CachePrimaryKey,
    Ash.Resource.Transformers.ValidatePrimaryActions,
    Ash.Resource.Transformers.ValidateActionTypesSupported,
    Ash.Resource.Transformers.CountableActions,
    Ash.Resource.Transformers.ValidateMultitenancy,
    Ash.Resource.Transformers.DefaultPrimaryKey,
    Ash.Resource.Transformers.DefaultAccept,
    Ash.Resource.Transformers.RequireUniqueFieldNames,
    Ash.Resource.Transformers.NoReservedFieldNames,
    Ash.Resource.Transformers.ValidateRelationshipAttributes,
    Ash.Resource.Transformers.ValidateEagerIdentities,
    Ash.Resource.Transformers.ValidateAggregatesSupported,
    Ash.Resource.Transformers.ValidateAccept
  ]

  @verifiers [
    Ash.Resource.Verifiers.ValidateRelationshipAttributesMatch
  ]

  @moduledoc """
  The built in resource DSL.
  <!--- ash-hq-hide-start--> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop--> <!--- -->
  """

  use Spark.Dsl.Extension,
    sections: @sections,
    transformers: @transformers,
    verifiers: @verifiers

  @doc false
  def identity(x), do: x
end

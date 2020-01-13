defmodule Ash.Api do
  @using_schema Ashton.schema(
                  opts: [
                    interface?: :boolean,
                    max_page_size: :integer,
                    default_page_size: :integer,
                    # TODO: Support configuring this from env variables
                    authorization_explanations: [:boolean]
                  ],
                  defaults: [
                    interface?: true,
                    max_page_size: 100,
                    default_page_size: 25,
                    authorization_explanations: false
                  ],
                  describe: [
                    interface?:
                      "If set to false, no code interface is defined for this resource e.g `MyApi.create(...)` is not defined.",
                    max_page_size:
                      "The maximum page size for any read action. Any request for a higher page size will simply use this number. Uses the smaller of the Api's or Resource's value.",
                    default_page_size:
                      "The default page size for any read action. If no page size is specified, this value is used. Uses the smaller of the Api's or Resource's value.",
                    authorization_explanations:
                      "A setting that determines whether or not verbose authorization errors should be returned."
                  ],
                  constraints: [
                    max_page_size:
                      {&Ash.Constraints.greater_than_zero?/1, "must be greater than zero"},
                    default_page_size:
                      {&Ash.Constraints.greater_than_zero?/1, "must be greater than zero"}
                  ]
                )

  @moduledoc """
  An Api allows you to interact with your resources, anc holds non-resource-specific configuration.

  Your Api can also house config that is not resource specific.
  Defining a resource won't do much for you. Once you have some resources defined,
  you include them in an Api like so:

  ```elixir
  defmodule MyApp.Api do
    use Ash.Api

    resources [OneResource, SecondResource]
  end
  ```

  Then you can interact through that Api with the actions that those resources expose.
  For example: `MyApp.Api.create(OneResource, %{attributes: %{name: "thing"}})`, or
  `MyApp.Api.read(OneResource, filter: [name: "thing"])`. Corresponding actions must
  be defined in your resources in order to call them through the Api.
  """
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @before_compile Ash.Api

      opts =
        case Ashton.validate(opts, Ash.Api.using_schema()) do
          {:ok, opts} ->
            opts

          {:error, [{key, message} | _]} ->
            raise Ash.Error.ApiDslError,
              using: __MODULE__,
              option: key,
              message: message
        end

      @default_page_size nil
      @max_page_size nil
      @interface? opts[:interface?]
      @side_load_type :simple
      @authorization_explanations opts[:authorization_explanations] || false

      Module.register_attribute(__MODULE__, :mix_ins, accumulate: true)
      Module.register_attribute(__MODULE__, :resources, accumulate: true)
      Module.register_attribute(__MODULE__, :named_resources, accumulate: true)

      import Ash.Api,
        only: [
          resources: 1
        ]
    end
  end

  @doc false
  def using_schema(), do: @using_schema

  defmacro resources(resources) do
    quote do
      Enum.map(unquote(resources), fn resource ->
        case resource do
          {name, resource} ->
            @resources resource
            @named_resources {name, resource}

          resource ->
            @resources resource
        end
      end)
    end
  end

  @parallel_side_load_schema Ashton.schema(
                               opts: [
                                 supervisor: :atom,
                                 max_concurrency: :integer,
                                 timeout: :integer,
                                 shutdown: :integer
                               ],
                               required: [:supervisor],
                               defaults: [
                                 timeout: 5000,
                                 shutdown: 5000
                               ],
                               describe: [
                                 supervisor:
                                   "The name of the task supervisor that will supervise the parallel side loading tasks.",
                                 max_concurrency:
                                   "The total number of side loads (per side load nesting level, per ongoing side load) that can be running. Uses `System.schedulers_online/0` if unset.",
                                 timeout:
                                   "the maximum amount of time to wait (in milliseconds) without receiving a task reply. see `task.supervisor.async_stream/6`",
                                 shutdown:
                                   "an integer indicating the timeout value. Defaults to 5000 milliseconds"
                               ],
                               constraints: [
                                 max_concurrency:
                                   {&Ash.Constraints.greater_than_zero?/1,
                                    "must be greater than zero"},
                                 timeout: {&Ash.Constraints.positive?/1, "must be positive"},
                                 shutdown: {&Ash.Constraints.positive?/1, "must be positive"}
                               ]
                             )

  defmacro __before_compile__(env) do
    quote generated: true do
      def default_page_size(), do: @default_page_size
      def max_page_size(), do: @max_page_size
      def mix_ins(), do: @mix_ins
      def resources(), do: @resources
      def side_load_config(), do: {@side_load_type, @side_load_config}
      def authorization_explanations(), do: @authorization_explanations

      def get_resource(mod) when mod in @resources, do: {:ok, mod}

      def get_resource(name) do
        Keyword.fetch(@named_resources, name)
      end

      if @interface? do
        use Ash.Api.Interface
      end

      Enum.map(@mix_ins, fn hook_module ->
        code = hook_module.before_compile_hook(unquote(Macro.escape(env)))
        Module.eval_quoted(__MODULE__, code)
      end)
    end
  end
end

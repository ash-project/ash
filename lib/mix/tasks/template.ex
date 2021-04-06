defmodule Ash.Template do
  import Mix.Tasks.Ash.Gen.Resource,
    only: [require_package: 0, valid_attributes: 0, get_template_strings: 3]

  alias Mix.Tasks.Ash.Helpers

  @callback resource_template(map) :: String.t()
  @callback guide_template(map) :: String.t()
  @callback shell_template(map) :: String.t()

  @spec resource_template(atom, map) :: String.t()
  def resource_template(:csv, assigns) do
    """
    csv do
    file "priv/data/#{assigns.table_name}.csv"
      create? true
      header? true
      separator '-'
    columns [#{for {attribute, _} <- assigns.attributes, do: ~s(:#{attribute},)}]
    end
    """
  end

  def resource_template(:json_api, assigns) do
    """
    json_api do
      routes do
        base "/#{assigns.name}"
        get :default
        index :default
      end
    end
    """
  end

  def resource_template(:policy_authorizer, _assigns) do
    """
    policies do
    end
    """
  end

  def resource_template(:postgres, assigns) do
    """
    postgres do
      repo #{assigns.project_name}.Repo
      table "#{assigns.table_name}"
    end
    """
  end

  def resource_template(_, _assigns), do: ""

  @spec guide_template(atom, map) :: String.t()
  def guide_template(:graphql, _assigns) do
    """
    graphql do
      type :post

      queries do
        get :get_post, :read
        list :list_posts, :read
      end

      mutations do
        create :create_post, :create
        update :update_post, :update
        destroy :destroy_post, :destroy
      end
    end
    """
  end

  def guide_template(:json_api, assigns) do
    """
    json_api do
      routes do
        base "/#{assigns.name}"
        # Add a `GET /#{assigns.name}/:id` route, that calls into the :read action called :default
        # Add a `GET /#{assigns.name}` route, that calls into the :read action called :default
        get :default
        index :default
      end
    end
    """
  end

  def guide_template(:policy_authorizer, _assigns) do
    """
    # policies do
    #   # Anything you can use in a condition, you can use in a check, and vice-versa
    #   # This policy applies if the actor is a super_user
    #   # Addtionally, this policy is declared as a `bypass`. That means that this check is allowed to fail without
    #   # failing the whole request, and that if this check *passes*, the entire request passes.
    #   bypass actor_attribute_equals(:super_user, true) do
    #     authorize_if always()
    #   end

    #   # This will likely be a common occurrence. Specifically, policies that apply to all read actions
    #   policy action_type(:read) do
    #     # unless the actor is an active user, forbid their request
    #     forbid_unless actor_attribute_equals(:active, true)
    #     # if the record is marked as public, authorize the request
    #     authorize_if attribute(:public, true)
    #     # if the actor is related to the data via that data's `owner` relationship, authorize the request
    #     authorize_if relates_to_actor_via(:owner)
    #   end
    # end
    """
  end

  def guide_template(:postgres, assigns) do
    """
    # postgres do
    #   repo #{assigns.project_name}.Repo
    #   table #{assigns.table_name}
    # end
    """
  end

  def guide_template(_, _assigns), do: ""

  @spec print_info([{atom, any}]) :: nil
  def print_info(cmd_switches) do
    print_dependencies(cmd_switches)
    print_required_changes(cmd_switches)
  end

  def print_resource_name_missing_info() do
    Mix.shell().info("""
    Please specify resource name eg.
    mix ash.gen.resource users name age integer born date
    """)

    :error_missing_resource
  end

  def print_missing_attributes() do
    Mix.shell().info("""
      You have not entered any column types for your resource
      mix ash.gen.resource user name string age integer
      where valid column types are:
    #{valid_attributes() |> Enum.map(&inspect/1) |> Enum.join("\n")}
    """)

    :error_missing_columns
  end

  def print_invalid_param_info(list_of_invalid_params) do
    Mix.shell().info("""
    You entered invalid params:
    #{list_of_invalid_params |> Enum.map(&inspect/1) |> Enum.join("\n")}
    remember to use '-' instead of '_' on command line parameters
    """)

    :error_invalid_attribute
  end

  def print_resource_info(resource, context) when is_nil(context) do
    Mix.shell().info("""
    Please add your resource to #{Helpers.api_file_name(resource)}

    resources do
      ...
      resource #{Helpers.project_module_name()}.#{Helpers.capitalize(resource)}
    end
    """)
  end

  def print_resource_info(resource, context_name) do
    context_name = Helpers.capitalize(context_name)

    Mix.shell().info("""
    Please add your resource to #{Helpers.api_file_name(resource, true)}

    resources do
      ...
      resource #{Helpers.project_module_name()}.#{context_name}.#{Helpers.capitalize(resource)}
    end
    """)
  end

  def print_required_changes(cmd_switches) do
    shell_info =
      get_template_strings(cmd_switches, :shell_template, %{})
      |> Enum.map(fn {_name, str} -> str end)
      |> Enum.filter(fn str -> is_bitstring(str) end)
      |> Enum.join("\n ###### \n")

    if Enum.count(cmd_switches) > 0 do
      Mix.shell().info("""
      Ensure you made these changes to your app:

        #{shell_info}

      """)
    end
  end

  @spec print_dependencies([{atom, any}]) :: nil
  def print_dependencies(cmd_switches) do
    cmd_switches =
      cmd_switches
      |> Enum.map(&dependency_template/1)
      |> Enum.filter(&if &1, do: true, else: false)

    if Enum.count(cmd_switches) > 0 do
      Mix.shell().info("""
      Ensure you've added dependencies to your mix.exs file.
      def deps do
      [
      ...

      #{Enum.join(cmd_switches, ",\n    ")}
      ]
      end
      """)
    end
  end

  @spec dependency_template({atom, any}) :: String.t() | nil
  def dependency_template({dependency, true}) do
    # TODO get dependency from hex
    if dependency in require_package() do
      ~s({:ash_#{dependency}, "~> x.y.z"})
    end
  end

  def dependency_template({_dependency, _}), do: nil

  @spec shell_template(atom, map) :: String.t()
  def shell_template(:graphql, _assigns) do
    """
    You can add graphgl configuration to your main context file

    graphql do
      authorize? false # To skip authorization for this API
    end
    """
  end

  def shell_template(:postgres, _assigns) do
    """
    Make sure you ran migrations

    mix ash_postgres.generate_migrations
    """
  end

  def shell_template(:json_api, _assigns) do
    """
    add json api extension to your api file

    defmodule MyApp.Api do
      use Ash.Api, extensions: [AshJsonApi.Api]

      ...

      json_api do
        prefix "/json_api"
        serve_schema? true
        log_errors? true
      end
    end

    This configuration is required to support working with the JSON:API custom mime type.
    Edit your config/config.exs
    config :mime, :types, %{
      "application/vnd.api+json" => ["json"]
    }

    Add the routes from your API module(s)
    In your router, use AshJsonApi.forward/2.

    For example:

    scope "/json_api" do
      pipe_through(:api)

      AshJsonApi.forward("/helpdesk", Helpdesk.Helpdesk.Api)
    end
    """
  end

  def shell_template(_, _assigns), do: nil
end

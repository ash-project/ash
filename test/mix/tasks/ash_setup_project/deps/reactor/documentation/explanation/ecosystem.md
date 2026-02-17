<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Reactor in the Elixir Ecosystem

Reactor is a framework-independent, dynamic, concurrent, dependency-resolving saga orchestrator for Elixir. While it's part of the ash-project organization and has excellent Ash framework integration, Reactor is designed to work with any Elixir application and can orchestrate workflows across diverse systems and frameworks.

## Framework Independence

### Core Design Philosophy

Reactor's architecture is deliberately framework-agnostic:

- **No Framework Dependencies**: Reactor's core has no hard dependencies on web frameworks, data layers, or specific libraries
- **Generic Step Interface**: The `Reactor.Step` behaviour works with any Elixir code
- **Composable Architecture**: Steps can integrate with any Elixir libraries or external systems
- **Universal Patterns**: Dependency resolution, error handling, and compensation work regardless of the underlying technology stack

```elixir
# Reactor works with any Elixir code
defmodule GenericWorkflowReactor do
  use Reactor

  input :data

  step :process_with_genserver do
    argument :data, input(:data)
    run fn args, _context ->
      GenServer.call(MyService, {:process, args.data})
    end
  end

  step :call_external_api do
    argument :processed_data, result(:process_with_genserver)
    run fn args, _context ->
      HTTPoison.post("https://api.example.com", args.processed_data)
    end
  end

  step :store_in_ets do
    argument :api_response, result(:call_external_api)
    run fn args, _context ->
      :ets.insert(:my_table, {:result, args.api_response})
      {:ok, :stored}
    end
  end

  return :store_in_ets
end
```

## Reactor Ecosystem Packages

The Reactor ecosystem includes several specialized packages that extend its capabilities:

### Core Ecosystem Packages

**[reactor_file](https://hex.pm/packages/reactor_file)**
- File system operations within reactor workflows
- File copying, moving, and manipulation steps
- Directory creation/removal, permissions, and file I/O operations

**[reactor_process](https://hex.pm/packages/reactor_process)**
- Supervisor and process management operations
- Start, restart, terminate, and delete child processes
- Process supervision and lifecycle management within workflows

**[reactor_req](https://hex.pm/packages/reactor_req)**
- HTTP client steps for making web requests
- DSL support for GET, POST, PUT, DELETE, and other HTTP methods
- Integration with the Req library for structured HTTP operations

### Extension Pattern

Ecosystem packages provide additional DSL entities that become available when you include them as extensions:

```elixir
# Example ecosystem integration
defmodule MyWorkflowReactor do
  use Reactor, extensions: [Reactor.Req, Reactor.File, Reactor.Process]
  
  input :file_path
  input :upload_url
  
  # File operations from reactor_file (DSL entities)
  file_read :process_file do
    path input(:file_path)
  end
  
  # HTTP requests from reactor_req (DSL entities)
  req_post :upload_data do
    url input(:upload_url)
    body result(:process_file)
  end
  
  # Process management from reactor_process (DSL entities)
  start_child :start_worker do
    supervisor value(MyApp.WorkerSupervisor) 
    child_spec template({MyApp.Worker, result(:process_file)})
  end
  
  return :upload_data
end
```

## Ash Framework Integration

### Ash.Reactor Extension

While Reactor is framework-independent, the **[Ash.Reactor](https://hexdocs.pm/ash/Ash.Reactor.html)** extension provides deep integration with Ash resources and actions:

```elixir
defmodule UserOnboardingReactor do
  # Using the Ash.Reactor extension
  use Ash.Reactor

  input :user_params

  # Ash-specific create action step
  create :user, MyApp.User, :create do
    inputs %{
      email: input(:user_params, [:email]),
      name: input(:user_params, [:name])
    }
  end

  # Ash-specific update action step  
  update :activate_user, MyApp.User, :activate do
    record result(:user)
    inputs %{activated_at: value(DateTime.utc_now())}
  end

  # Ash action with automatic undo support
  action :send_welcome_email, MyApp.Notifications, :send_welcome do
    inputs %{user_id: result(:user, :id)}
    undo :outside_transaction
  end

  return :activate_user
end
```

### Ash-Specific Capabilities

**Resource Action Integration:**
- Direct integration with Ash resource actions (create, read, update, destroy, and generic actions)
- Reactor modules can serve as the `run` implementation for Ash generic actions
- Bidirectional integration: Reactor can orchestrate Ash actions, and Ash actions can use Reactor for implementation
- Automatic changeset handling and validation
- Built-in support for Ash's authorization policies

**Transaction Management:**
- Automatic database transaction coordination
- Rollback capabilities integrated with Ash's transaction handling
- Undo strategies that respect Ash's data layer behaviour

**Notification Handling:**
- Proper integration with Ash's notification system

### When to Use Ash.Reactor vs Core Reactor

**Use Ash.Reactor when:**
- Working primarily with Ash resources and actions
- Need automatic changeset validation and error handling
- Require database transaction coordination
- Want built-in authorization policy integration

**Use Core Reactor when:**
- Working with non-Ash systems and libraries
- Building framework-agnostic workflows
- Integrating with existing Elixir applications
- Need maximum flexibility and minimal dependencies

## Relationship to Other Elixir Tools

### vs. GenServer and OTP Patterns

**Reactor Strengths:**
- Declarative workflow definition with automatic dependency resolution
- Built-in compensation patterns for error recovery
- Multi-system coordination and state management

**OTP Strengths:**
- Long-running stateful processes
- Real-time systems requiring immediate response
- Performance-critical operations with direct supervision tree integration

### vs. Broadway for Data Processing

**Reactor Use Cases:**
- Complex workflows with interdependent steps and compensation needs
- Multi-system coordination requiring state management
- Business processes needing human intervention or approval
- Processing individual messages or batches within Broadway pipelines

**Broadway Use Cases:**
- High-throughput stream processing with simple transforms
- Message queue consumption with batching and linear processing

### vs. Task and Async Patterns

**Reactor Advantages:**
- Automatic dependency resolution and sophisticated error handling
- Declarative composition with state management across operations
- Shared concurrency pools that prevent resource exhaustion

**Task/Async Advantages:**
- Minimal overhead with direct process control
- Simpler model for basic parallelisation and performance-critical paths

## Integration Examples

### Multi-Framework Workflow

```elixir
defmodule E2EProcessingReactor do
  use Reactor

  input :upload_params

  # Phoenix file upload handling
  step :handle_upload do
    argument :params, input(:upload_params)
    run &MyAppWeb.UploadController.process_upload/2
  end

  # Ash resource operations (if using Ash.Reactor)
  step :store_metadata do
    argument :file_info, result(:handle_upload)
    run fn args, _context ->
      MyApp.FileMetadata.create(args.file_info)
    end
  end

  # External service integration
  step :process_with_ai do
    argument :file_path, result(:handle_upload, [:path])
    run fn args, _context ->
      HTTPoison.post("https://ai-service.com/process", %{
        file_url: args.file_path
      })
    end
  end

  # Background job scheduling
  step :schedule_cleanup do
    argument :file_path, result(:handle_upload, [:path])
    run fn args, _context ->
      Oban.insert(CleanupWorker.new(%{file_path: args.file_path}))
    end
  end

  return :store_metadata
end
```

### Bidirectional Ash Integration

```elixir
# Reactor as Ash action implementation
defmodule MyApp.UserRegistrationReactor do
  use Ash.Reactor

  input :email
  input :password
  input :name

  create :user, MyApp.User, :create do
    inputs %{
      email: input(:email),
      name: input(:name)
    }
  end

  action :send_welcome_email, MyApp.Notifications, :send_welcome do
    inputs %{user_id: result(:user, :id)}
  end

  return :user
end

# Ash resource using Reactor as action implementation
defmodule MyApp.User do
  use Ash.Resource

  actions do
    create :create do
      argument :email, :string, allow_nil?: false
      argument :password, :string, allow_nil?: false
      argument :name, :string, allow_nil?: false
    end

    # Reactor module as generic action implementation
    action :complete_registration, :struct do
      argument :email, :string, allow_nil?: false
      argument :password, :string, allow_nil?: false  
      argument :name, :string, allow_nil?: false
      
      run MyApp.UserRegistrationReactor
    end
  end
end
```

### Ecosystem Package Integration

```elixir
defmodule DocumentProcessingReactor do
  use Reactor, extensions: [Reactor.Req, Reactor.File, Reactor.Process]

  input :document_url
  input :processing_config

  # HTTP download with reactor_req
  req_get :download_document do
    url input(:document_url)
  end

  # File processing with reactor_file
  file_write :save_to_temp do
    path template("/tmp/{{uuid}}.pdf")
    content result(:download_document, [:body])
  end

  # External processing with reactor_process
  start_child :convert_to_text do
    supervisor value(MyApp.ProcessSupervisor)
    child_spec template({PDFConverter, [result(:save_to_temp, [:path])]})
  end

  # Cleanup with compensation
  file_delete :cleanup_temp_file do
    path result(:save_to_temp, [:path])
    wait_for :convert_to_text
  end

  return :convert_to_text
end
```

## When to Choose Reactor

### Decision Framework

**Choose Reactor when your workflow has:**

1. **Complex Dependencies**: Multi-step processes where operations depend on results from previous steps
2. **Error Recovery Needs**: Sophisticated compensation, rollback, or cleanup requirements
3. **Cross-System Coordination**: Integration spanning multiple services, APIs, or external systems
4. **State Management Requirements**: Need to track intermediate results and context across operations
5. **Declarative Benefits**: Complex logic that benefits from clear, maintainable specifications

**Specific use cases include:**
- Financial transactions requiring compensation (payment processing, refunds)
- Multi-stage business processes (user onboarding, order fulfilment)
- Data pipelines with validation and transformation steps
- API orchestration across microservices
- Resource provisioning with cleanup on failure

**Choose alternatives when:**
- **High Performance**: Maximum throughput with minimal overhead is critical
- **Simple Operations**: Straightforward CRUD or linear processing without dependencies
- **Real-Time Constraints**: Immediate response requirements (use OTP patterns)
- **Stream Processing**: High-throughput data transformation (use Broadway)

## Community and Ecosystem

### Learning Resources

**Framework-Independent Learning:**
- Core Reactor patterns apply across any Elixir application
- Ecosystem packages provide specialized capabilities
- Community patterns and best practices work universally

**Ash-Specific Learning:**
- [Ash Reactor guide](https://hexdocs.pm/ash/reactor.html) for framework integration
- [Ash community Discord](https://discord.gg/3hA2j4Jt) for specific integration questions
- Combined Ash + Reactor tutorials and examples

### Future Evolution

Reactor's framework-independent design makes it adaptable to new technologies and integration requirements. The ecosystem could potentially grow in many directions:

**Possible Integration Areas:**
- Database-specific integrations beyond Ash
- Cloud service orchestration packages
- Monitoring and observability extensions
- Testing and development tools

**Framework Integration Opportunities:**
- Phoenix LiveView workflow coordination
- Nerves IoT device orchestration  
- Machine learning pipeline management
- Blockchain and distributed system coordination

**Contributing to the Ecosystem:**
What integrations would be valuable for your use cases? The Reactor community welcomes contributions that extend its capabilities while maintaining its core strengths in workflow orchestration, dependency resolution, and error handling. Whether used standalone or with framework-specific extensions like Ash.Reactor, it provides a solid foundation for building reliable, maintainable business workflows.

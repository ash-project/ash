<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Error Handling with Compensation and Undo

In this tutorial, you'll learn how to make your reactors resilient by adding proper error handling, retry logic, and rollback capabilities.

## What you'll build

You'll enhance the user registration workflow from the first tutorial to handle:
1. Network failures with automatic retries
2. Validation errors with graceful failure
3. Rollback scenarios when later steps fail
4. Email service failures with compensation

## You'll learn

- Implementing compensation for retryable errors
- Adding undo logic for rollback scenarios
- The difference between compensation and undo
- Building resilient workflows that handle failures gracefully

## Error Handling Flow

Here's how Reactor handles errors through compensation and undo:

```mermaid
sequenceDiagram
    participant Reactor
    participant StepA
    participant StepB
    participant StepC
    
    Reactor->>StepA: run()
    StepA-->>Reactor: {:ok, result}
    
    Reactor->>StepB: run()
    StepB-->>Reactor: {:ok, result}
    
    Reactor->>StepC: run()
    StepC-->>Reactor: {:error, reason}
    
    Note over Reactor: Begin compensation
    Reactor->>StepB: compensate()
    StepB-->>Reactor: {:continue, context}
    
    Reactor->>StepA: compensate()
    StepA-->>Reactor: {:continue, context}
    
    Reactor-->>Reactor: Return compensated error
```

## Prerequisites

- Complete the [Getting Started tutorial](01-getting-started.md)
- Basic knowledge of Elixir error handling

## Step 1: Set up the project

If you don't have the project from the previous tutorial, create it:

```bash
mix igniter.new reactor_tutorial --install reactor
cd reactor_tutorial
```

## Step 2: Understanding Reactor error handling

Reactor provides two main mechanisms for error handling:

### Compensation
**When**: A step fails during execution  
**Purpose**: Decide whether to retry, continue, or fail the reactor  
**Return values**:
- `:retry` - Try the step again
- `{:continue, value}` - Continue execution with the provided value
- `:ok` - Successfully compensated, but still triggers rollback
- `{:error, reason}` - Fail the entire reactor

### Undo
**When**: A step succeeded but a later step failed  
**Purpose**: Roll back the successful step's changes  
**Return values**:
- `:ok` - Successfully undone
- `{:error, reason}` - Failed to undo (this will fail the reactor)

## Step 3: Create services with realistic error handling

Let's create services that demonstrate different types of failures. Create `lib/email_service.ex`:

```elixir
defmodule EmailService do
  use Reactor.Step

  # Simulate realistic email service failures based on email content
  @impl true
  def run(arguments, _context, _options) do
    email = arguments.email
    
    cond do
      # Simulate network timeout (temporary failure)
      String.contains?(email, "timeout") ->
        {:error, %{type: :network_timeout, message: "Network timeout - please retry"}}
      
      # Simulate rate limiting (temporary failure)  
      String.contains?(email, "ratelimit") ->
        {:error, %{type: :rate_limit, message: "Rate limit exceeded - please retry"}}
      
      # Simulate blocked email (permanent failure)
      String.contains?(email, "blocked") ->
        {:error, %{type: :blocked_email, message: "Email address is blocked"}}
      
      # Simulate invalid email (permanent failure)
      not String.contains?(email, "@") ->
        {:error, %{type: :invalid_email, message: "Invalid email format"}}
      
      # Success case - all other emails work
      true ->
        {:ok, %{
          message_id: "msg_#{:rand.uniform(10000)}", 
          sent_at: DateTime.utc_now(),
          recipient: email
        }}
    end
  end

  @impl true
  def compensate(error, _arguments, _context, _options) do
    case error do
      # Temporary failures - retry with helpful logging
      %{type: :network_timeout} -> 
        IO.puts("🔄 Network timeout - retrying email send...")
        :retry
      
      %{type: :rate_limit} -> 
        IO.puts("🔄 Rate limited - retrying email send...")
        :retry
      
      # Permanent failures - don't retry
      %{type: :blocked_email} -> 
        IO.puts("❌ Email blocked - cannot retry")
        :ok
      
      %{type: :invalid_email} -> 
        IO.puts("❌ Invalid email - cannot retry")
        :ok
      
      _other -> 
        :ok
    end
  end

  @impl true
  def undo(result, _arguments, _context, _options) do
    IO.puts("📧 Canceling email #{result.message_id} to #{result.recipient}")
    :ok
  end
end
```

Now create `lib/notification_service.ex` for internal admin notifications:

```elixir
defmodule NotificationService do
  use Reactor.Step

  @impl true
  def run(arguments, _context, _options) do
    user = arguments.user
    
    # Admin notifications always succeed (internal system)
    {:ok, %{
      notification_id: "notif_#{:rand.uniform(10000)}",
      sent_at: DateTime.utc_now(),
      message: "New user registered: #{user.email}"
    }}
  end

  @impl true
  def undo(result, _arguments, _context, _options) do
    IO.puts("🔔 Canceling admin notification #{result.notification_id}")
    :ok
  end
end
```

## Step 4: Create a database service that needs rollback

Create `lib/database_service.ex`:

```elixir
defmodule DatabaseService do
  use Reactor.Step

  @impl true
  def run(arguments, _context, _options) do
    user = %{
      id: :rand.uniform(10000),
      email: arguments.email,
      password_hash: arguments.password_hash,
      created_at: DateTime.utc_now()
    }
    
    {:ok, user}
  end

  @impl true
  def compensate(_error, _arguments, _context, _options) do
    # Database errors are usually retryable
    :retry
  end

  @impl true
  def undo(user, _arguments, _context, _options) do
    IO.puts("Rolling back user creation for #{user.email} (ID: #{user.id})")
    :ok
  end
end
```

## Step 5: Build a reactor with error handling

Now create `lib/resilient_user_registration.ex`:

```elixir
defmodule ResilientUserRegistration do
  use Reactor

  input :email
  input :password

  step :validate_email do
    argument :email, input(:email)
    
    run fn %{email: email}, _context ->
      if String.contains?(email, "@") and String.length(email) > 5 do
        {:ok, email}
      else
        {:error, "Email must contain @ and be longer than 5 characters"}
      end
    end
  end

  step :hash_password do
    argument :password, input(:password)
    
    run fn %{password: password}, _context ->
      if String.length(password) >= 8 do
        hashed = :crypto.hash(:sha256, password) |> Base.encode16()
        {:ok, hashed}
      else
        {:error, "Password must be at least 8 characters"}
      end
    end
  end

  step :create_user, DatabaseService do
    argument :email, result(:validate_email)
    argument :password_hash, result(:hash_password)
    max_retries 3
  end

  step :send_welcome_email, EmailService do
    argument :email, result(:validate_email)
    argument :user, result(:create_user)
    max_retries 2
  end

  step :send_admin_notification, NotificationService do
    argument :user, result(:create_user)
    max_retries 1
  end

  return :create_user
end
```

## Step 6: Test the error handling

Let's test our reactor in IEx:

```bash
iex -S mix
```

```elixir
# ✅ SUCCESS: Normal email succeeds
{:ok, user} = Reactor.run(ResilientUserRegistration, %{
  email: "alice@example.com",
  password: "secretpassword123"
})

# 🔄 RETRY: Network timeout triggers retry logic
{:error, reason} = Reactor.run(ResilientUserRegistration, %{
  email: "timeout@example.com",  # Will trigger network timeout
  password: "secretpassword123"
})

# 🔄 RETRY: Rate limiting triggers retry logic  
{:error, reason} = Reactor.run(ResilientUserRegistration, %{
  email: "ratelimit@example.com",  # Will trigger rate limit
  password: "secretpassword123"
})

# ❌ PERMANENT FAILURE: Blocked email fails immediately
{:error, reason} = Reactor.run(ResilientUserRegistration, %{
  email: "blocked@example.com",  # Will fail permanently
  password: "secretpassword123"
})

# ❌ VALIDATION FAILURE: Invalid inputs fail immediately
{:error, reason} = Reactor.run(ResilientUserRegistration, %{
  email: "invalid-email",  # No @ symbol
  password: "short"        # Too short
})
```

## Step 7: Understanding the behaviour

When you run the tests, you'll see different behaviours based on the email content:

**Successful execution** (`alice@example.com`): All steps succeed, user is created, welcome email is sent, and admin notification is sent.

**Retry scenarios**:
- `timeout@example.com` - Triggers network timeout, compensation returns `:retry`, step retries up to max_retries limit
- `ratelimit@example.com` - Triggers rate limiting, compensation returns `:retry`, step retries up to max_retries limit

**Permanent failures**:
- `blocked@example.com` - Email is blocked, compensation returns `:ok` (no retry)
- `invalid-email` - Invalid format, compensation returns `:ok` (no retry)

**Validation failures**: Invalid input (short passwords, malformed emails) fails immediately without retries - these are caught by the validation steps before reaching the email service.

## Step 8: Adding retry backoff for better resilience

When steps retry immediately, they might overwhelm failing external services. Reactor supports **backoff** - adding delays between retry attempts. Importantly, the executor doesn't block during backoff - it continues processing other ready steps while the failed step waits to be rescheduled.

> #### Note {: .info}
>
> Backoff delays are **minimum delays** - the actual retry time will be at least the specified delay, but may be longer because the executor prioritises processing other ready steps before checking for expired backoffs. Let's enhance our email service with intelligent retry delays.

### Understanding backoff timing

Here's how backoff integrates with Reactor's retry flow:

```mermaid
sequenceDiagram
    participant Executor
    participant Step
    participant BackoffStep as Step.backoff/4
    participant Scheduler
    participant OtherSteps as Other Ready Steps

    Executor->>Step: run()
    Step-->>Executor: {:error, reason}

    Note over Executor: Begin compensation
    Executor->>Step: compensate()
    Step-->>Executor: :retry

    Note over Executor: Check for backoff
    Executor->>BackoffStep: backoff(error, args, context, step)
    BackoffStep-->>Executor: 5000 (5 seconds)

    Note over Executor: Schedule retry after delay
    Executor->>Scheduler: schedule step retry in 5000ms

    Note over Executor: Continue with other work
    Executor->>OtherSteps: process ready steps
    OtherSteps-->>Executor: results

    Note over Scheduler: 5 seconds later...
    Scheduler-->>Executor: step ready for retry

    Note over Executor: Retry now ready
    Executor->>Step: run()
    Step-->>Executor: {:ok, result}
```

### Implementing backoff strategies

Update the `EmailService` to include backoff logic:

```elixir
defmodule EmailService do
  use Reactor.Step

  @impl true
  def run(arguments, _context, _options) do
    email = arguments.email

    cond do
      String.contains?(email, "timeout") ->
        {:error, %{type: :network_timeout, message: "Network timeout - please retry"}}

      String.contains?(email, "ratelimit") ->
        {:error, %{type: :rate_limit, message: "Rate limit exceeded - please retry"}}

      String.contains?(email, "blocked") ->
        {:error, %{type: :blocked_email, message: "Email address is blocked"}}

      not String.contains?(email, "@") ->
        {:error, %{type: :invalid_email, message: "Invalid email format"}}

      true ->
        {:ok, %{
          message_id: "msg_#{:rand.uniform(10000)}",
          sent_at: DateTime.utc_now(),
          recipient: email
        }}
    end
  end

  @impl true
  def compensate(error, _arguments, _context, _options) do
    case error do
      %{type: :network_timeout} ->
        IO.puts("🔄 Network timeout - retrying email send...")
        :retry

      %{type: :rate_limit} ->
        IO.puts("🔄 Rate limited - retrying email send...")
        :retry

      %{type: :blocked_email} ->
        IO.puts("❌ Email blocked - cannot retry")
        :ok

      %{type: :invalid_email} ->
        IO.puts("❌ Invalid email - cannot retry")
        :ok

      _other ->
        :ok
    end
  end

  # NEW: Backoff implementation
  @impl true
  def backoff(error, _arguments, context, _options) do
    case error do
      %{type: :network_timeout} ->
        # Exponential backoff for network issues
        retry_count = Map.get(context, :current_try, 0)
        delay_ms = :math.pow(2, retry_count) * 1000 |> round() |> min(30_000)
        IO.puts("⏰ Network timeout - backing off for #{delay_ms}ms")
        delay_ms

      %{type: :rate_limit} ->
        # Longer fixed delay for rate limiting
        delay_ms = 10_000  # 10 seconds
        IO.puts("⏰ Rate limited - backing off for #{delay_ms}ms")
        delay_ms

      _other ->
        # No backoff for non-retryable errors
        :now
    end
  end

  @impl true
  def undo(result, _arguments, _context, _options) do
    IO.puts("📧 Canceling email #{result.message_id} to #{result.recipient}")
    :ok
  end
end
```

### Using backoff in DSL steps

You can also define backoff logic directly in DSL steps when using anonymous functions for `run`, `compensate`, etc. (The DSL `backoff` option is not available when using implementation modules):

```elixir
defmodule BackoffUserRegistration do
  use Reactor

  input :email
  input :password

  step :validate_email do
    argument :email, input(:email)

    run fn %{email: email}, _context ->
      if String.contains?(email, "@") and String.length(email) > 5 do
        {:ok, email}
      else
        {:error, "Email must contain @ and be longer than 5 characters"}
      end
    end
  end

  step :hash_password do
    argument :password, input(:password)

    run fn %{password: password}, _context ->
      if String.length(password) >= 8 do
        hashed = :crypto.hash(:sha256, password) |> Base.encode16()
        {:ok, hashed}
      else
        {:error, "Password must be at least 8 characters"}
      end
    end
  end

  step :create_user do
    argument :email, result(:validate_email)
    argument :password_hash, result(:hash_password)
    max_retries 3

    run fn %{email: email, password_hash: hash}, _context ->
      user = %{
        id: :rand.uniform(10000),
        email: email,
        password_hash: hash,
        created_at: DateTime.utc_now()
      }
      {:ok, user}
    end

    compensate fn _error, _args, _context ->
      :retry  # Database errors are usually retryable
    end

    # DSL backoff function (only available with anonymous run functions)
    backoff fn _error, _args, context ->
      retry_count = Map.get(context, :current_try, 0)
      # Exponential backoff: 1s, 2s, 4s, 8s...
      delay = :math.pow(2, retry_count) * 1000 |> round()
      IO.puts("🔄 Database retry #{retry_count + 1} - waiting #{delay}ms")
      delay
    end
  end

  step :send_welcome_email, EmailService do
    argument :email, result(:validate_email)
    argument :user, result(:create_user)
    max_retries 3
    # EmailService module has its own backoff/4 callback
  end

  step :send_admin_notification, NotificationService do
    argument :user, result(:create_user)
    max_retries 1
  end

  return :create_user
end
```

### Testing backoff behaviour

Test the improved retry behaviour:

```elixir
# This will now retry with exponential backoff delays
{:error, reason} = Reactor.run(BackoffUserRegistration, %{
  email: "timeout@example.com",  # Triggers network timeout with backoff
  password: "secretpassword123"
})

# Watch the console output:
# 🔄 Network timeout - retrying email send...
# ⏰ Network timeout - backing off for 1000ms
# (1 second delay)
# 🔄 Network timeout - retrying email send...
# ⏰ Network timeout - backing off for 2000ms
# (2 second delay)
# 🔄 Network timeout - retrying email send...
# ⏰ Network timeout - backing off for 4000ms
# (4 second delay - final retry)
```

### Backoff strategies explained

**Exponential backoff**: Doubles delay each retry (1s, 2s, 4s, 8s...) - good for network issues and service overload.

**Fixed backoff**: Same delay each time - good for rate limiting where you know the reset interval.

**No backoff**: Use `:now` for errors that don't benefit from delays.

**Custom strategies**: Implement any timing logic based on error type, retry count, or external factors.


## What you learned

You now understand Reactor's error handling mechanisms:

- **[Compensation](../reference/glossary.md#compensation)** handles step failures with retry logic
- **Backoff strategies** add intelligent delays between retry attempts to prevent overwhelming services
- **Undo operations** roll back successful steps when later steps fail
- **Max retries** controls how many times compensation can retry a step
- **Error types** should be handled differently (retry vs fail, with or without backoff)
- **Context contains retry state** for intelligent retry and backoff logic
- **DSL backoff functions** allow inline backoff logic without full step modules

## What's next

Now that you can handle errors, you're ready for more advanced concepts:

- **[Async Workflows](03-async-workflows.md)** - Explore concurrent processing patterns
- **[Composition](04-composition.md)** - Build complex workflows with sub-reactors  
- **[Testing Strategies](documentation/how-to/testing-strategies.md)** - Learn how to test error scenarios

## Common issues

**Steps retry infinitely**: Always set `max_retries` and ensure compensation doesn't always return `:retry`

**Undo operations fail**: Make undo operations idempotent - they should succeed even if called multiple times

**Reactor fails instead of retrying**: Check that your compensation function returns `:retry`, not `{:error, reason}`

Happy building resilient workflows! 🛡️

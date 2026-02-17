<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# How to Test Reactors and Steps

## Problem

You need comprehensive testing strategies for your reactors, including unit tests for individual steps, integration tests for complete workflows, and proper testing of error handling and compensation logic.

## Solution Overview

This guide shows you different approaches to testing reactors, from testing individual step modules to full workflow integration tests. We'll cover unit testing, integration testing, error scenario testing, and concurrent execution testing.

## Prerequisites

- Understanding of Reactor basics
- Familiarity with ExUnit testing
- Basic knowledge of mocking with Mimic (for advanced scenarios)

## Testing Strategies

### 1. Unit Testing Individual Steps

The most granular level is testing individual step modules directly.

#### Basic Step Testing

```elixir
defmodule MyApp.Steps.ValidateEmailTest do
  use ExUnit.Case, async: true
  
  alias MyApp.Steps.ValidateEmail
  
  test "validates correct email format" do
    arguments = %{email: "user@example.com"}
    context = %{}
    options = []
    
    assert {:ok, "user@example.com"} = ValidateEmail.run(arguments, context, options)
  end
  
  test "returns error for invalid email" do
    arguments = %{email: "invalid-email"}
    context = %{}
    options = []
    
    assert {:error, %ArgumentError{}} = ValidateEmail.run(arguments, context, options)
  end
end
```

#### Testing Anonymous Function Steps

Instead of testing anonymous functions inline, extract them to public functions that can be unit tested:

```elixir
defmodule MyApp.UserReactor do
  use Reactor
  
  input :name
  
  step :greet do
    argument :name, input(:name)
    run &greet_user/2
  end
  
  def greet_user(%{name: name}, _context) do
    {:ok, "Hello, #{name}!"}
  end
end

defmodule MyApp.UserReactorTest do
  use ExUnit.Case, async: true
  
  test "greet_user formats greeting correctly" do
    assert {:ok, "Hello, Marty!"} = 
      MyApp.UserReactor.greet_user(%{name: "Marty"}, %{})
  end
  
  test "greet_user handles edge cases" do
    assert {:ok, "Hello, !"} = 
      MyApp.UserReactor.greet_user(%{name: ""}, %{})
  end
end
```

### 2. Integration Testing Complete Reactors

Test entire workflows by running complete reactors.

#### Module-Based Reactor Testing

Test your actual reactor modules directly. When testing reactors that interact with databases or other shared resources, you'll typically want to disable async execution to ensure proper isolation with test sandboxes:

```elixir
defmodule MyApp.UserRegistrationReactor do
  use Reactor
  
  input :email
  input :password
  
  step :validate_email, MyApp.Steps.ValidateEmail do
    argument :email, input(:email)
  end
  
  step :hash_password, MyApp.Steps.HashPassword do
    argument :password, input(:password)
  end
  
  step :create_user, MyApp.Steps.CreateUser do
    argument :email, result(:validate_email)
    argument :password_hash, result(:hash_password)
  end
end

defmodule MyApp.UserRegistrationReactorTest do
  use ExUnit.Case, async: false
  
  alias MyApp.UserRegistrationReactor
  
  test "successful user registration flow" do
    inputs = %{
      email: "user@example.com",
      password: "secure_password"
    }
    
    assert {:ok, %{id: user_id}} = Reactor.run(UserRegistrationReactor, inputs, async?: false)
    assert is_binary(user_id)
  end
  
  test "handles invalid email gracefully" do
    inputs = %{
      email: "invalid-email",
      password: "secure_password"
    }
    
    assert {:error, _reason} = Reactor.run(UserRegistrationReactor, inputs, async?: false)
  end
end
```

### 3. Testing Error Handling and Compensation

Test how your reactors handle failures and compensation using Mimic to control step behavior.

#### Setting Up Mimic for Step Mocking

We recommend using [Mimic](https://hex.pm/packages/mimic) as your mocking library for testing Reactor steps. Mimic allows you to stub function calls without modifying your production code.

First, set up your test helper to copy the step modules you want to mock:

```elixir
# test/test_helper.exs
Mimic.copy(MyApp.Steps.ProcessPayment)
Mimic.copy(MyApp.Steps.ReserveInventory)
Mimic.copy(MyApp.Steps.SendConfirmation)
ExUnit.start()
```

#### Testing Error Scenarios

```elixir
defmodule MyApp.PaymentReactor do
  use Reactor
  
  input :payment_data
  input :items
  
  step :process_payment, MyApp.Steps.ProcessPayment do
    argument :payment_data, input(:payment_data)
  end
  
  step :reserve_inventory, MyApp.Steps.ReserveInventory do
    argument :items, input(:items)
    argument :payment_id, result(:process_payment, [:id])
  end
  
  step :send_confirmation, MyApp.Steps.SendConfirmation do
    argument :payment_id, result(:process_payment, [:id])
  end
end

defmodule MyApp.PaymentReactorTest do
  use ExUnit.Case, async: false
  use Mimic
  
  alias MyApp.PaymentReactor
  
  test "successful payment flow" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{id: "payment_123", status: :completed}}
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{reservation_id: "res_456"}}
    end)
    
    MyApp.Steps.SendConfirmation
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, :sent}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:ok, :sent} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "payment failure with no compensation needed" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :insufficient_funds}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "inventory failure triggers payment compensation" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{id: "payment_123", status: :completed}}
    end)
    |> expect(:compensate, fn _reason, _args, _context, _opts ->
      :ok
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :out_of_stock}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "compensation returns continue value" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :temporary_failure}
    end)
    |> stub(:compensate, fn _reason, _args, _context, _opts ->
      {:continue, %{id: "fallback_payment", status: :manual_review}}
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{reservation_id: "res_456"}}
    end)
    
    MyApp.Steps.SendConfirmation
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, :sent}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:ok, :sent} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
end
```

#### Testing Undo Logic

```elixir
test "later step failure triggers undo of earlier steps" do
  MyApp.Steps.ProcessPayment
  |> stub(:run, fn _args, _context, _opts -> 
    {:ok, %{id: "payment_123"}}
  end)
  |> expect(:undo, fn _result, _args, _context, _opts ->
    :ok
  end)
  
  MyApp.Steps.ReserveInventory
  |> stub(:run, fn _args, _context, _opts -> 
    {:ok, %{reservation_id: "res_456"}}
  end)
  
  MyApp.Steps.SendConfirmation
  |> stub(:run, fn _args, _context, _opts -> 
    {:error, :email_service_down}
  end)
  
  inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
  assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
end
```

## Best Practices

### Test Organization

1. **Unit Tests**: Create one test file per step module
2. **Integration Tests**: Group related workflow tests together
3. **Use async: true**: For pure unit tests that don't depend on external state
4. **Use async: false**: For integration tests that need deterministic execution

## Related Guides

- [Debugging Workflows](debugging-workflows.md) - Troubleshooting techniques
- [Error Handling Tutorial](../tutorials/02-error-handling.md) - Learn compensation patterns
- [Async Workflows Tutorial](../tutorials/03-async-workflows.md) - Understanding concurrency

This comprehensive testing approach ensures your reactors are reliable, maintainable, and perform well under various conditions.

<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Building Complex Workflows with Composition

In this tutorial, you'll learn how to build large, maintainable workflows by composing smaller reactors together. This is essential for managing complexity in real-world applications.

## What you'll build

A multi-stage e-commerce order processing system that:
1. **User Management** - Validates and enriches user data
2. **Inventory Management** - Checks and reserves product inventory  
3. **Payment Processing** - Handles payment authorization and capture
4. **Order Fulfillment** - Coordinates shipping and notifications
5. **Master Orchestrator** - Composes all sub-workflows together

## You'll learn

- How to break complex workflows into composable reactors
- When to use composition vs building one large reactor
- How to pass data between composed reactors
- Error handling and rollback across composed workflows
- Testing strategies for composed systems

## Prerequisites

- Complete the [Getting Started tutorial](01-getting-started.md)
- Complete the [Error Handling tutorial](02-error-handling.md)  
- Complete the [Async Workflows tutorial](03-async-workflows.md)

## Step 1: Set up the project

If you don't have a project from the previous tutorials:

```bash
mix igniter.new reactor_tutorial --install reactor
cd reactor_tutorial
```

## Step 2: Understanding Reactor composition

Reactor composition allows you to:

**Break down complexity** - Instead of one massive reactor, create focused sub-reactors:
```elixir
# Use composition:
defmodule OrderProcessor do
  use Reactor
  
  compose :user_management, UserManagementReactor
  compose :inventory_check, InventoryReactor  
  compose :payment_processing, PaymentReactor
  compose :fulfillment, FulfillmentReactor
end
```

**Enable reusability** - Sub-reactors can be used in multiple contexts:
```elixir
compose :validate_buyer, UserManagementReactor
compose :validate_seller, UserManagementReactor  
```

**Improve testability** - Test each sub-reactor independently.

## Step 3: Create simple domain reactors

Let's start by building focused reactors for each domain. Create `lib/user_validation_reactor.ex`:

```elixir
defmodule UserValidationReactor do
  use Reactor

  input :user_id

  step :fetch_user do
    argument :user_id, input(:user_id)
    
    run fn %{user_id: user_id}, _context ->
      {:ok, %{
        id: user_id,
        name: "User #{user_id}",
        email: "user#{user_id}@example.com",
        active: true
      }}
    end
  end

  step :validate_user do
    argument :user, result(:fetch_user)
    
    run fn %{user: user}, _context ->
      if user.active do
        {:ok, user}
      else
        {:error, "User is not active"}
      end
    end
  end

  return :validate_user
end
```

Create `lib/inventory_reactor.ex`:

```elixir
defmodule InventoryReactor do
  use Reactor

  input :product_id
  input :quantity

  step :check_availability do
    argument :product_id, input(:product_id)
    argument :quantity, input(:quantity)
    
    run fn %{product_id: product_id, quantity: quantity}, _context ->
      available = 50
      
      if quantity <= available do
        {:ok, %{product_id: product_id, available: available}}
      else
        {:error, "Not enough inventory"}
      end
    end
  end

  step :reserve_items do
    argument :availability, result(:check_availability)
    argument :quantity, input(:quantity)
    
    run fn %{availability: avail, quantity: qty}, _context ->
      reservation = %{
        product_id: avail.product_id,
        quantity: qty,
        reserved_at: DateTime.utc_now()
      }
      {:ok, reservation}
    end
  end

  return :reserve_items
end
```

Create `lib/payment_reactor.ex`:

```elixir
defmodule PaymentReactor do
  use Reactor

  input :user_id
  input :amount

  step :validate_payment do
    argument :user_id, input(:user_id)
    argument :amount, input(:amount)
    
    run fn %{user_id: user_id, amount: amount}, _context ->
      if amount > 0 and amount < 10000 do
        {:ok, %{user_id: user_id, amount: amount, valid: true}}
      else
        {:error, "Invalid payment amount"}
      end
    end
  end

  step :process_payment do
    argument :payment_info, result(:validate_payment)
    
    run fn %{payment_info: info}, _context ->
      payment = %{
        payment_id: "pay_#{:rand.uniform(1000)}",
        user_id: info.user_id,
        amount: info.amount,
        status: :completed,
        processed_at: DateTime.utc_now()
      }
      {:ok, payment}
    end
  end

  return :process_payment
end
```

## Step 4: Create the master orchestrator

Now create the main reactor that composes all sub-reactors. Create `lib/order_processing_reactor.ex`:

```elixir
defmodule OrderProcessingReactor do
  use Reactor

  input :user_id
  input :product_id
  input :quantity
  input :amount

  # Step 1: Validate user
  compose :user_validation, UserValidationReactor do
    argument :user_id, input(:user_id)
  end

  # Step 2: Check inventory (can run in parallel with payment)
  compose :inventory_check, InventoryReactor do
    argument :product_id, input(:product_id)
    argument :quantity, input(:quantity)
  end

  # Step 3: Process payment
  compose :payment_processing, PaymentReactor do
    argument :user_id, input(:user_id)
    argument :amount, input(:amount)
  end

  # Step 4: Create final order (depends on all previous steps)
  step :create_order do
    argument :user, result(:user_validation)
    argument :reservation, result(:inventory_check)
    argument :payment, result(:payment_processing)
    
    run fn %{user: user, reservation: res, payment: pay}, _context ->
      order = %{
        order_id: "order_#{:rand.uniform(1000)}",
        user: user,
        product_id: res.product_id,
        quantity: res.quantity,
        payment_id: pay.payment_id,
        total: pay.amount,
        created_at: DateTime.utc_now()
      }
      
      {:ok, order}
    end
  end

  return :create_order
end
```

## Step 5: Test the composition

Let's test our composed reactor:

```bash
iex -S mix
```

```elixir
# Test the composed order processing
{:ok, order} = Reactor.run(OrderProcessingReactor, %{
  user_id: 123,
  product_id: 456,
  quantity: 2,
  amount: 99.99
})

IO.inspect(order.order_id)
# Should output something like "order_123"

# Test individual reactors too
{:ok, user} = Reactor.run(UserValidationReactor, %{user_id: 123})
IO.inspect(user.name)
# Should output "User 123"
```

## What you learned

You now understand Reactor composition:

- **Composition over monoliths** - Many small reactors are easier to manage than one large one
- **Clear interfaces** - Define clear inputs and outputs for each sub-reactor
- **Independent testing** - Test each reactor in isolation before testing compositions
- **Error boundaries** - Failures in sub-reactors can be handled by the parent reactor
- **Reusability** - Well-designed sub-reactors can be used in multiple contexts

### Design guidelines:

- **Single responsibility** - Each reactor should have one clear purpose
- **Loose coupling** - Minimise dependencies between reactors
- **High cohesion** - Related steps belong in the same reactor

## What's next

Now that you understand composition, you're ready for advanced patterns:

- **[Recursive Execution](05-recursive-execution.md)** - Handle iterative and recursive workflows
- **[Testing Strategies](documentation/how-to/testing-strategies.md)** - Comprehensive testing approaches
- **[Performance Optimization](documentation/how-to/performance-optimization.md)** - Advanced performance techniques

## Common issues

**Sub-reactor outputs don't match expectations**: Use clear interface contracts and validate inputs/outputs in tests

**Error handling doesn't work across boundaries**: Ensure compensation and undo are implemented in the appropriate reactor layer

**Composed reactors are hard to debug**: Test each sub-reactor individually and use descriptive logging

Happy building modular workflows! 🧩

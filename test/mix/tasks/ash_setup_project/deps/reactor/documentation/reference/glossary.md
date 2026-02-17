<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Reactor Glossary

This glossary defines key terms, concepts, and technical vocabulary used throughout Reactor documentation.

## Core Reactor Concepts

**Argument** - A dependency declaration in a step that specifies what data the step needs and where it comes from (inputs, results from other steps, or static values).

**Argument Transformation** - A function applied to an argument value before it's passed to a step, automatically extracted as separate transform steps during planning.

**Compensation** - Error handling mechanism where a step defines how to handle its own failures, returning `:retry`, `:ok`, `{:continue, value}`, or `{:error, reason}`.

**Compose** - A DSL step type that embeds another reactor as a single step, allowing hierarchical workflow composition.

**Context** - Runtime execution environment shared across steps, containing user data, step metadata, retry information, and concurrency details.

**Dependency Graph** - A directed acyclic graph (DAG) where vertices represent steps and edges represent dependencies, used to determine execution order.

**DSL (Domain Specific Language)** - Declarative syntax built with Spark for defining reactors with inputs, steps, and their relationships.

**Dynamic Step Creation** - The ability for steps to emit new steps during execution using `{:ok, result, new_steps}` return format.

**Input** - A named parameter that a reactor accepts when executed, similar to function arguments.

**Intermediate Results** - Storage for step outputs that are needed by dependent steps or the reactor's return value.

**Reactor** - A workflow definition containing inputs, steps, dependencies, and execution logic for orchestrating complex business processes.

**Result** - The output value from a successfully executed step, accessible to dependent steps via `result(:step_name)` syntax.

**Saga Pattern** - Transaction-like coordination pattern across multiple resources without requiring distributed transactions, using compensation for failure handling.

**Step** - A unit of work in a reactor with a unique name, dependencies, implementation, and optional error handling callbacks.

**Subpath Access** - Ability to extract nested values from step results using syntax like `result(:step, [:key, :subkey])`.

**Undo** - Rollback mechanism called when a step succeeded but a later step failed, used to maintain system consistency.

## Technical Architecture Terms

**Concurrency Pool** - Shared resource allocation system that limits concurrent step execution across reactor hierarchies to prevent resource exhaustion.

**Concurrency Tracker** - ETS-based global system managing concurrency pools with atomic operations and process monitoring.

**Cycle Detection** - Validation process ensuring the dependency graph contains no circular dependencies that would create deadlocks.

**DAG (Directed Acyclic Graph)** - Mathematical structure representing step dependencies where edges flow from prerequisite steps to dependent steps.

**Deadlock Prevention** - Automatic detection and mitigation of resource contention in nested reactor scenarios.

**Dependency Resolution** - Process of determining when steps can execute based on availability of their required arguments.

**Executor** - Core engine that coordinates reactor execution, managing concurrency, error handling, and step lifecycle.

**libgraph** - External library used by Reactor for efficient DAG operations and dependency graph management.

**Planner** - Component that converts step definitions into an executable dependency graph with cycle detection and ready state identification.

**Ready Steps** - Steps with all dependencies satisfied that can begin execution immediately.

**State Management** - System for tracking reactor execution state, intermediate results, undo stack, and concurrency information.

**Step Runner** - Component handling individual step execution including argument collection, guard evaluation, and result processing.

## DSL-Specific Terminology

**Around Step** - DSL step type that wraps other steps with setup and teardown logic, similar to middleware.

**Collect Step** - DSL step type that gathers multiple arguments into a single result, optionally applying transformations.

**Debug Step** - Built-in step type that logs information to help with workflow debugging and monitoring.

**Element** - Reference to current item in map step iterations using `element(:map_step_name)` syntax.

**Flunk Step** - DSL step type that always fails with a specified error message, used for testing and controlled failures.

**Group Step** - DSL step type that runs before/after functions around a collection of steps.

**Guard** - Conditional execution mechanism that can halt step execution or provide alternative results based on runtime conditions.

**Map Step** - DSL step type for processing collections by running nested steps for each item in the source data.

**Middleware** - Cross-cutting concern components that can intercept and modify reactor execution at various points.

**Recurse Step** - DSL step type for executing a reactor recursively until exit conditions are met or maximum iterations reached.

**Switch Step** - DSL step type providing conditional logic with multiple branches based on predicate matching.

**Template Step** - DSL step type that processes EEx templates with step arguments as assigns.

**Wait For** - Dependency declaration that waits for step completion without using its result data.

**Where Clause** - Simple conditional execution predicate that determines whether a step should run.

## Error Handling Concepts

**Backoff** - Delay mechanism that adds intelligent waiting periods between retry attempts to prevent overwhelming external services and improve system stability. Backoff delays are minimum delays - actual retry timing may be longer as the executor prioritises processing ready steps before checking for expired backoffs.

**Backoff Callback** - Optional `c:Reactor.Step.backoff/4` callback that determines retry delay based on arguments, context, step metadata, and error reason.

**Compensation Logic** - Step-level error handling that decides whether to retry, continue with alternative values, or fail.

**Error Classification** - Categorisation of errors using splode library into Invalid, Internal, Unknown, and Validation types.

**Exponential Backoff** - Retry strategy where delay increases exponentially with each retry attempt (1s, 2s, 4s, 8s...), commonly used for network issues and service overload.

**Fixed Backoff** - Retry strategy using consistent delays between attempts, often used for rate limiting when reset intervals are known.

**Jittered Backoff** - Backoff strategy that adds randomness to delays to prevent thundering herd problems when multiple clients retry simultaneously.

**Max Retries** - Configuration limiting how many times a step can be retried through compensation.

**Retry Logic** - Automatic re-execution of failed steps when compensation returns `:retry`, now enhanced with optional backoff delays.

**Three-Tier Error Handling** - Reactor's approach using compensation (retry), backoff (delay), undo (rollback), and global rollback levels.

**Undo Stack** - Last-in-first-out collection of successfully completed undoable steps for rollback purposes.

## Concurrency and Execution Terms

**Async by Default** - Reactor's philosophy of running steps asynchronously unless explicitly configured otherwise.

**Concurrency Key** - Identifier for shared concurrency pools allowing resource coordination across reactor hierarchies.

**Execution Loop** - Main algorithm prioritising async task completion, new task starts, and sync execution.

**Max Concurrency** - Configuration parameter limiting simultaneous step execution (defaults to system CPU count).

**Process Supervision** - Management of async step execution under Task.Supervisor with proper cleanup.

**Resource Management** - System for allocation, tracking, and cleanup of concurrency slots and process resources.

**Shared Pools** - Concurrency allocation mechanism allowing multiple reactors to coordinate resource usage.

**Synchronous Execution** - Sequential step execution mode used for testing, resource constraints, or explicit requirements.

**Task Supervision** - OTP pattern for managing async step execution with failure isolation and cleanup.

## Integration and Ecosystem Terms

**Ash.Reactor** - Framework-specific extension providing deep integration with Ash resources, actions, and transactions. Available at [hex.pm/packages/ash](https://hex.pm/packages/ash).

**Builder API** - Programmatic interface for constructing reactors at runtime using `Reactor.Builder` functions.

**Ecosystem Extensions** - Additional packages that extend DSL capabilities:
- [reactor_file](https://hex.pm/packages/reactor_file) - File system operations (copying, moving, permissions, I/O)
- [reactor_process](https://hex.pm/packages/reactor_process) - Supervisor and process management operations  
- [reactor_req](https://hex.pm/packages/reactor_req) - HTTP client steps with DSL for all HTTP methods

**Framework Independence** - Design principle allowing Reactor to work with any Elixir application without framework dependencies.

**Spark** - Foundation library providing DSL infrastructure shared across the Ash ecosystem.

## Performance and Scalability Terms

**Batch Size** - Configuration for map steps controlling how many items are processed together.

**Bounded Resources** - Concurrency limits and resource constraints preventing system exhaustion.

**Memory Optimisation** - Strategies for efficient intermediate result storage and cleanup.

**Strict Ordering** - Map step configuration ensuring results are returned in input order (vs performance-optimised unordered).

## Testing and Development Terms

**Deterministic Execution** - Predictable step execution order achieved by disabling async mode for testing.

**Integration Testing** - Testing complete reactor workflows end-to-end with real or mocked dependencies.

**Mimic** - Recommended mocking library for testing step behaviour and error scenarios.

**Unit Testing** - Testing individual step modules in isolation from reactor execution.

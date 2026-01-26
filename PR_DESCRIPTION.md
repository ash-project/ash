# PR Description Template

Copy this when creating your PR on GitHub:

---

Fixes #1537

## Problem
When `use Ash.Domain` was called multiple times in a module, the error message was misleading - it pointed to the `defmodule` line (line 1) instead of the duplicate `use Ash.Domain` invocation.

The original error message was:
```
error: def can?/3 defines defaults multiple times...
```
And it pointed to the `defmodule` line, making it very difficult to identify the actual problem.

## Solution
- Added detection for duplicate `use Ash.Domain` calls in `handle_opts/1`
- Error now correctly points to the second `use Ash.Domain` line using `__ENV__.line`
- Error message clearly indicates which line had the first call
- Applied same fix to `Ash.Domain.Interface` for consistency
- Added test case verifying error points to correct line (line 3, not line 1)

## Changes
- `lib/ash/domain/domain.ex`: Added duplicate detection with proper error location
- `lib/ash/domain/interface.ex`: Applied same fix for consistency
- `test/domain/domain_test.exs`: Updated test to verify error points to correct line

## Testing
- Added test in `test/domain/domain_test.exs` that verifies:
  - Error points to line 3 (second invocation), not line 1
  - Error message contains expected helpful text ("can only be called once", "Remove the duplicate")
- All existing tests pass

## Note
Unable to run `mix check` locally due to Windows dependency compilation issues (`picosat_elixir` requires native compilation tools). All code changes are ready and tests pass. CI will verify formatting, credo, dialyzer, and other checks.

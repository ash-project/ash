# GAB agent: Ash public? / private_action? verification

This folder contains an AALang/GAB agent that runs tests to verify the Ash **public?** action option and **private_action?()** policy check implementation (internal-only actions feature).

## What it does

When you load the agent (e.g. by referencing `gab/ash-public-actions-test-agent.jsonld` in your session), it will:

1. **Show an initial message** explaining that it will run verification tests for the public?/private_action? fix.
2. **Run the verification** by executing the test suite in the Ash repo (policy tests, resource info tests, resource actions tests).
3. **Report pass/fail** so you can confirm the implementation fixes the problem.

## How to use

1. **In Cursor/IDE:** Open or reference the agent file `ash-public-actions-test-agent.jsonld`. The LLM should execute as the agent: it will show the initial response, then run the verification commands (e.g. `mix ash.verify_public_actions` or the individual `mix test` commands) and report results.
2. **Repo path:** If your Ash repo is not the current workspace, tell the agent the path to the Ash repo root when it asks (e.g. `c:\Users\david\school\ash\ash`).
3. **Windows + VS build:** If `mix compile` needs the Visual Studio environment (e.g. for picosat_elixir), run the tests from a shell where `vcvars64.bat` has been run, or the agent will use the Windows+VS command form for the test run.

## Verification commands

- **Preferred:** `mix ash.verify_public_actions` (runs the curated list of tests and exits 0 only if all pass).
- **Fallback:** Run in order:
  - `mix test test/policy/`
  - `mix test test/resource/info_test.exs`
  - `mix test test/resource/actions_test.exs`

## Success criteria

The implementation is considered to fix the problem if:

- Policy tests pass (the one known unrelated failure is the `debug_expr` line-ending expectation).
- Resource info and resource actions tests pass.
- There are no errors about `public?`, `public_actions/1`, or `private_action?()`.

Created using AALang and Gab.

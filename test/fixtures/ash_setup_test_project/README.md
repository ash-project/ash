# Ash setup test fixture project

Minimal Mix project used as a fixture for `Mix.Tasks.Ash.Setup` tests.

- **Purpose:** Manual testing of `mix ash.setup` from a real project (e.g. `cd` here and run `mix ash.setup`). The automated tests in `test/mix/tasks/ash.setup_test.exs` use Mimic stubs instead of this project.
- **Layout:** Standard Mix app with `ash` as a path dependency (points at the parent Ash repo).

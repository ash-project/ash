# Contributing to Ash

## Welcome!

We are delighted to have anyone contribute to Ash, regardless of their skill level or background. We welcome contributions both large and small, from typos and documentation improvements, to bug fixes and features. There is a place for everyone's contribution here. Check the issue tracker or join the ElixirForum/discord server to see how you can help! Make sure to read the rules below as well.

## Contributing to Documentation

Documentation contributions are one of the most valuable kinds of contributions you can make! Good documentation helps everyone in the community understand and use Ash more effectively.

### Protocol for Documentation Improvements

**We prefer Pull Requests over issues for documentation improvements.** Here's why and how:

- **Make a PR directly** - This is the preferred approach! Even if you're not 100% sure about your changes, submitting a PR with your suggested improvement is much more helpful than opening an issue to discuss it.
- **PRs represent tangible suggestions** - They're easy to review, approve, reject, or modify. We can see exactly what you're proposing and act on it quickly.
- **Issues are okay too** - If you're really unsure or want to discuss a larger documentation restructuring, you can open an issue first. But for most cases, just make the PR!
- **Don't worry about rejection** - If a PR doesn't fit or needs changes, we'll provide feedback or close it with explanation. This is much more efficient than back-and-forth discussion in issues.

### Making Documentation Changes

The best way to contribute to documentation is often through GitHub's web interface, which allows you to make changes without having to clone the code locally:

**For Guides:**
- While viewing any guide on the documentation website, look for the `</>` button in the top right of the page
- Clicking this button will take you directly to GitHub's editing interface for that file

![Guide Edit Button](documentation/assets/images/guides-link.png)

**For Module Documentation:**
- When viewing module documentation, the `</>` button will also be in the top right of the page

**For Function Documentation:**
- When viewing individual functions, you'll find the `</>` button next to the function header

![Function Edit Button](documentation/assets/images/functions-link.png)

Once you click the `</>` button, GitHub will:
1. Fork the repository for you (if you haven't already)
2. Open the file in GitHub's web editor
3. Allow you to make your changes directly in the browser
4. Help you create a pull request with your improvements

This workflow makes it incredibly easy to fix typos, clarify explanations, add examples, or improve any part of the documentation you encounter while using Ash.

### Important Note About DSL Documentation

**DSL documentation cannot be edited directly on GitHub.** The documentation you see for DSL options (like those for `Ash.Resource`, `Ash.Domain`, etc.) is generated from the source code of the DSL definition modules.

For example, if you want to improve documentation for `Ash.Resource` options, you need to edit the source code in the `Ash.Resource.Dsl` module, not the generated documentation files. The DSL documentation is automatically generated from the `@doc` attributes and option definitions in these modules.

To find the right module to edit:
- For `Ash.Resource` DSL docs → edit `lib/ash/resource/dsl.ex`
- For `Ash.Domain` DSL docs → edit `lib/ash/domain/dsl.ex`
- And so on for other DSL modules

When making DSL documentation improvements, make sure to:
1. Edit the appropriate DSL definition module (not generated docs)
3. Test that your changes generate correctly by running `mix docs`

## Rules

* We have a zero tolerance policy for failure to abide by our code of conduct. It is very standard, but please make sure
  you have read it.
* Issues may be opened to propose new ideas, to ask questions, or to file bugs.
* Before working on a feature, please talk to the core team/the rest of the community via a proposal. We are
  building something that needs to be cohesive and well thought out across all use cases. Our top priority is
  supporting real life use cases like yours, but we have to make sure that we do that in a sustainable way. The
  best compromise there is to make sure that discussions are centered around the *use case* for a feature, rather
  than the proposed feature itself.
* Before starting work, please comment on the issue and/or ask in the discord if anyone is handling an issue. Be aware that if you've commented on an issue that you'd like to tackle it, but no one can reach you and/or demand/need arises sooner, it may still need to be done before you have a chance to finish. However, we will make all efforts to allow you to finish anything you claim.

## Local Development & Testing

### Setting Up Your Development Environment

1. **Fork and clone the repository:**
   ```bash
   git clone https://github.com/your-username/ash.git
   cd ash
   ```

2. **Install dependencies:**
   ```bash
   mix deps.get
   ```

3. **Compile the project:**
   ```bash
   mix compile
   ```

### Running Tests and Checks

Before submitting any pull request, please run the full test suite and quality checks locally:

```bash
mix check
```

This command runs a comprehensive suite of checks including:
- Compilation
- Tests
- Code formatting (via `spark.formatter`)
- Credo (static code analysis)
- Dialyzer (type checking)
- Documentation generation and validation
- Sobelow (security analysis)
- And other quality checks

You can also run individual checks:
- `mix test` - Run the test suite
- `mix format` - Format code
- `mix credo` - Run static analysis
- `mix dialyzer` - Run type checking
- `mix docs` - Generate documentation

### Testing Ash with Your Application

If you want to test your Ash changes with your own application, you can use Ash as a local dependency. In your application's `mix.exs`, replace the hex dependency with a path dependency:

```elixir
defp deps do
  [
    # Replace this:
    # {:ash, "~> 3.0"}

    # With this (adjust path as needed):
    {:ash, path: "../ash"},

    # Your other dependencies...
  ]
end
```

Then run:
```bash
mix deps.get
mix compile
```

This allows you to:
- Test your changes against real-world usage
- Verify that your changes don't break existing functionality
- Develop features iteratively with immediate feedback

Testing in your own application is not sufficient, you must also include automated tests.

### Development Workflow

1. **Create a feature branch:**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** and write tests

3. **Run the full check suite:**
   ```bash
   mix check
   ```

4. **Commit your changes:**
   ```bash
   git add .
   git commit -m "Add feature description"
   ```

5. **Push and create a pull request**

### Common Development Tasks

- **Generate documentation:** `mix docs`
- **Run tests in watch mode:** `mix test.watch`
- **Check formatting:** `mix format --check-formatted`
- **Run specific test file:** `mix test test/path/to/test_file.exs`
- **Run tests with coverage:** `mix test --cover`

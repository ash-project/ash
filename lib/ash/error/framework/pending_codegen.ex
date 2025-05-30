defmodule Ash.Error.Framework.PendingCodegen do
  @moduledoc "Used when an extension has pending code generation and the --check flag is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:diff, explain: false], class: :framework

  def message(%{diff: diff, explain: explain}) do
    """
    Pending Code Generation Detected for #{Enum.count(diff)} files#{explain(explain)}
    """
  end

  defp explain(true) do
    """


    Don't worry! This just means Ash needs to generate some files based on your recent changes.

    Here's what you can do:

    🚀 Ready to generate the files?
       Run: `mix ash.codegen --dev`
       (This creates temporary development files so you can keep working)

    🔍 Want to see what will be generated?
       Run: `mix ash.codegen --dry-run`

    ✅ Finished with your changes?
       Run: `mix ash.codegen <describe_your_changes>`
       (This creates the final, production-ready files)

    💡 The --dev flag is handy because it creates temporary files (like migrations ending in `_dev`)
       that get cleaned up and properly named when you're ready to finalize everything.
    """
  end
end

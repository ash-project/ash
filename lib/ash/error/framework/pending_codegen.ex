defmodule Ash.Error.Framework.PendingCodegen do
  @moduledoc "Used when an extension has pending code generation and the --check flag is provided"

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

    🚀 Ready to generate the files? Create temporary development files so you can keep working:

           mix ash.codegen --dev

    🔍 Want to see what will be generated?

           mix ash.codegen --dry-run

    ✅ Finished with your changes? Create final production-ready files:

           mix ash.codegen <describe_your_changes>
    """
  end

  defp explain(_), do: ""
end

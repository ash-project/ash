# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.SynchronousEngineStuck do
  @moduledoc "Used when the sycnrhonous engine cannot proceed"

  use Splode.Error, fields: [], class: :framework

  def message(_) do
    """
    Synchronous Engine Stuck

    This should not be possible, please report a detailed bug at:

    https://github.com/ash-project/ash/issues/new?assignees=&labels=bug%2C+needs+review&template=bug_report.md&title=
    """
  end
end

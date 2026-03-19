# SPDX-FileCopyrightText: 2024 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Gen.GettextTest do
  use ExUnit.Case, async: false

  @marker "## -- ash-gettext:start --"

  setup do
    tmp_dir =
      Path.join(System.tmp_dir!(), "ash_gen_gettext_test_#{System.unique_integer([:positive])}")

    File.mkdir_p!(tmp_dir)
    on_cleanup = fn -> File.rm_rf!(tmp_dir) end

    original_cwd = File.cwd!()
    File.cd!(tmp_dir)

    on_exit(fn ->
      File.cd!(original_cwd)
      on_cleanup.()
    end)

    {:ok, tmp_dir: tmp_dir}
  end

  describe "default (ash domain)" do
    test "copies ash.pot as before" do
      Mix.Tasks.Ash.Gen.Gettext.run([])

      dest = "priv/gettext/ash.pot"
      assert File.exists?(dest)

      content = File.read!(dest)
      assert content =~ "must be present"
      assert content =~ ~s("Domain: ash\\n")
    end
  end

  describe "--domain errors" do
    test "creates new errors.pot when file does not exist" do
      Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "errors"])

      dest = "priv/gettext/errors.pot"
      assert File.exists?(dest)

      content = File.read!(dest)
      assert content =~ @marker
      assert content =~ "must be present"
      assert content =~ ~s("Domain: errors\\n")
      refute content =~ ~s("Domain: ash\\n")
    end

    test "merges into existing errors.pot preserving existing entries" do
      File.mkdir_p!("priv/gettext")

      existing = """
      ## This file is a PO Template file.

      msgid ""
      msgstr ""
      "Content-Type: text/plain; charset=UTF-8\\n"
      "Domain: errors\\n"

      #: lib/my_app/accounts.ex:10
      msgid "custom app error"
      msgstr ""
      """

      File.write!("priv/gettext/errors.pot", existing)

      Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "errors"])

      content = File.read!("priv/gettext/errors.pot")
      assert content =~ "custom app error"
      assert content =~ @marker
      assert content =~ "must be present"
    end

    test "is idempotent — re-running replaces marker block" do
      Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "errors"])
      first = File.read!("priv/gettext/errors.pot")

      Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "errors"])
      second = File.read!("priv/gettext/errors.pot")

      assert first == second
    end

    test "preserves other library blocks after ash block" do
      File.mkdir_p!("priv/gettext")

      other_marker = "## -- ash_authentication-gettext:start --"

      existing = """
      msgid ""
      msgstr ""
      "Domain: errors\\n"

      #{@marker}
      msgid "old ash message"
      msgstr ""

      #{other_marker}
      msgid "must confirm email"
      msgstr ""
      """

      File.write!("priv/gettext/errors.pot", existing)

      Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "errors"])

      content = File.read!("priv/gettext/errors.pot")
      # ash block replaced
      assert content =~ "must be present"
      refute content =~ "old ash message"
      # other library block preserved
      assert content =~ other_marker
      assert content =~ "must confirm email"
    end
  end

  describe "domain validation" do
    test "rejects invalid domain names" do
      assert_raise Mix.Error, ~r/Invalid domain/, fn ->
        Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "../escape"])
      end

      assert_raise Mix.Error, ~r/Invalid domain/, fn ->
        Mix.Tasks.Ash.Gen.Gettext.run(["--domain", ""])
      end

      assert_raise Mix.Error, ~r/Invalid domain/, fn ->
        Mix.Tasks.Ash.Gen.Gettext.run(["--domain", "has/slash"])
      end
    end
  end
end

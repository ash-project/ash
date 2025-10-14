# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.FileTest do
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain
  alias Ash.Type.File, as: FileType

  doctest FileType

  defmodule MyFile do
    defstruct [:path]

    @behaviour FileType.Implementation

    @impl FileType.Implementation
    def path(%MyFile{path: path}), do: {:ok, path}

    @impl FileType.Implementation
    def open(%MyFile{path: path}, options), do: File.open(path, options)

    defimpl FileType.Source do
      def implementation(%MyFile{}), do: {:ok, MyFile}
    end
  end

  defmodule StringImpl do
    @behaviour FileType.Implementation

    @impl FileType.Implementation
    def open(string, _options), do: StringIO.open(string, [])
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults []
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      action :import, :file do
        argument :file, :file, allow_nil?: false

        run fn %Ash.ActionInput{arguments: %{file: file}}, _context ->
          {:ok, file}
        end
      end
    end
  end

  describe inspect(&FileType.cast_input/2) do
    test "can cast itself" do
      file = %FileType{source: __ENV__.file, implementation: FileType.Path}

      assert {:ok, ^file} =
               Post
               |> Ash.ActionInput.for_action(:import, %{file: file})
               |> Ash.run_action()
    end

    test "can cast protocol implementation" do
      file = %MyFile{path: __ENV__.file}

      assert {:ok, %FileType{implementation: MyFile, source: ^file}} =
               Post
               |> Ash.ActionInput.for_action(:import, %{file: file})
               |> Ash.run_action()
    end

    test "can cast Plug.Upload" do
      file = %Plug.Upload{path: __ENV__.file}

      assert {:ok, %FileType{implementation: FileType.PlugUpload, source: ^file}} =
               Post
               |> Ash.ActionInput.for_action(:import, %{file: file})
               |> Ash.run_action()
    end

    test "can't cast any other value" do
      file = DateTime.utc_now()

      assert {:error,
              %Ash.Error.Invalid{
                errors: [%Ash.Error.Action.InvalidArgument{field: :file, class: :invalid}]
              }} =
               Post
               |> Ash.ActionInput.for_action(:import, %{file: file})
               |> Ash.run_action()
    end
  end

  describe inspect(&FileType.open/2) do
    test "can open path" do
      file = FileType.from_path(__ENV__.file)

      assert {:ok, handle} = FileType.open(file, [:read])

      assert IO.read(handle, 9) == "# SPDX-Fi"
    end

    test "can open IO device" do
      {:ok, device} = StringIO.open("Test")
      file = FileType.from_io(device)

      assert {:ok, handle} = FileType.open(file, [:read])

      assert IO.read(handle, 4) == "Test"
    end

    test "can open Plug.Upload" do
      file = %FileType{
        source: %Plug.Upload{path: __ENV__.file},
        implementation: FileType.PlugUpload
      }

      assert {:ok, handle} = FileType.open(file, [:read])

      assert IO.read(handle, 9) == "# SPDX-Fi"
    end
  end

  describe inspect(&FileType.path/1) do
    test "results in path if supported" do
      file = FileType.from_path(__ENV__.file)

      assert FileType.path(file) == {:ok, __ENV__.file}
    end

    test "results in path for Plug.Upload" do
      file = %FileType{
        source: %Plug.Upload{path: __ENV__.file},
        implementation: FileType.PlugUpload
      }

      assert FileType.path(file) == {:ok, __ENV__.file}
    end

    test "errors if not supported" do
      {:ok, device} = StringIO.open("Test")
      file = FileType.from_io(device)

      assert FileType.path(file) == {:error, :not_supported}
    end
  end
end

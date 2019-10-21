defmodule RscriptTest do
  use ExUnit.Case
  doctest Rscript

  test "greets the world" do
    assert Rscript.hello() == :world
  end
end

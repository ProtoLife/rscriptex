# Rscript

Testing communication between Elixir and Rscript

```
iex -S mix
iex(1)> Rscript.start_link("testJsonChars.r", [script_runner: :littler, protocol: :chars, args: [], json_arg: %{a: 1, b: "hello"}])
```


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `rscript` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:rscript, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/rscript](https://hexdocs.pm/rscript).

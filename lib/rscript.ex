defmodule Rscript do
  @moduledoc """
  Proof of concept for API script runner.
  """
  use GenServer

  @doc """
  Opens a port to an OS process running an `r` or `Rscript` shell.

  Arguments:
    fname - path of Rscript to run
    opts - a keyword list with these options:
      :script_runner - :littler (default) or :rscript
      :args - optional list of command-line string arguments to pass to script
      :json_arg - if supplied, an Elixir object that will be JSON-encoded and piped
        to the Rscript's stdin on startup.
      :protocol - :lines (default) or :chars. Protocol used to send :json_arg to the Rscript.
        Use :chars for large objects. See testJsonChars.r for how to parse :chars.
        Use :lines for small objects (JSON string < 4096 bytes). See testJsonLines.r
  """
  def start_link(fname \\ "testParallel.r", opts \\ []) do
    GenServer.start_link(__MODULE__, [fname, opts])
  end

  def init([fname, opts]) when is_binary(fname) do
    script_path = case fname do
      "" ->
        nil
      <<first :: binary-size(1)>> <> rest ->
        if first == "/" do
          fname
        else
          Path.join(:code.priv_dir(:rscript), "R", fname)
        end
    end
    script_runner = Keyword.get(opts, :script_runner, :littler)
    protocol = Keyword.get(opts, :protocol, :lines)
    args = Keyword.get(opts, :args, [])
    json_arg = Keyword.get(opts, :json_arg)
    IO.puts "script_path: #{script_path}"
    IO.puts "script_runner: #{script_runner}"
    IO.puts "protocol: #{protocol}"
    IO.puts "args: #{inspect(args)}"
    IO.puts "json_arg: #{inspect(json_arg)}"
    state = %{
      script_runner: script_runner,
      script_path: script_path,
      protocol: protocol,
      args: args,
      json_arg: json_arg,
      port: nil
    }
    {:ok, state, {:continue, :open_port}}
  end

  def init(arg) do
    {:stop, "The script file name is required"}
  end

  def handle_continue(:open_port,
    %{script_runner: script_runner, script_path: script_path, protocol: protocol,
      args: args, json_arg: json_arg} = state) do
    Process.flag(:trap_exit, true)
    executable = case script_runner do
      :littler ->
        "/usr/bin/r"
      _ ->
        "/usr/bin/Rscript"
    end
    cmd_args = [script_path | args]
    port = Port.open({:spawn_executable, executable}, [:binary, :exit_status, args: cmd_args])
    {:os_pid, os_pid} = Port.info(port, :os_pid)
    if !is_nil(json_arg) do
      Process.send_after(self(), {port, {:json_arg, json_arg}}, 100)
    end
    # IO.puts "Port opened, os_pid: #{os_pid}"
    {:noreply, %{state | port: port}}
  end

  # Send arguments to port's stdin
  def handle_call(:close, _from, %{port: port} = state) do
    Port.close(port)
    {:reply, :ok, state}
  end

  # Send arguments to port's stdin
  def handle_info({port, {:json_arg, nil}}, state) do
    {:noreply, state}
  end
  def handle_info({port, {:json_arg, arg}}, %{protocol: protocol} = state) do
    data = Jason.encode_to_iodata!(arg)
    data_length = IO.iodata_length(data)
    IO.puts "Sending #{data_length} bytes via #{protocol} to port #{inspect(port)}"
    case protocol do
      :chars ->
        Port.command(port, [Integer.to_string(data_length), "\n"])
        Port.command(port, data)
        Port.command(port, ["\n"])
      _ ->
        Port.command(port, data)
        Port.command(port, ["\n"])
    end
    {:noreply, state}
  end

  def handle_info({port, :connected}, state) do
    # IO.puts "Received :connected message from port #{inspect(port)}"
    {:noreply, %{state | port: port}}
  end

  def handle_info({port, :closed}, state) do
    # IO.puts "Received :closed message from port #{inspect(port)}"
    {:noreply, %{state | port: nil}}
  end

  def handle_info({port, {:data, msg}}, state) do
    IO.puts "Received data from port #{inspect(port)}: #{msg}"
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, rc}}, state) do
    IO.puts "Port #{inspect(port)} exited with status #{rc}"
    {:noreply, state}
  end

  def handle_info({:EXIT, port, reason}, state) do
    IO.puts "Received :EXIT message from port #{inspect(port)}, reason: #{inspect(reason)}"
    {:noreply, %{state | port: nil}}
  end
end

defmodule GenServerCallNormal do
  use DynamicSupervisor

  defmodule Server do
    use GenServer, restart: :transient

    def start_link(arg), do: GenServer.start_link(__MODULE__, arg)

    def init({iteration, timeout, puts?}) do
      Process.send_after(self(), :shutdown, timeout)
      {:ok, {iteration, puts?}}
    end

    def handle_info(:shutdown, {iteration, puts?} = state) do
      if puts?, do: IO.puts("#{iteration}: exiting...")

      {:stop, :normal, state}
    end

    def handle_call(:do_work, _from, state) do
      {:reply, :ok, state}
    end
  end

  @default_opts timeout: 100, iters: 100, puts?: true

  def run(opts \\ []) do
    opts = Keyword.merge(@default_opts, opts)

    timeout = opts[:timeout]
    iters = opts[:iters]
    puts? = opts[:puts?]

    {:ok, supervisor} = DynamicSupervisor.start_link(__MODULE__, [])

    try do
      loop(%{}, iters, supervisor, timeout, puts?)
    after
      DynamicSupervisor.stop(supervisor)
    end
  end

  def init(_init_arg), do: DynamicSupervisor.init(strategy: :one_for_one)

  defp loop(acc, 0, _supervisor, _timeout, _puts?), do: acc

  defp loop(acc, iters, supervisor, timeout, puts?) do
    {:ok, server} = DynamicSupervisor.start_child(supervisor, {Server, {iters, timeout, puts?}})

    Process.sleep(timeout)
    GenServer.call(server, :do_work)

    acc
    |> Map.update(:success, 1, &(&1 + 1))
    |> loop(iters - 1, supervisor, timeout, puts?)
  catch
    :exit, {reason, _request} ->
      acc
      |> Map.update(reason, 1, &(&1 + 1))
      |> loop(iters - 1, supervisor, timeout, puts?)
  end
end

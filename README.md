# GenServerCallNormal

[Erlang docs](https://www.erlang.org/doc/man/gen_server.html#call-3) for `gen_server:call/2,3` state that a call can exit with `{normal, _Location}` if `handle_call/3` returns `{stop, normal, _}` without replying:

<blockquote>
<dl>
  <dt><code>normal</code><br>
      <code>{shutdown,Term}</code></dt>
  <dd>The server stopped during the call by returning <code>{stop,Reason,_}</code> from its <a href="https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3"><code>Module:handle_call/3</code></a> callback, without replying. See also <a href="https://www.erlang.org/doc/man/gen_server.html#stop-3"><code>stop/3.</code></a></dd>
</dl>
</blockquote>

This repository contains minimal reproducible examples (written in both Erlang and
Elixir) which demonstrate that `gen_server:call/2,3` can also exit with `normal` as
the reason if `handle_call/3` does return a reply and the `gen_server` process
was stopped from within another `gen_server` callback (`handle_info/2`).

The code is provided as an Elixir project, so to run it launch an IEx session:

```bash
$ iex -S mix
```

Elixir example:

```iex
iex(1)> GenServerCallNormal.run()
# [...skip output...]
%{normal: 100}
```

Erlang example:

```iex
iex(2)> :gen_server_call_normal.run(100, 100, true)
# [...skip output...]
%{normal: 98, success: 2}
```

Here we performed 100 `gen_server` calls of which 100 exited with the `normal` reason
for the Elixir example (and 98 for the Erlang example); the actual numbers can
differ from run to run.

Interestingly enough, this behavior cannot be reproduced if we don't write to the standard output in the `handle_info/2` callback:

```iex
iex(3)> GenServerCallNormal.run(puts?: false)
%{noproc: 100}
iex(4)> :gen_server_call_normal.run(100, 100, false)
%{noproc: 100}
```

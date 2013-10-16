{application, server,
 [{description, "engine manager Server!"},
  {id, "server"},
  {vsn, "0.1"},
  {modules, [server]},
  {registered, [server, server_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {server, []}},
  {env, []}
  ]}.

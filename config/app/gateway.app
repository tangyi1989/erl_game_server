{application, gateway,
 [{description, "engine manager Server!"},
  {id, "gateway"},
  {vsn, "0.1"},
  {modules, [gateway]},
  {registered, [gateway, gateway_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {gateway, []}},
  {env, []}
  ]}.

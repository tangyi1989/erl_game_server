{application, manager,
 [{description, "engine manager Server!"},
  {id, "manager"},
  {vsn, "0.1"},
  {modules, [manager]},
  {registered, [manager, manager_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {manager, []}},
  {env, []}
  ]}.

{application, logger,
 [{description, "engine logger Server!"},
  {id, "logger"},
  {vsn, "0.1"},
  {modules, [logger]},
  {registered, [logger, logger_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {logger, []}},
  {env, []}
  ]}.

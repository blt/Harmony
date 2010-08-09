{application, harmony,
 [{description, "Harmony Project daemon"},
  {vsn, "0.1.0"},
  {modules, [harmony_app, harmony_sup, harmony_listener,
             harmony_uni, harmony_logger]},
  {registered, []},
  {applications, [kernel, stdlib, mnesia, sasl]},
  {mod, {harmony_app, []}},
  {env, []}
 ]}.

{application, twitmachine,
 [{description, "twitmachine"},
  {vsn, "0.1"},
  {modules, [
    twitmachine,
    twitmachine_app,
    twitmachine_sup,
    twitmachine_deps,
    twitmachine_resource
  ]},
  {registered, []},
  {mod, {twitmachine_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.

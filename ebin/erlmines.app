{application, erlmines,
  [{description, "Minetest server written on Erlang"},
   {vsn, "0.0.0"},
   {modules, [erlmines,erlmines_app,erlmines_sup,clientserver]},
   {registered, [erlmines]},
   {applications, [kernel, stdlib]},
   {mod, {erlmines_app, []}},
   {start_phases, []}
  ]}.

{application, s5_server,
  [{vsn, "1.0.0"},
    {modules, [s5_server_app, s5_server_handler, s5_server_sup, yb_common]},
    {registered, [s5_server_sup]},
    {mod, {s5_server_app, []}}
    ]}.

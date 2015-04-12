{application, s5_client,
  [{vsn, "1.0.0"},
    {modules, [s5_client_app, s5_client_handler, s5_client_sup, yb_common]},
    {registered, [s5_client_sup]},
    {mod, {s5_client_app, []}}
    ]}.

{application, nodes_controller.app,
    [{description, "Node list controller"},
    {vsn, "1"},
    {modules, [nodes_controller, nl_server, nodes_controller_sup]},
    {registered, [nodes_controller_sup, nl_server]},
    {applications, [kernel, stdlib]},
    {mod, {nodes_controller,[]}}
 ]}.


{application, papers_server.app,
    [{description, "Papers Server"},
    {vsn, "1"},
    {modules, [papers_server, papers, papers_server_sup]},
    {registered, [papers, papers_server_sup]},
    {applications, [kernel, stdlib]},
    {mod, {papers_server,[]}}
 ]}.


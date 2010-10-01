{application, erlfix,
 [{description, "FIX engine"},
  {vsn, "0.0.1"},
  {modules, [erlfix_parser,erlfix_supervisor,erlfix_server,erfix_accepter,erlfix]},
  {registered, []},
  {maxT,infinity},
  {applications, [kernel, stdlib]},
  {env,[
      {host,"localhost"},
      {port, 9878},
      {type, accepter}
      ]},
  {mod, {erlfix,[]}}
 ]}.
{application, langolier_app,
 [
  {description, ""},
  {vsn, "1"},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {langolier_app, [top_handler]}},
  {env, [
		{main_socket, [{type, server}, {ip, <<"10.55.5.52">>}, {port, 8001}]},
		{clients_sockets, [
			[{type, client}, {ip, <<"10.55.5.52">>}, {port, 7001}],
			[{type, client}, {ip, <<"10.55.5.52">>}, {port, 7002}],
			[{type, client}, {ip, <<"10.55.5.52">>}, {port, 7003}]]}
	]}
 ]}.
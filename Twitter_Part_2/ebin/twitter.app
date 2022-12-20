{application, 'twitter', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['hello_handler','hello_handlers','reg','twitter','twitter_app','twitter_sup']},
	{registered, [twitter_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {twitter_app, []}},
	{env, []}
]}.
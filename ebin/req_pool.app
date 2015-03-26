{application, req_pool, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, ""},
	{modules, ['req_pool', 'req_pool_api', 'req_pool_app', 'req_pool_sup', 'req_pool_supersup', 'req_pool_worker_sup', 'req_worker', 'rss_wc_lib']},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		jiffy,
		inets,
		statsd
	]},
	{mod, {req_pool_app, []}},
%	{mod, {statsd_app, ["localhost", 8125]}},
	{env, 
	  [{search_path, "//item/description/text()"},
	   {token_string, "= "},
	   {count_limit, 10},
	   {stopwords_file, "./stopwords.txt"}
	  ]}
]}.

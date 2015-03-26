PROJECT = req_pool
DEPS = jiffy statsd-erlang
dep_statsd-erlang =  git https://github.com/domnikl/statsd-erlang.git HEAD
include erlang.mk

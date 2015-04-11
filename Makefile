PROJECT = tbcd
DEPS = cowboy lager erlang-uuid
dep_erlang-uuid = git https://github.com/avtobiff/erlang-uuid.git v0.4.7
include erlang.mk
ERLC_OPTS += +'{parse_transform, lager_transform}'

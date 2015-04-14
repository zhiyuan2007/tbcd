PROJECT = tbcd
DEPS = cowboy lager erlang-uuid jiffy
dep_erlang-uuid = git https://github.com/avtobiff/erlang-uuid.git v0.4.7
dep_jiffy = git https://github.com/davisp/jiffy.git 0.13.3
include erlang.mk
ERLC_OPTS += +'{parse_transform, lager_transform}'

PROJECT = tbcd
DEPS = cowboy lager
include erlang.mk
ERLC_OPTS += +'{parse_transform, lager_transform}'

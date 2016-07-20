PROJECT = erprice
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS= cowboy eredis
include erlang.mk

GG_ERLANG_OPTS=-sname cli -setCookie ErZaukerCluster -pa deps/*/ebin/ -pa ebin/ +K true -smp enable  ${ERL_ARGS}
# GG Specific task for windows:
runwin:	
	_rel/erprice_release/bin/erprice_release.cmd console

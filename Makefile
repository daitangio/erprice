PROJECT = erprice
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1


# Dependency tracking
DEPS= cowboy eredis lager

# Eredis commit of Sep 10 2015: proper compilation with OTP >= 17.0
dep_eredis_commit=bf12ecb30253c84a2331f4f0d93fd68856fcb9f4

dep_cowboy_commit=1.0.4

dep_lager_commit=3.2.1


#### Unit testing
EUNIT_OPTS = verbose



include erlang.mk

GG_ERLANG_OPTS=-sname cli -setCookie ErZaukerCluster -pa deps/*/ebin/ -pa ebin/ +K true -smp enable  ${ERL_ARGS}
# GG Specific task for windows:
runwin:	
	_rel/erprice_release/bin/erprice_release.cmd console

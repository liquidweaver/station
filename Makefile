REBAR=`which rebar`
SPRITES := $(patsubst %.dmi,%.meta,$(wildcard web/sprites/*.dmi))
all: deps compile $(SPRITES)
deps:
	@$(REBAR) -r get-deps
compile:
	@$(REBAR) -r compile
test:
	@$(REBAR) -r skip_deps=true eunit
clean:
	@$(REBAR) -r skip_deps=true clean
generate: 
	@cd rel && rebar generate
start: compile generate
	rel/station/bin/station console

%.meta : %.dmi
	identify -verbose $< | sed -n '/BEGIN DMI/,/END DMI/p' | sed /#/d > $@

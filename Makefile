REBAR=`which rebar`
SPRITES := $(patsubst %.dmi,%.meta,$(wildcard web/sprites/*.dmi))
all: deps compile $(SPRITES)
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
generate: compile
	@cd rel && rebar generate

%.meta : %.dmi
	identify -verbose $< | sed -n '/BEGIN DMI/,/END DMI/p' | sed /#/d > $@

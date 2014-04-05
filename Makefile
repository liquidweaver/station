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
	@$(REBAR) -r clean
generate: compile
	@cd rel && rebar generate

%.meta : %.dmi
	identify -verbose $< | sed -n '/BEGIN DMI/,/END DMI/p' | sed /#/d > $@

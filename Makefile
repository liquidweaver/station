REBAR=`which rebar`
BEHAVIORS := $(patsubst %.erl,%.beam,$(wildcard behaviors/*.erl))
SPRITES := $(patsubst %.dmi,%.meta,$(wildcard web/sprites/*.dmi))
DEPSOLVER_PLT=.dialyzer_plt
HOWLERJS=web/js/howler.js
all: compile $(SPRITES) $(HOWLERJS)
update_deps:
	@$(REBAR) update-deps
compile: update_deps $(BEHAVIORS)
	@$(REBAR) compile
tests:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) skip_deps=true clean
generate:
	@cd rel && rebar generate
start: compile generate
	rel/station/bin/station console

$(HOWLERJS): howler.js/howler.min.js
	cp howler.js/howler.min.js $(HOWLERJS)

%.beam : %.erl $(DEPSOLVER_PLT)
	erlc +debug_info -o behaviors $<
	cp behaviors/*.beam ebin/
	dialyzer --add_to_plt --plt $(DEPSOLVER_PLT) -r behaviors

%.meta : %.dmi
	identify -verbose $< | sed -n '/BEGIN DMI/,/END DMI/p' | sed /#/d > $@

$(DEPSOLVER_PLT):
	- dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	--apps erts kernel stdlib crypto public_key -r deps behaviors

dialyzer: $(DEPSOLVER_PLT) compile
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src src/objects
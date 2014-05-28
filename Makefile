REBAR=`which rebar`
BEHAVIORS := $(patsubst %.erl,%.beam,$(wildcard behaviors/*.erl))
SPRITES := $(patsubst %.dmi,%.meta,$(wildcard web/sprites/*.dmi))
DEPSOLVER_PLT=.dialyzer_plt
all: compile $(SPRITES)
deps:
	@$(REBAR) get-deps
	@$(REBAR) compile
compile: deps $(BEHAVIORS)
	@$(REBAR) compile skip_deps=true
tests:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) skip_deps=true clean
update_deps:
	@$(REBAR) update-deps
generate:
	@cd rel && rebar generate
start: compile
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(station)."

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

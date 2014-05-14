.PHONY: deps test

REBAR := ./rebar
APPS = erts kernel stdlib sasl
COMBO_PLT = .leo_pod_combo_dialyzer_plt

all:
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) compile
	@$(REBAR) xref skip_deps=true
	@$(REBAR) eunit skip_deps=true
compile:
	@$(REBAR) compile skip_deps=true
xref:
	@$(REBAR) xref skip_deps=true
eunit:
	@$(REBAR) eunit skip_deps=true
check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)
build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)
dialyzer:
	@$(REBAR) compile
	dialyzer --plt $(COMBO_PLT) -r src/ --src
doc: compile
	@$(REBAR) doc
clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean

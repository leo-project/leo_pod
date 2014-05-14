.PHONY: deps test

REBAR := ./rebar
APPS = erts kernel stdlib sasl
PLT_FILE = .leo_pod_combo_dialyzer_plt

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
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)
build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS)
dialyzer:
	@$(REBAR) compile
	dialyzer --plt $(PLT_FILE) --dump_callgraph leo_pod.dot -r src/ --src
doc: compile
	@$(REBAR) doc
callgraph: graphviz
	dot -Tpng -oleo_pod.png leo_pod.dot
	@echo "Dependency graph created as leo_pod.png"
graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))
clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean

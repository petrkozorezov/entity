REBAR=./rebar
DIALYZER=dialyzer
#ERL_LIBS=${PWD}/deps:${PWD}/apps
#export ERL_LIBS


.PHONY: all compile deps clean distclean doc dialyzer eunit ct tests

all: compile

$(REBAR):
	wget https://github.com/downloads/basho/rebar/rebar -O $(REBAR)
	chmod +x $(REBAR)

deps: $(REBAR)
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps
	rm -rfv plts

release: compile
	cd rel && ../$(REBAR) generate

## dialyzer
~/.dialyzer_plt:
	dialyzer --build_plt --output_plt ~/.dialyzer_plt --apps `ls /usr/lib/erlang/lib/ | awk -F "-" '{print $$1}' | sed '/erl_interface/d' | sed '/jinterface/d'`; true

## you have to compile project first
plts/otp.plt: ~/.dialyzer_plt
	mkdir -p plts && cp ~/.dialyzer_plt plts/otp.plt

dialyzer: plts/otp.plt
	rm -rf ".eunit"
	$(DIALYZER) --plt plts/otp.plt -n --no_native ebin; true

#\
#   -Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_fun_app \
#   -Wunderspecs -Wno_opaque -Wno_return -Wno_unused -Wno_improper_lists
# TODO fix empty "suite="
inittests:
	$(REBAR) -C rebar.tests.config get-deps
	$(REBAR) -C rebar.tests.config compile

eunit:
	$(REBAR) -C rebar.tests.config eunit skip_deps=true

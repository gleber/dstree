REBAR=$(shell which rebar || echo ./rebar)

all: $(REBAR)
	$(REBAR) compile

tests:  $(REBAR)
	$(REBAR) eunit

sh: all
	erl -pa ebin/ -eval 'shell_default:m(dstree).'



# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
		-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean:
	rm -f ./rebar

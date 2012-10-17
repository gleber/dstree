REBAR=$(shell which rebar || echo ./rebar)

all: $(REBAR)
	$(REBAR) get-deps compile

tests:  $(REBAR)
	$(REBAR) eunit skip_deps=true suite=dstree

sh: all
	erl -pa ebin/ -run reloader -eval 'shell_default:m(dstree).'

test: tests


# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
		-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean:
	rm -f ./rebar

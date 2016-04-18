VER=none


all:	$(VER)

dep:
	rebar g-d

gen_parser: dep
	erlc ./tool/gen_parser.erl
	erl -pa ./deps/erlsom/ebin/ -noshell -s gen_parser main $(spec) -s init stop

none:
	@echo "usage:"
	@echo "    make gen_parser spec=FIX44.xml"
	@echo "avaiable spec:"
	@ls spec

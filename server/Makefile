all: ebin/ script

script: source
	erl -pa ./ebin/ -eval 'systools:make_script("harmony-0.1.0"), init:stop().'

source:
	(cd src;$(MAKE) all)

edoc:
	(cd src;$(MAKE) edoc)

test: ebin/
	(cd src;$(MAKE) test)

clean:
	@rm ebin/*.beam *.boot *.script

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin


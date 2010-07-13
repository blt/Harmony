dist_erl_DATA = harmony_app.erl harmony_sup.erl harmony_server.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_harmony)/src
dist_hrl_DATA = harmony.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_harmony)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) harmony.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_harmony)/ebin
privdir = $(ERLANG_INSTALL_LIB_DIR_harmony)/priv
boot_DATA = harmony.boot harmony.script
bootdir = $(libdir)/erlang/bin
CLEANFILES = $(beam_DATA) $(boot_DATA) harmony.rel
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

.rel.boot:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,harmony})" -noshell -s init stop

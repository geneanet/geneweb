Makefile.config: configure
	@if [ -e "$@" ]; then \
	  echo "configure file has changed. Please rerun ./configure"; exit 1; \
	else \
	  echo "Please run ./configure first"; exit 1; \
	fi

include Makefile.config
-include Makefile.local

# Variables for packagers.
PREFIX=/usr
DISTRIB_DIR=distribution

BUILD_DIR=_build/default

EXE = \
	bin/distrib/connex.exe \
	bin/distrib/consang.exe \
	bin/distrib/ged2gwb.exe \
	bin/distrib/gwb2ged.exe \
	bin/distrib/gwc.exe \
	bin/distrib/gwd.exe \
	bin/distrib/gwdiff.exe \
	bin/distrib/gwtp.exe \
	bin/distrib/gwu.exe \
	bin/distrib/setup.exe \
	bin/distrib/update_nldb.exe \

###### [BEGIN] Generated files section

CAMLP5_PA_EXTEND_FILES = \
	bin/distrib/ged2gwb/ged2gwb \
	lib/templ \
	bin/distrib/setup/setup

CAMLP5_Q_MLAST_FILES = \
	lib/templ

CAMLP5_FILES = $(sort $(CAMLP5_Q_MLAST_FILES) $(CAMLP5_PA_EXTEND_FILES))

$(CAMLP5_PA_EXTEND_FILES:=.ml): CAMLP5_OPT += pa_extend.cmo
$(CAMLP5_Q_MLAST_FILES:=.ml): CAMLP5_OPT += q_MLast.cmo

%.ml: CAMLP5_OPT=

%.ml: %.camlp5.ml
	@([ -z "$(CAMLP5_OPT)" ] \
	|| false \
	&& echo "ERROR generating $@: CAMLP5_OPT variable must be defined") \
	|| (echo -n "Generating $@..." \
	    && echo "(* DO NOT EDIT *)" > $@ \
	    && echo "(* This file was generated from $< *)" >> $@ \
	    && camlp5o pr_o.cmo $(CAMLP5_OPT) -impl $< >> $@ \
	    && sed -i.bak -E 's/[(]\* (\[@.+\]) \*[)]/\1/g' $@ \
	    && sed -i.bak -E 's/[(]\* (#[^\*]+) \*[)]/\1/g' $@ \
	    && rm $@.bak \
	    && echo " Done!")

lib/gwlib.ml:
	echo "let prefix =" > $@
	echo "  try Sys.getenv \"GWPREFIX\"" >> $@
	echo "  with Not_found -> \"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@

CPPO_D=$(API_D) $(GWDB_D)

%/dune: %/dune.in
	cat $< \
	| cppo -n $(CPPO_D) \
	| sed \
	-e "s/%%%CPPO_D%%%/$(CPPO_D)/g" \
	-e "s/%%%API_PKG%%%/$(API_PKG)/g" \
	-e "s/%%%SOSA_PKG%%%/$(SOSA_PKG)/g" \
	-e "s/%%%GWDB_PKG%%%/$(GWDB_PKG)/g" \
	-e "s/%%%DUNE_DIRS_EXCLUDE%%%/$(DUNE_DIRS_EXCLUDE)/g" \
	> $@

hd/etc/version.txt:
	echo "GeneWeb[:] [compiled on %s from commit %s:::" > $@
	echo "$$(date '+%Y-%m-%d'):" >> $@
	echo "$$(git show -s --date=short --pretty=format:'<a href="https://github.com/geneweb/geneweb/commit/%h">%h (%cd)</a>')]" >> $@
.PHONY:hd/etc/version.txt

###### [End] Generated files section

GENERATED_FILES_DEP = \
	hd/etc/version.txt \
	lib/gwlib.ml \
	$(CAMLP5_FILES:=.ml) \
	benchmark/dune \
	bin/distrib/dune \
	lib/dune \

ifdef API_D
piqi:
	$(foreach p, $(wildcard lib/*.proto), \
		piqi of-proto --normalize $(p) ; \
		piqic-ocaml -C lib/ --ext $(p).piqi ; \
	  )
	$(RM) lib/*.piqi
else
piqi:
endif
.PHONY: piqi

geneweb.install:
	dune build @install
.PHONY: geneweb.install

%.exe: | piqi $(GENERATED_FILES_DEP)
	dune build $@
exe:
	dune build $(ALL_EXE:=.exe)
.DEFAULT_GOAL = exe

geneweb.install exe: $(GENERATED_FILES_DEP) piqi

###### [BEGIN] Installation / Distribution section

install: geneweb.install
	dune install geneweb

uninstall: geneweb.install
	dune uninstall geneweb

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/distrib/

distrib: exe
	$(RM) -r $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
	cp CHANGES $(DISTRIB_DIR)/CHANGES.txt
	cp LICENSE $(DISTRIB_DIR)/LICENSE.txt
	cp etc/README.txt $(DISTRIB_DIR)/.
	cp etc/LISEZMOI.txt $(DISTRIB_DIR)/.
	cp etc/START.htm $(DISTRIB_DIR)/.
	if test $(OS_TYPE) = "Win"; then \
	  cp etc/Windows/gwd.bat $(DISTRIB_DIR); \
	  cp etc/Windows/gwsetup.bat $(DISTRIB_DIR); \
	  cp -f etc/Windows/README.txt $(DISTRIB_DIR)/README.txt; \
	  cp -f etc/Windows/LISEZMOI.txt $(DISTRIB_DIR)/LISEZMOI.txt; \
	elif test $(OS_TYPE) = "Darwin"; then \
	  cp etc/gwd $(DISTRIB_DIR)/gwd.command; \
	  cp etc/gwsetup $(DISTRIB_DIR)/gwsetup.command; \
	  cp etc/macOS/geneweb.command $(DISTRIB_DIR); \
	else \
	  cp etc/gwd $(DISTRIB_DIR); \
	  cp etc/gwsetup $(DISTRIB_DIR); \
	fi
	mkdir $(DISTRIB_DIR)/gw
	cp etc/a.gwf $(DISTRIB_DIR)/gw/.
	echo "127.0.0.1" > $(DISTRIB_DIR)/gw/only.txt
	echo "-setup_link" > $(DISTRIB_DIR)/gw/gwd.arg
	cp $(BUILD_DISTRIB_DIR)gwc.exe $(DISTRIB_DIR)/gw/gwc$(EXT);
	cp $(BUILD_DISTRIB_DIR)consang.exe $(DISTRIB_DIR)/gw/consang$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXT);
	cp $(BUILD_DISTRIB_DIR)ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXT);
	cp $(BUILD_DISTRIB_DIR)connex.exe $(DISTRIB_DIR)/gw/connex$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXT);
	cp $(BUILD_DISTRIB_DIR)setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT);
	cp $(BUILD_DISTRIB_DIR)update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT);
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp/lang
	cp bin/distrib/gwtp/README $(DISTRIB_DIR)/gw/gwtp_tmp/.
	cp $(BUILD_DISTRIB_DIR)/gwtp.exe $(DISTRIB_DIR)/gw/gwtp_tmp/gwtp$(EXT)
	cp bin/distrib/gwtp/lang/*.txt $(DISTRIB_DIR)/gw/gwtp_tmp/lang/.
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/distrib/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/distrib/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/distrib/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/distrib/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/distrib/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/distrib/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp -R hd/* $(DISTRIB_DIR)/gw/
	$(RM) $(DISTRIB_DIR)/exe-version.txt
	echo "Commit: `git log  -1 | grep commit | cut -c8-15`, `date`"      > $(DISTRIB_DIR)/commit.txt
	echo "`ocaml  -version`"      >> $(DISTRIB_DIR)/commit.txt
	echo "`camlp5 -v 2>&1`"       >> $(DISTRIB_DIR)/commit.txt
	echo "-----"                  >> $(DISTRIB_DIR)/commit.txt

.PHONY: install uninstall distrib

###### [END] Installation / Distribution section

doc: | piqi $(GENERATED_FILES_DEP)
	dune build @doc
.PHONY: doc

test: | piqi $(GENERATED_FILES_DEP)
	dune build @runtest
.PHONY: test

bench: | piqi $(GENERATED_FILES_DEP)
	dune build @runbench
.PHONY: bench

clean:
	$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	$(RM) -r $(DISTRIB_DIR)
	dune clean
.PHONY: clean

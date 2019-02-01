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

###### [BEGIN] Executables list

INSTALL_EXE = \
	bin/distrib/connex \
	bin/distrib/ged2gwb \
	bin/distrib/ged2gwb2 \
	bin/distrib/gwb2ged \
	bin/distrib/gwc1 \
	bin/distrib/gwc2 \
	bin/distrib/gwd \
	bin/distrib/gwdiff \
	bin/distrib/gwtp \
	bin/distrib/gwu \
	bin/distrib/mk_consang \
	bin/distrib/setup \
	bin/distrib/update_nldb

ALL_EXE = \
	$(INSTALL_EXE) \
	bin/contrib/gui/gui \
	bin/contrib/gwFix/gwFindCpl \
	bin/contrib/gwFix/gwFixBase \
	bin/contrib/gwFix/gwFixBurial \
	bin/contrib/gwFix/gwFixColon \
	bin/contrib/gwFix/gwFixEvtSrc \
	bin/contrib/gwFix/gwFixFromFile \
	bin/contrib/gwFix/gwFixFromFileAlias \
	bin/contrib/gwFix/gwFixFromFileDomicile \
	bin/contrib/gwFix/gwFixY \
	bin/contrib/gwbase/etc/public \
	bin/contrib/gwbase/etc/public2 \
	bin/contrib/gwpublic/gwiftitles \
	bin/contrib/gwpublic/gwprivate \
	bin/contrib/gwpublic/gwpublic \
	bin/contrib/gwpublic/gwpublic1 \
	bin/contrib/gwpublic/gwpublic2 \
	bin/contrib/gwpublic/gwpublic2priv \
	bin/contrib/history/convert_hist \
	bin/contrib/history/fix_hist \
	bin/contrib/history/is_gw_plus \
	bin/contrib/lex/lex_utils \
	bin/contrib/misc/lower_string \
	bin/contrib/oneshot/gwBaseCompatiblePlus \
	bin/contrib/oneshot/gwExportAscCSV \
	bin/contrib/oneshot/gwFixDateText \
	bin/contrib/oneshot/gwRemoveImgGallery

EVERYTHING_EXE = \
	$(ALL_EXE) \
	bin/contrib/check_base/check_base \
	bin/contrib/dag2html/main \
	bin/contrib/gwbase/etc/chkimg \
	bin/contrib/gwbase/etc/clavier \
	bin/contrib/gwbase/etc/consmoy \
	bin/contrib/gwbase/etc/geneanet \
	bin/contrib/gwbase/etc/gwck \
	bin/contrib/gwbase/etc/hist \
	bin/contrib/gwbase/etc/lune \
	bin/contrib/gwbase/etc/nbdesc \
	bin/contrib/gwbase/etc/probot \
	bin/contrib/gwbase/etc/selroy \
	bin/contrib/gwbase/etc/titres \
	bin/contrib/i18n_check/i18n_check \
	bin/contrib/oneshot/gwFixAddEvent \
	bin/contrib/oneshot/gwMostAsc

###### [END] Executables list

###### [BEGIN] Generated files section

CAMLP5_PA_EXTEND_FILES = \
	bin/distrib/ged2gwb/ged2gwb \
	bin/distrib/ged2gwb/ged2gwb2 \
	lib/templ \
	lib/update \
	bin/distrib/setup/setup

CAMLP5_Q_MLAST_FILES = \
	lib/templ

CAMLP5_FILES = $(sort $(CAMLP5_Q_MLAST_FILES) $(CAMLP5_PA_EXTEND_FILES))

$(CAMLP5_PA_EXTEND_FILES:=.ml): CAMLP5_OPT += pa_extend.cmo
$(CAMLP5_Q_MLAST_FILES:=.ml): CAMLP5_OPT += q_MLast.cmo

%.ml: CAMLP5_OPT=

%.ml: %.camlp5
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

%/dune: %/dune.in
	sed -e "s/%%%API%%%/$(API)/g" -e "s/%%%API_DEP%%%/$(API_DEP)/g" $< > $@

###### [End] Generated files section

GENERATED_FILES_DEP = lib/gwlib.ml $(CAMLP5_FILES:=.ml) lib/dune

ifdef API
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

install-exe:
	dune build $(INSTALL_EXE:=.exe)
exe:
	dune build $(ALL_EXE:=.exe)
everything-exe:
	dune build $(EVERYTHING_EXE:=.exe)
.DEFAULT_GOAL = exe

geneweb.install install-exe exe everything-exe: $(GENERATED_FILES_DEP) piqi

###### [BEGIN] Installation / Distribution section

install: geneweb.install
	dune install geneweb

uninstall: geneweb.install
	dune uninstall geneweb

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/distrib/

distrib: install-exe
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
	cp $(BUILD_DISTRIB_DIR)gwc1/gwc1.exe $(DISTRIB_DIR)/gw/gwc$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwc1/gwc1.exe $(DISTRIB_DIR)/gw/gwc1$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwc2/gwc2.exe $(DISTRIB_DIR)/gw/gwc2$(EXE); \
	cp $(BUILD_DISTRIB_DIR)mk_consang/mk_consang.exe $(DISTRIB_DIR)/gw/consang$(EXE); \
	cp $(BUILD_DISTRIB_DIR)mk_consang/mk_consang.exe $(DISTRIB_DIR)/gw/mk_consang$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwd/gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwu/gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXE); \
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXE); \
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb2.exe $(DISTRIB_DIR)/gw/ged2gwb2$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXE); \
	cp $(BUILD_DISTRIB_DIR)connex/connex.exe $(DISTRIB_DIR)/gw/connex$(EXE); \
	cp $(BUILD_DISTRIB_DIR)gwdiff/gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXE); \
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXE); \
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXE); \
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp/lang
	cp bin/distrib/gwtp/README $(DISTRIB_DIR)/gw/gwtp_tmp/.
	cp $(BUILD_DISTRIB_DIR)/gwtp.exe $(DISTRIB_DIR)/gw/gwtp_tmp/gwtp$(EXE)
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

clean:
	$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	$(RM) -r distribution
	dune clean
.PHONY: clean

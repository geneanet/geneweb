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
	src/gwc1 \
	src/mk_consang \
	src/gwd \
	src/gwu \
	src/update_nldb \
	ged2gwb/ged2gwb \
	gwb2ged/gwb2ged \
	setup/setup

DISTRIB_EXE = $(INSTALL_EXE) gwtp/gwtp

ALL_EXE = \
	$(INSTALL_EXE) \
	gwtp/gwtp \
	gui/gui \
	contrib/gwpublic/gwpublic1 \
	contrib/gwpublic/gwpublic2 \
	contrib/gwpublic/gwpublic2priv \
	contrib/gwpublic/gwprivate \
	contrib/gwpublic/gwiftitles \
	contrib/gwFix/gwFixBase \
	contrib/gwFix/gwFixFromFile \
	contrib/gwFix/gwFixFromFileDomicile \
	contrib/gwFix/gwFixFromFileAlias \
	contrib/gwFix/gwFixBurial \
	contrib/gwFix/gwFixEvtSrc \
	contrib/gwFix/gwFixColon \
	contrib/gwFix/gwFindCpl \
	contrib/gwFix/gwFixY \
	contrib/gwdiff/gwdiff \
	contrib/gwbase/etc/public \
	contrib/gwbase/etc/public2 \
	contrib/history/convert_hist \
	contrib/history/fix_hist \
	contrib/history/is_gw_plus \
	contrib/lex/lex_utils \
	contrib/misc/lower_string \
	contrib/oneshot/gwRemoveImgGallery \
	contrib/oneshot/gwBaseCompatiblePlus \
	contrib/oneshot/gwFixDateText \
	contrib/oneshot/gwExportAscCSV

EVERYTHING_EXE = \
	$(ALL_EXE) \
	contrib/gwbase/etc/geneanet \
	contrib/gwbase/etc/clavier \
	contrib/gwbase/etc/connex \
	contrib/gwbase/etc/hist \
	contrib/gwbase/etc/selroy \
	contrib/gwbase/etc/chkimg \
	contrib/gwbase/etc/consmoy \
	contrib/gwbase/etc/lune \
	contrib/gwbase/etc/titres \
	contrib/gwbase/etc/gwck \
	contrib/gwbase/etc/nbdesc \
	contrib/gwbase/etc/probot \
	contrib/oneshot/gwFixAddEvent \
	contrib/oneshot/gwMostAsc \
	dag2html/main \
	gwtp/recover \
	src/check_base \
	src/i18n_check

###### [END] Executables list

###### [BEGIN] Generated files section

CAMLP5_PA_EXTEND_FILES = \
	ged2gwb/ged2gwb \
	src/pr_transl \
	internal/templ \
	internal/update \
	setup/setup

CAMLP5_Q_MLAST_FILES = \
	src/pr_transl \
	internal/templ

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

internal/gwlib.ml:
	echo "let prefix =" > $@
	echo "  try Sys.getenv \"GWPREFIX\"" >> $@
	echo "  with Not_found -> \"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@

internal/compilation.ml:
	echo "let scan_dmy s =" > $@
	echo "let open Adef in" >> $@
	echo "Scanf.sscanf s \"%d %d %d\" @@ fun day month year ->" >> $@
	echo "Dgreg({day;month;year;prec=Sure;delta=0},Dgregorian)" >> $@
	echo "let compilation_time = scan_dmy \"$$(date "+%d %m %Y")\"" >> $@
	echo "let commit = \"$$(git show -s --pretty=format:%h)\"" >> $@
	echo "let commit_date = scan_dmy \"$$(git show -s --pretty=format:%cd --date=format:'%d %m %Y')\"" >> $@
.PHONY:internal/compilation.ml

%/dune: %/dune.in
	sed -e "s/%%%API%%%/$(API)/g" -e "s/%%%API_DEP%%%/$(API_DEP)/g" $< > $@

###### [End] Generated files section

GENERATED_FILES_DEP = internal/gwlib.ml $(CAMLP5_FILES:=.ml) lib/dune internal/dune internal/compilation.ml

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
distrib-exe:
	dune build $(DISTRIB_EXE:=.exe)
exe:
	dune build $(ALL_EXE:=.exe)
everything-exe:
	dune build $(EVERYTHING_EXE:=.exe)
.DEFAULT_GOAL = exe

geneweb.install install-exe distrib-exe exe everything-exe: $(GENERATED_FILES_DEP) piqi

###### [BEGIN] Installation / Distribution section

install: geneweb.install
	dune install geneweb

uninstall: geneweb.install
	dune uninstall geneweb

distrib: distrib-exe
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
	cp $(BUILD_DIR)/src/gwc1.exe $(DISTRIB_DIR)/gw/gwc$(EXE); \
	cp $(BUILD_DIR)/src/gwc1.exe $(DISTRIB_DIR)/gw/gwc1$(EXE); \
	cp $(BUILD_DIR)/src/gwc2.exe $(DISTRIB_DIR)/gw/gwc2$(EXE); \
	cp $(BUILD_DIR)/src/mk_consang.exe $(DISTRIB_DIR)/gw/mk_consang$(EXE); \
	cp $(BUILD_DIR)/src/mk_consang.exe $(DISTRIB_DIR)/gw/consang$(EXE); \
	cp $(BUILD_DIR)/src/gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXE); \
	cp $(BUILD_DIR)/src/gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXE); \
	cp $(BUILD_DIR)/ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXE); \
	cp $(BUILD_DIR)/gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXE); \
	cp $(BUILD_DIR)/contrib/gwbase/etc/connex.exe $(DISTRIB_DIR)/gw/connex$(EXE); \
	cp $(BUILD_DIR)/contrib/gwdiff/gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXE); \
	cp $(BUILD_DIR)/setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXE); \
	cp $(BUILD_DIR)/src/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXE); \
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp
	mkdir $(DISTRIB_DIR)/gw/gwtp_tmp/lang
	cp gwtp/README $(DISTRIB_DIR)/gw/gwtp_tmp/.
	cp $(BUILD_DIR)/gwtp/gwtp.exe $(DISTRIB_DIR)/gw/gwtp_tmp/gwtp$(EXE)
	cp gwtp/lang/*.txt $(DISTRIB_DIR)/gw/gwtp_tmp/lang/.
	mkdir $(DISTRIB_DIR)/gw/setup
	cp setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp -R hd/* $(DISTRIB_DIR)/gw/
	$(RM) $(DISTRIB_DIR)/exe-version.txt
	echo "Commit: `git log  -1 | grep commit | cut -c8-15`, `date`"      > $(DISTRIB_DIR)/commit.txt
	echo "`ocaml  -version`"      >> $(DISTRIB_DIR)/commit.txt
	echo "`camlp5 -v 2>&1`"       >> $(DISTRIB_DIR)/commit.txt
	echo "-----"                  >> $(DISTRIB_DIR)/commit.txt

.PHONY: install uninstall distrib

###### [END] Installation / Distribution section

clean:
	$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	$(RM) -r distribution
	dune clean
.PHONY: clean

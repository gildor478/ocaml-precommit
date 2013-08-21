default: build
	cd ../ounit && \
	  $(CURDIR)/_build/src/OCamlPrecommit.byte --full \
	    --exclude log-html \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

OASIS2DEBIAN_ARGS="--distribution wheezy \
		--executable-name ocaml-precommit"

deploy:
	admin-gallu-deploy --verbose \
		--debian_pkg --debuild --debian_upload \
		--oasis2debian_args '$(OASIS2DEBIAN_ARGS)' \
		--forge_upload	--forge_group ocaml-precommit --forge_user gildor-admin
	admin-gallu-oasis-increment --use_vcs \
		--setup_run --setup_args '-setup-update dynamic'

.PHONY: deploy

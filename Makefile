default: test

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

OASIS2DEBIAN_ARGS="--distribution wheezy --executable-name ocaml-precommit"

deploy:
	admin-gallu-deploy --verbose \
	  --debian_pkg --debuild --debian_upload \
	  --oasis2debian_args '$(OASIS2DEBIAN_ARGS)' \
	  --forge_upload	--forge_group ocaml-precommit --forge_user gildor-admin
	admin-gallu-oasis-increment --use_vcs \
	  --setup_run --setup_args '-setup-update dynamic'

.PHONY: deploy

# Precommit target
#  Check style of code.
PRECOMMIT_ARGS= \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

OCAML_PRECOMMIT=_build/src/bin/OCamlPrecommit.byte
precommit: build
	@if [ -e $(OCAML_PRECOMMIT) ]; then \
	  $(OCAML_PRECOMMIT) $(PRECOMMIT_ARGS); \
	else \
	  echo "Skipping precommit checks.";\
	fi

precommit-full: build
	$(OCAML_PRECOMMIT) --full $(PRECOMMIT_ARGS)

test: precommit

.PHONY: precommit

# Headache target
#  Fix license header of file.

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name .git -prune -false \
	  -o -name .svn -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: headache

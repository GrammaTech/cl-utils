# To use this file first define the following variables in your
# makefile and then include cl.mk.
#
# PACKAGE_NAME ------- The full name of the CL package
# PACKAGE_NICKNAME --- The nickname of the CL package
#                      (default: PACKAGE_NAME)
# DOC_PACKAGES ------- Names of packages to document
#                      (default: PACKAGE_NAME)
# DOC_DEPS ----------- Optional additional Makefile targets for doc
# BINS --------------- Names of binaries to build
# TEST_ARTIFACTS ----- Name of dependencies for testing
# TEST_BINS ---------- Name of lisp binaries needed for testing
# TEST_BIN_DIR ------- Directory of lisp binaries needed for testing
# LISP_DEPS ---------- Packages require to build CL package
# TEST_LISP_DEPS ----- Packages require to build CL test package
# BIN_TEST_DIR ------- Directory holding command-line tests
# BIN_TESTS ---------- List of command line tests
# LONG_BIN_TESTS ----- List of longer running command line tests
#                      Used by the `real-check' target.
SHELL=bash

.PHONY: test-artifacts check unit-check real-check clean more-clean real-clean Dockerfile

.SECONDARY:

# Set default values of PACKAGE_NICKNAME
PACKAGE_NICKNAME ?= $(PACKAGE_NAME)
DOC_PACKAGES ?= $(PACKAGE_NAME)

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
INSTDIR ?= /usr/synth
QUICK_LISP ?= $(INSTDIR)/quicklisp/

MANIFEST=$(QUICK_LISP)/local-projects/system-index.txt

ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

LISP_DEPS ?=				\
	$(wildcard *.lisp) 		\
	$(wildcard src/*.lisp)

# Default lisp to build manifest file.
LISP ?= sbcl
ifneq (,$(findstring sbcl, $(LISP)))
ifeq ("$(SBCL_HOME)","")
LISP_HOME = SBCL_HOME=$(dir $(shell which $(LISP)))../lib/sbcl
endif
endif
REPL_STARTUP ?= ()

ifneq ($(LISP_STACK),)
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --noinform --dynamic-space-size $(LISP_STACK) --no-userinit --no-sysinit
else
ifneq (,$(findstring ecl, $(LISP)))
# TODO: Figure out how to set --heap-size appropriately.
LISP_FLAGS = --norc
else
LISP_FLAGS = --stack-size $(LISP_STACK) --quiet --no-init --batch
endif
endif
else
ifneq (,$(findstring sbcl, $(LISP)))
LISP_FLAGS = --no-userinit --no-sysinit
else
ifneq (,$(findstring ecl, $(LISP)))
LISP_FLAGS = --norc
else
LISP_FLAGS = --quiet --no-init --batch
endif
endif
endif

ifneq ($(GT),)
LISP_FLAGS += --eval "(push :GT *features*)"
endif

ifneq ($(REPORT),)
LISP_FLAGS += --eval "(push :REPORT *features*)"
endif

all: $(addprefix bin/, $(BINS))

ifneq ($(GT),)
.qlfile: .ci/qlfile.grammatech
	cp $< $@
else
.qlfile: .ci/qlfile.external
	cp $< $@
endif

$(MANIFEST): .qlfile
	awk '{if($$4){br=$$4}else{br="master"}print $$3, br}' .qlfile|while read pair;do \
	dependency=$$(echo "$${pair}"|cut -f1 -d' '); \
	base=$(QUICK_LISP)/local-projects/$$(basename $$dependency .git); \
	branch=$$(echo "$${pair}"|cut -f2 -d' '); \
	[ -d $$base ] || git clone $$dependency $$base --branch $$branch; \
	done
	$(LISP_HOME) $(LISP) $(LISP_FLAGS) --load $(QUICK_LISP)/setup.lisp \
		--eval '(ql:register-local-projects)' \
		--eval '#+sbcl (exit) #+ccl (quit)'

bin/%: $(LISP_DEPS) $(MANIFEST)
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(ql:quickload :$(PACKAGE_NAME)/run-$*)' \
	--eval '(setf uiop/image::*lisp-interaction* nil)' \
	--eval '(ql::call-with-quiet-compilation (lambda () (asdf:make :$(PACKAGE_NAME)/run-$* :type :program :monolithic t)))' \
	--eval '(quit)'

$(TEST_BIN_DIR)/%: $(LISP_DEPS) $(MANIFEST)
	@rm -f $@
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(ql:quickload :$(PACKAGE_NAME)/run-$*)' \
	--eval '(setf uiop/image::*lisp-interaction* nil)' \
	--eval '(ql::call-with-quiet-compilation (lambda () (asdf:make :$(PACKAGE_NAME)/run-$* :type :program :monolithic t)))' \
	--eval '(quit)'

bin:
	mkdir -p $@


## Testing
TEST_ARTIFACTS ?=
TEST_LISP_DEPS ?= $(wildcard test.lisp)
TEST_LISP_LIBS += $(PACKAGE_NAME)/test

test-artifacts: $(TEST_ARTIFACTS)

unit-check: test-artifacts $(TEST_LISP_DEPS) $(LISP_DEPS) $(MANIFEST)
	CC=$(CC) $(LISP_HOME) LISP=$(LISP) $(LISP) $(LISP_FLAGS) \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(ql:quickload :$(PACKAGE_NAME)/test)' \
	--eval '($(PACKAGE_NAME)/test::run-batch)' \
	--eval '(uiop:quit (if $(PACKAGE_NAME)/test::*success* 0 1))'

check: unit-check bin-check

real-check: check long-bin-check


## Interactive testing
SWANK_PORT ?= 4005
swank: $(QUICK_LISP)/setup.lisp
	$(LISP_HOME) $(LISP)					\
	--load $<						\
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(in-package :$(PACKAGE_NAME))'			\
	--eval '(ql::call-with-quiet-compilation (lambda () (swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)))'

swank-test: $(QUICK_LISP)/setup.lisp test-artifacts
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(ql:quickload :swank)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)'		\
	--eval '(in-package :$(PACKAGE_NAME)-test)'		\
	--eval '(ql::call-with-quiet-compilation (lambda () (swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t)))'

repl: $(QUICK_LISP)/setup.lisp
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(in-package :$(PACKAGE_NAME))'			\
	--eval '(ql::call-with-quiet-compilation (lambda () $(REPL_STARTUP)))'

repl-test: $(QUICK_LISP)/setup.lisp test-artifacts
	$(LISP_HOME) $(LISP) $(LISP_FLAGS)			\
	--load $<						\
	--eval '(ql:quickload :repl)'				\
	--eval '(ql:quickload :$(PACKAGE_NAME))'		\
	--eval '(ql:quickload :$(PACKAGE_NAME)-test)'		\
	--eval '(in-package :$(PACKAGE_NAME)-test)'		\
	--eval '(ql::call-with-quiet-compilation (lambda () $(REPL_STARTUP)))'


## Command-line testing.
BIN_TEST_DIR ?= test/bin

PASS=\e[1;1m\e[1;32mPASS\e[1;0m
FAIL=\e[1;1m\e[1;31mFAIL\e[1;0m
check/%: $(BIN_TEST_DIR)/% $(addprefix bin/, $(BINS))
	@export PATH=./bin:$(PATH); \
	if ./$< >/dev/null 2>/dev/null;then \
	printf "$(PASS)\t\e[1;1m%s\e[1;0m\n" $*; exit 0; \
	else \
	printf "$(FAIL)\t\e[1;1m%s\e[1;0m\n" $*; exit 1; \
	fi

desc/%: check/%
	@$(BIN_TEST_DIR)/$* -d

bin-check: test-artifacts $(addprefix check/, $(BIN_TESTS))
bin-check-desc: test-artifacts $(addprefix desc/, $(BIN_TESTS))

long-bin-check: test-artifacts $(addprefix check/, $(LONG_BIN_TESTS))
long-bin-check-desc: test-artifacts $(addprefix desc/, $(LONG_BIN_TESTS))


## Cleaning
clean:
	rm -f $(addprefix bin/, $(BINS))
	rm -f $(TEST_ARTIFACTS)
	rm -f $(addprefix test/bin/, $(TEST_BINS))

more-clean: clean
	find . -type f -name "*.fasl" -exec rm {} \+
	find . -type f -name "*.lx32fsl" -exec rm {} \+
	find . -type f -name "*.lx64fsl" -exec rm {} \+
	make -C doc clean

real-clean: more-clean
	rm -f .qlfile Dockerfile
	rm -rf $(MANIFEST)

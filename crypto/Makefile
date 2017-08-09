PROJECT = crypto
PROJECT_DESCRIPTION = Crypto-related code
PROJECT_VERSION = 0.1.0

PROJECT_DIR := $(realpath .)

ORG_SRC = $(wildcard src/*.org)
ERL_SRC = $(ORG_SRC:.org=.erl)

app:: $(ERL_SRC)

src/%.erl: src/%.org
	@emacs --eval="(setq package-user-dir (concat \"$(PROJECT_DIR)\" \"/.emacs.d\"))" -l $(PROJECT_DIR)/.emacs --visit=$< --batch -f org-babel-tangle --kill
	@echo "created $@"

clean::
	@rm $(ERL_SRC)

include erlang.mk

# Ocsigen
# http://www.ocsigen.org/
# Makefile
# Copyright (C) 2009 Stéphane Glondu Vincent Balat
# Laboratoire PPS - CNRS Université Paris Diderot
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, with linking exception;
# either version 2.1 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

include Makefile.config

VERSION :=$(shell head -n 1 VERSION)
DESTDIR :=$(shell ocamlfind printconf destdir)
DISPLAYFLAG := -classic-display

OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild -X nis_chkpwd $(DISPLAYFLAG)

MYOCAMLFIND := _build/myocamlfind.byte
TARGETS := ocsforge.otarget
OBROWSERDIR := $(shell ocamlfind query obrowser)
ELIOMOBROWSERDIR := $(shell ocamlfind query ocsigen.eliom_obrowser_client)

TOINSTALL := 

STATICFILES := 

all: $(MYOCAMLFIND) ocsforge

ocsforge: $(MYOCAMLFIND)
	PGUSER=$(DBUSER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(OCAMLBUILD) $(TARGETS)

$(MYOCAMLFIND): myocamlfind.ml
	$(OCAMLBUILD) -no-plugin $(subst _build/,,$@)

install:

uninstall:
	$(OCAMLFIND) remove ocsforge

clean:
	rm -Rf _build
#	make -C nis_chkpwd clean

.PHONY: all ocamlbuild clean

SHELL=bash

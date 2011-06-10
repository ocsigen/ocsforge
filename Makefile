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
OCAMLBUILD_SWIG := ocamlbuild -cflag swig_svn.o -cflag swig_svn_wrap.o -X nis_chkpwd
SWIG := swig -ocaml

MYOCAMLFIND := _build/myocamlfind.byte
TARGETS := ocsforge.otarget
#OBROWSERDIR := $(shell ocamlfind query obrowser)
AXODIR := $(OBROWSERDIR)
#ELIOMOBROWSERDIR := $(shell ocamlfind query ocsigen.eliom_obrowser_client)
#PAELIOMOBROWSERDIR := $(shell ocamlfind query ocsigen.eliom_obrowser_syntax)
#OCSIMOREOBROWSERDIR := $(shell ocamlfind query ocsimore.client)

TOINSTALL := _build/ocsforge_services_hashtable.cmi

STATICFILES := static/ocsimore_client.uue \
	       static/accept.png\
	       static/cross_octagon.png\
	       static/message_error.png\
	       static/open_repository.png\
	       static/up.png\
	       static/add.png\
	       static/down.png\
	       static/message_warning.png\
	       static/parent_directory.png\
	       static/source_folder.png\
	       static/application_side_list.png\
	       static/highlighter.png\
	       static/ocsforge_sources.css\
	       static/pencil.png\
	       static/sources_background.png\
	       static/wand.png\
	       static/application_side_tree.png\
	       static/magnifier.png\
	       static/preview.png\
	       static/sources_menu_item.png\
	       static/sources_menu_item_hover.png\
               static/ocsforge_title_background.png\
               static/file_history.png\
	       static/source_file.png\
	       static/diff_to_previous.png

all: ocsforge_client.ml $(MYOCAMLFIND) ocsforge static/vm.js _build/ocsforge_client.cmo static/ocsimore_client.uue

ocsforge: $(MYOCAMLFIND) _build/ocsforge_svn.cma
	PGUSER=$(DBUSER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(OCAMLBUILD) $(TARGETS)


$(MYOCAMLFIND): myocamlfind.ml
	$(OCAMLBUILD) -no-plugin $(subst _build/,,$@)

#_build/ocsforge_svn.cma:
#	ocamlc -c swig.mli
#	ocamlc -c swig.ml
#	$(SWIG) swig_svn.i
#	ocamlc -c swig_svn.mli
#	ocamlc -c swig_svn.ml
#	ocamlfind ocamlc -thread -package ocsigen -c ocsforge_source_types.mli
#	ocamlfind ocamlc -thread -package ocsigen -c ocsforge_source_types.ml
#	ocamlfind ocamlc -thread -package ocsigen -c ocsforge_version_managers.ml
#	ocamlc -c ocsforge_source_tree.mli
#	ocamlc -c ocsforge_source_tree.ml
#	ocamlc -c -ccopt "-I/usr/include/subversion-1 -I/usr/include/apr-1.0 -D_LARGEFILE64_SOURCE" swig_svn_wrap.c
#	ocamlc -c -ccopt "-I/usr/include/subversion-1 -I/usr/include/apr-1.0 -D_LARGEFILE64_SOURCE" swig_svn.c
#	ocamlmklib -o ocsforge_svn -L/usr/lib swig_svn.o swig_svn_wrap.o \
#	swig.cmo swig_svn.cmo ocsforge_version_managers.cmo ocsforge_source_tree.cmo -lsvn_client-1
#	rm -rf _build
#	mkdir _build
#	mv *.o *.a *.so _build
#	mv *.cm* _build

_build/ocsforge_client.cmo: ocsforge_client.ml
	CAMLLIB=$(OBROWSERDIR) ocamlc -c -I $(ELIOMOBROWSERDIR) $(AXODIR)/AXO.cma ocsforge_client.ml
	mv ocsforge_client.cm[io] _build/

ocsforge_client.ml: ocsforge_client_calls.ml

ocsforge_client_calls.ml: ocsforge_client.p.ml
	camlp4of $(PAELIOMOBROWSERDIR)/pa_eliom_obrowser.cmo $< -o $@ \
	  -client ocsforge_client.ml

static/ocsimore_client.uue: _build/ocsforge_client.cmo
	CAMLLIB=$(OBROWSERDIR) ocamlc -o ocsimore_client $(ELIOMOBROWSERDIR)/eliom_obrowser_client.cma $(OBROWSERDIR)/AXO.cma $(OCSIMOREOBROWSERDIR)/wiki_client.cmo $(OCSIMOREOBROWSERDIR)/forum_client.cmo ./_build/ocsforge_client.cmo
	uuencode ocsimore_client stdout > static/ocsimore_client.uue


static/vm.js: $(OBROWSERDIR)/vm.js
	cp -f $(OBROWSERDIR)/vm.js static
	cp -f $(ELIOMOBROWSERDIR)/eliom_obrowser.js static
	cp -f $(STATICDIR)/ocsiwikistyle.css static
	cp -f $(STATICDIR)/ocsiforumstyle.css static
	cp -f $(STATICDIR)/ocsiadmin.css static
	cp -f $(STATICDIR)/creole_cheat_sheet.png static
#	cp -f $(STATICDIR)/crayon.png static

install:
	$(OCAMLFIND) install ocsforge META _build/ocsforge.cma \
	_build/ocsforge_svn.cma $(TOINSTALL)
	cp -f $(STATICFILES) $(STATICDIR)
	mkdir -p $(DESTDIR)/stublibs
	cp _build/dllocsforge_svn.so $(DESTDIR)/stublibs
	chmod 664 $(DESTDIR)/stublibs/dllocsforge_svn.so


uninstall:
	$(OCAMLFIND) remove ocsforge

clean:
	rm -Rf _build
	rm -f static/*.uue
#	make -C nis_chkpwd clean

.PHONY: all ocamlbuild clean

SHELL=bash

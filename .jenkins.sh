
opam pin add --no-action ocsforge .
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git#master'
opam pin add --no-action eliom 'https://github.com/ocsigen/eliom.git#master'
opam pin add --no-action js_of_ocaml 'https://github.com/ocsigen/js_of_ocaml.git#master'
EDITOR="sed -i s/0.6.1/dev/" opam pin add --no-action macaque 'https://github.com/ocsigen/macaque.git#master'
opam pin add --no-action ocsimore 'https://github.com/ocsigen/ocsimore.git#master'
opam install --deps-only ocsforge
opam install --verbose ocsforge

do_build_doc () {
  # Nothing yet
  echo "No documentation!"
}

do_remove () {
  opam remove --verbose ocsforge
}

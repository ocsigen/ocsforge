opam pin add --no-action ocsforge .
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git#master'
opam pin add --no-action eliom 'https://github.com/ocsigen/eliom.git#master'
EDITOR="sed -i s/0.6.1/dev/" opam pin add --no-action macaque 'https://github.com/ocsigen/macaque.git#master'
opam pin add --no-action ocsimore 'https://github.com/ocsigen/ocsimore.git#master'
opam install --deps-only ocsforge
opam install --verbose ocsforge
opam remove --verbose ocsforge

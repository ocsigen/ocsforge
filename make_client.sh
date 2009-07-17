CAMLLIB=/usr/local/lib/ocaml/3.11.0/obrowser ocamlc -o ocsimore_client /usr/local/lib/ocaml/3.11.0/ocsigen/eliom_obrowser_client.cmo _build/wiki_client.cmo _build/forum/forum_client.cmo ../obrowser/examples/axo/AXOJs.cmo ../obrowser/examples/axo/AXOCom.cmo ../ocsforge/_build/ocsforge_client.cmo
uuencode ocsimore_client stdout > static/ocsimore_client.uue

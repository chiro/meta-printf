default: ver2

mprintf2.cmo: mprintf2.ml
	metaocamlc mprintf2.ml

mprintf3.cmo: mprintf3.ml
	metaocamlc mprintf3.ml

ver1: mprintf.ml main.ml
	metaocamlc mprintf.ml; metaocamlc -o v1 mprintf.cmo main.ml

ver2: mprintf2.cmo main2.ml
	metaocamlc -o v2 mprintf2.cmo main2.ml

ver3: mprintf3.cmo main3.ml
	metaocamlc -o v3 mprintf3.cmo main3.ml

bench: bench.ml bench2.ml bench3.ml bench4.ml mprintf2.cmo mprintf3.cmo
	metaocamlc -o bench mprintf2.cmo bench.ml;
	metaocamlc -o bench2 bench2.ml;
	metaocamlc -o bench3 bench3.ml;
	metaocamlc -o bench4 mprintf3.cmo bench4.ml

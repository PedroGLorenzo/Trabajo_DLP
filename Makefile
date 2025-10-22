
BUILD_DIR := build
.PHONY: all clean build_dir

build_dir:
	mkdir $(BUILD_DIR)

all: build_dir lambda parser lexer main
	ocamlc -o top lambda.cmo parser.cmo lexer.cmo main.cmo
	mv top lexer.ml parser.mli parser.ml *.cmi *.cmo $(BUILD_DIR)

lambda: lambda.ml lambda.mli
	ocamlc -c lambda.mli lambda.ml

parser: parser.mly
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

main: main.ml
	ocamlc -c main.ml

clean:
	rm -rf $(BUILD_DIR)
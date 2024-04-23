test_file := Test/Test.pl

all:
	@ocamlbuild main.native
	@./main.native $(test_file)

clean:
	@ocamlbuild -clean
	@rm -f *~ main.native \#* main
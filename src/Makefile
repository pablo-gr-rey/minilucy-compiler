OCAMLBUILD= ocamlbuild -classic-display \
		-tags debug,annot
# 		-libs unix,nums,aez \
# 		-cflags "-I ../../lib/aez-0.3" \
# 		-lflags "-I ../../lib/aez-0.3" \

TARGET=native
MAIN=main

all: $(MAIN)

native: TARGET := native
native: all
opt: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := byte
byte: all
$(MAIN).byte: byte


$(MAIN): $(MAIN).target
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) $(MAIN).$(TARGET)


clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) *~

cleanall: realclean

OCAMLC = ocamlc
SOURCES = squelette.ml
TARGET = squelette

all: $(TARGET)

$(TARGET): $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(TARGET) $(SOURCES:.ml=.cmo)

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	rm -f $(TARGET) $(SOURCES:.ml=.cmi) $(SOURCES:.ml=.cmo)

# List all tests to run
TESTS = BHeapTest

# Set testing script with optional environment valiables
TESTS_ENVIRONMENT = ./randomTest.sh

# Clean all generated output
clean-am:
	rm -f *.cmi *.cmo *.cmx *.o $(TESTS)
	rm -fR testingResult*

$(TESTS): $(addsuffix .cmx, DetRandom BHeapTest)
	$(OCAMLOPT) $(BFLAGS) $(P4FLAGS) -o $@ $(EXTRA_CMA) ../BinomialHeap.cmxa DetRandom.cmx BHeapTest.cmx

%.cmx: %.ml
	$(OCAMLOPT) -c -I .. $(BFLAGS) $(P4FLAGS) $<

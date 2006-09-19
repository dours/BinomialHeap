TESTS = BHeapTest

TESTS_ENVIRONMENT = $(srcdir)/randomTest.sh

include $(top_srcdir)/m4/Makefile.am.vars

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"

TARGET = 

CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log testingResult* *.cmi *.cmx

include $(top_srcdir)/m4/Makefile.am.targets

# Rules to build tests
BHeapTest: $(addsuffix .cmx, DetRandom BHeapTest)
	$(OCAMLOPT) $(OFLAGS) $(EXTRA_CMXA) $(P4FLAGS) -o $@$(EXEEXT) $(addsuffix .cmx, DetRandom BHeapTest)

include $(top_srcdir)/m4/Makefile.am.rules

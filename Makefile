ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

CPPCOMPILER=-cc $(OCAMLOPTCPP)

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(TCSLIBOBJ)

MODULES=$(OBJDIR)/base.cmx \
        $(OBJDIR)/solvers.cmx \
        $(OBJDIR)/trapo.cmx \
        $(OBJDIR)/cachedtrapo.cmx \
        $(OBJDIR)/taggedtrapo.cmx \
        $(OBJDIR)/cachedtaggedtrapo.cmx \
        $(OBJDIR)/universality.cmx \
        $(OBJDIR)/subsumption.cmx
		
INTERFACES=$(TCSLIBOBJ)/tcsbasedata.cmi $(MODULES:.cmx=.cmi)

all: modules apps tools generators

modules: $(INTERFACES) $(MODULES)

apps: fadecider

tools: transform subtouniv collapse toopennwa

generators: randomnpa randomnpvpa

$(TCSLIBOBJ)/%.cmi:
	make -C $(TCSLIBROOT) all

$(TCSLIBOBJ)/tcslib.cmxa:
	make -C $(TCSLIBROOT) all

$(OBJDIR)/%.cmx: $(SRCDIR)/ramsey/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/ramsey/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/generators/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/tools/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/app/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/app/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<


clean:
	rm -f $(OBJDIR)/*.o \
	      $(OBJDIR)/*.cmx \
		  $(OBJDIR)/*.cmi \
		  $(BINDIR)/*

%: $(OBJDIR)/%.cmx
	$(OCAMLOPT) $(INCLUDES) $(CPPCOMPILER) nums.cmxa $(TCSLIBOBJ)/tcslib.cmxa $(MODULES) -o $(BINDIR)/$@ $(OBJDIR)/$@.cmx

PACKAGE=fadecider.tar

package:
	$(TAR) cvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,^,fadecider/," Makefile Config.default README src bin obj benchmarks
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,TCSlib,fadecider/TCSlib," $(TCSLIBROOT)/obj $(TCSLIBROOT)/src $(TCSLIBROOT)/Makefile $(TCSLIBROOT)/Config.default
	$(GZIP) $(PACKAGE)

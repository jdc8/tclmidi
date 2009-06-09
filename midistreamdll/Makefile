TCLDIR = c:/Tcl

TCLINCDIR = $(TCLDIR)/include
TCLLIBDIR = $(TCLDIR)/lib
TCLLIB = $(TCLLIBDIR)/tcl85.lib

all: midistreamdll.dll

midistreamdll.dll : midistreamdll.obj
	link /dll /out:midistreamdll.dll midistreamdll.obj odbc32.lib odbccp32.lib $(TCLLIB) winmm.lib

midistreamdll.obj : midistreamdll.c
	cl /c midistreamdll.c /I$(TCLINCDIR)

clean:
	del midistreamdll.obj midistreamdll.exp midistreamdll.lib midistreamdll.dll.manifest

distclean: clean
	del midistreamdll.dll

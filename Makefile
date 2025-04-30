.SUFFIXES: .F .o

all: dummy UGWP_physics

dummy:
	echo "****** compiling UGWP_physics ******"

OBJS = \
	bl_ugwp.o       \
	bl_ugwpv1_ngw.o \
	cires_ugwpv1_initialize.o \
	cires_ugwpv1_module.o \
	cires_tauamf_data.o \
	cires_ugwpv1_triggers.o \
	cires_ugwpv1_solv2.o

# DEPENDENCIES:
bl_ugwpv1_ngw.o: \
	cires_ugwpv1_module.o \
	cires_tauamf_data.o \
	cires_ugwpv1_triggers.o \
	cires_ugwpv1_solv2.o

cires_tauamf_data.o: \
        cires_ugwpv1_initialize.o

cires_ugwpv1_module.o: \
	cires_ugwpv1_initialize.o \
	cires_tauamf_data.o

cires_ugwpv1_solv2.o: \
	cires_ugwpv1_module.o \
	cires_ugwpv1_initialize.o

UGWP_physics: $(OBJS)
	ar -ru ./../../libphys.a $(OBJS)

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F.o:
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(COREDEF) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../.. -I../../../../framework -I../../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I../.. -I../../../../framework -I../../../../external/esmf_time_f90
endif

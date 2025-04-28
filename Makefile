.SUFFIXES: .F .o

all: dummy check_for_physics_gsl UGWP_physics

dummy:
	echo "****** compiling UGWP_physics ******"

check_for_physics_gsl:
	echo "*** Checking if UGWP is under 'physics_gsl' directory ***"
	echo "physics_gsl_exists="$(PHYSICS_GSL_EXISTS)
ifeq ($(wildcard ../../physics_gsl/.),)
   PHYSICS_GSL_EXISTS := false
else
   PHYSICS_GSL_EXISTS := true
endif

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
ifeq "$(PHYSICS_GSL_EXISTS)" "true"
	@# UGWP submodule is located in 'src/core_atmosphere/physics/physics_gsl/UGWP' directory
	ar -ru ./../../libphys.a $(OBJS)
else
	@# UGWP submodule is located in 'src/core_atmosphere/physics/UGWP' directory'
	ar -ru ./../libphys.a $(OBJS)
endif

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F.o:
ifeq "$(PHYSICS_GSL_EXISTS)" "true"
	@# UGWP submodule is located in 'src/core_atmosphere/physics/physics_gsl/UGWP' directory
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(COREDEF) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../.. -I../../../../framework -I../../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I../.. -I../../../../framework -I../../../../external/esmf_time_f90
endif
else
	@# UGWP submodule is located in 'src/core_atmosphere/physics/UGWP' directory'
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(COREDEF) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I.. -I../../../framework -I../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I.. -I../../../framework -I../../../external/esmf_time_f90
endif
endif

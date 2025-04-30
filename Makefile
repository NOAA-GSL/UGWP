.SUFFIXES: .F .o

all: dummy check_for_physics_noaa UGWP_physics

dummy:
	echo "****** compiling UGWP_physics ******"

check_for_physics_noaa:
	echo "*** Checking if UGWP is under 'physics_noaa' directory ***"
	echo "physics_noaa_exists="$(PHYSICS_NOAA_EXISTS)
ifeq ($(wildcard ../../physics_noaa/.),)
   PHYSICS_NOAA_EXISTS := false
else
   PHYSICS_NOAA_EXISTS := true
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
ifeq "$(PHYSICS_NOAA_EXISTS)" "true"
	@# UGWP submodule is located in 'src/core_atmosphere/physics/physics_noaa/UGWP' directory
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
ifeq "$(PHYSICS_NOAA_EXISTS)" "true"
	@# UGWP submodule is located in 'src/core_atmosphere/physics/physics_noaa/UGWP' directory
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

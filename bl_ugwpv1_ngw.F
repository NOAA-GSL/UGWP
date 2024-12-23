module bl_ugwpv1_ngw

! This module contains the UGWPv1 non-stationary gravity wave drag (NGW) scheme.
!      1) The "V1 CIRES UGWP" scheme as tested in the FV3GFSv16-127L atmosphere model and workflow, which includes
!               the v1 CIRES ugwp non-stationary GW scheme, new revision that generate realistic climate of FV3GFS-127L
!               in the strato-mesosphere in the multi-year simulations (Annual cycles, SAO and QBO in th tropical dynamics).
! See Valery Yudin's presentation at 2020 UFS User's meeting (Jul 2020):
! Gravity waves (GWs): Mesoscale GWs transport momentum, energy (heat) , and create eddy mixing in the whole atmosphere domain; Breaking and dissipating GWs deposit: (a) momentum; (b) heat (energy); and create (c) turbulent mixing of momentum, heat, and tracers
! To properly incorporate GW effects (a-c) unresolved by DYCOREs we need GW physics
! "Unified": a) all GW effects due to both dissipation/breaking; b) identical GW solvers for all GW sources; c) ability to replace solvers.
! Unified Formalism:
! 1. GW Sources: Stochastic and physics based mechanisms for GW-excitations in the lower atmosphere, calibrated by the high-res analyses/forecasts, and observations (3 types of GW sources: orography, convection, fronts/jets).
! 2. GW Propagation: Unified solver for "propagation, dissipation and breaking" excited from all type of GW sources.
! 3. GW Effects: Unified representation of GW impacts on the "resolved" flow for all sources (energy-balanced schemes for momentum, heat and mixing).
! https://www.weather.gov/media/sti/nggps/Presentations%202017/02%20NGGPS_VYUDIN_2017_.pdf
!
!


use ccpp_kind_types,     only: kind_phys
use cires_ugwpv1_module, only: cires_ugwpv1_init, ngwflux_update
use cires_ugwpv1_solv2,  only: cires_ugwpv1_ngw_solv2


contains


subroutine ugwpv1_ngw_init (lat_r,levs,dtp,rdzw,dzu,ntau_d1y,knob_ugwp_tauamp,    &
                            ugwp_taulat,jindx1_tau,jindx2_tau,ddy_j1tau,ddy_j2tau)

   use ugwp_common
   use mpas_atmphys_constants, only : P0
   use mpas_constants, only : pii, gravity, omega, a, cp, rgas, rv_moist=>rv
   use cires_tauamf_data,   only:  cires_indx_ugwp

   implicit none

   real(kind=kind_phys), dimension(:), intent(in) :: lat_r  ! latitude in radians
   integer, intent(in) :: levs     ! number of model levels
   real(kind=kind_phys), intent(in) :: dtp     ! physics time step
   real(kind=kind_phys), dimension(:), intent(in) :: rdzw ! inverse delta-zeta at u-levels
   real(kind=kind_phys), dimension(:), intent(in) :: dzu  ! delta-zeta at w-levels
   integer, intent(in) :: ntau_d1y
   real(kind=kind_phys), intent(in) :: knob_ugwp_tauamp
   real(kind=kind_phys), dimension(:), intent(in) :: ugwp_taulat
   integer, dimension(:), intent(inout) :: jindx1_tau, jindx2_tau
   real(kind=kind_phys), dimension(:), intent(inout) :: ddy_j1tau, ddy_j2tau

   integer :: ios
   logical :: exists
   real    :: dxsg
   integer :: k

   real(kind=kind_phys) :: p0_std

   real(kind=kind_phys), dimension(levs) :: zu  ! vert. coord. zeta at u-levels centers (m)


   pi    = pii
   arad  = a        ! Radius of earth (m)
   p0_std = 101325._kind_phys  ! Standard atmospheric pressure (Pa) - Note P0 = ref pressure (10e5)
   grav  = gravity
   omega1= omega
   cpd   = cp
   rd    = rgas
   rv    = rv_moist
   fv    = rv/rd-1._kind_phys

   grav2  = grav + grav; rgrav  = 1._kind_phys/grav ; rgrav2 = rgrav*rgrav
   rdi    = 1._kind_phys / rd ; rcpd = 1._kind_phys/cpd
   rcpd2  = 0.5_kind_phys/cpd
   gor    = grav/rd
   gr2    = grav*gor
   grcp   = grav*rcpd
   gocp   = grcp
   rcpdl  = cpd*rgrav
   grav2cpd = grav*grcp

   pi2      = 2._kind_phys*pi ;  pih = .5_kind_phys*pi
   rad_to_deg=180.0_kind_phys/pi
   deg_to_rad=pi/180.0_kind_phys

   bnv2min = (pi2/1800._kind_phys)*(pi2/1800._kind_phys)
   bnv2max = (pi2/30._kind_phys)*(pi2/30._kind_phys)
   dw2min  = 1.0_kind_phys
   velmin  = sqrt(dw2min)
   minvel  = 0.5_kind_phys

   omega2  = 2._kind_phys*omega1
   omega3  = 3._kind_phys*omega1

   hpscale = 7000._kind_phys ; hpskm = hpscale*1.e-3_kind_phys
   rhp     = 1._kind_phys/hpscale
   rhp2 = 0.5_kind_phys*rhp; rh4 = 0.25_kind_phys*rhp
   rhp4 = rhp2 * rhp2
   khp  = rhp* rd/cpd
   mkzmin  = pi2/80.0e3_kind_phys
   mkz2min = mkzmin*mkzmin
   mkzmax  = pi2/500._kind_phys
   mkz2max = mkzmax*mkzmax
   cdmin   = 2.e-2_kind_phys/mkzmax

   rcpdt  = rcpd/dtp

   ! Create array of vertical coordinate zeta values at layer centers
   zu(1) = 0.5_kind_phys/rdzw(1)     
   do k = 2,levs
      zu(k) = zu(k-1) + dzu(k)
   enddo

   call cires_ugwpv1_init (levs, zu, p0_std, dtp, knob_ugwp_tauamp)

   ! Calculate spatial interpolation weights for NGW tau_amf
   call cires_indx_ugwp(pi,lat_r,ntau_d1y,ugwp_taulat,               &
                        jindx1_tau,jindx2_tau,ddy_j1tau,ddy_j2tau)

end subroutine ugwpv1_ngw_init


subroutine ugwpv1_ngw_run(xlatd,raincv,rainncv,ddy_j1tau,ddy_j2tau,     &
                jindx1_tau,jindx2_tau,r_DoY,kdt,dtp,ugrs,vgrs,          &
                tgrs,q1,prsl,prslk,prsi,zl,zi,ntau_d2t,days_limb,       &
                tau_limb,rublten,rvblten,rthblten,                      &
                ugwp_diags,dudt_ngw,dvdt_ngw,dtdt_ngw,its,ite,levs)

   use ugwp_common
   use ugwp_wmsdis_init,      only: tamp_mpa
   use cires_tauamf_data,     only: tau_amf_interp
   use cires_ugwpv1_triggers, only: slat_geos5_2020

   implicit none

   real(kind=kind_phys), intent(in) :: xlatd(:),ddy_j1tau(:),ddy_j2tau(:)
   real(kind=kind_phys), intent(in) ::            &
                     raincv(:),  &   ! time-step convective precipitation (mm)
                     rainncv(:)      ! time-step grid-scale precipitation (mm)
   real(kind=kind_phys), intent(in) ::            &
                     ugrs(:,:),  &   ! zonal wind (m/s)
                     vgrs(:,:),  &   ! meridional wind (m/2)
                     tgrs(:,:),  &   ! temperature (K)
                     q1(:,:),    &   ! water vapor mixing ratio (kg/kg)
                     prsl(:,:),  &   ! mid-layer pressure (Pa)
                     prslk(:,:), &   ! mid-layer dimensionless Exner function (-)
                     prsi(:,:),  &   ! interface pressure (Pa)
                     zl(:,:),    &   ! mid-layer geopotential height (m)
                     zi(:,:)         ! interface geopotential height (m)
                   
   real(kind=kind_phys), intent(in) :: r_DoY,dtp

   integer, intent(in) :: jindx1_tau(:),jindx2_tau(:)
   integer, intent(in) :: kdt,its,ite,levs

   logical, intent(in) :: ugwp_diags

   integer, intent(in) :: ntau_d2t
   real(kind=kind_phys), intent(in) :: days_limb(:), tau_limb(:,:)

   real(kind=kind_phys), intent(out), optional :: dudt_ngw(:,:),dvdt_ngw(:,:),  &
                                                  dtdt_ngw(:,:)

   real(kind=kind_phys), intent(inout) :: rublten(:,:),rvblten(:,:),rthblten(:,:)


   integer :: im,k

   real(kind=kind_phys), dimension(its:ite) ::    &
                         tau_ngw,  &   ! momentum flux due to nonstationary gravity waves
                         tau_amf,  &   ! source NGW momentum flux
                         rain          ! total rain at this time step (m)

   real(kind=kind_phys), dimension(ite-its+1,levs) ::  & 
                                        pdudt_ngw,pdvdt_ngw,pdtdt_ngw,kdis_ngw
   real(kind=kind_phys), dimension(ite-its+1) :: zngw



   ! Initialize ngw tendencies
   pdudt_ngw = 0.0      ; pdvdt_ngw = 0.0 ;      pdtdt_ngw = 0.0

   ! Initialize ngw diagnostics
   kdis_ngw = 0.0      ; zngw = 0.0     ! NOTE:  These variables may be output at a later date


   ! Initialize optional diagnostic output
   if ( ugwp_diags ) then
       dudt_ngw = 0.0 ;   dvdt_ngw = 0.0 ;   dtdt_ngw = 0.0
   endif


   im = ite-its+1
   tau_ngw(:) = 0.
   rain(:) = (raincv(:)+rainncv(:))*0.001_kind_phys  ! converting to meters


   ! Interpolate NGW sources 'tau_amf' in space and time
   call tau_amf_interp(im, r_DoY, jindx1_tau, jindx2_tau,  &
                       ddy_j1tau, ddy_j2tau, ntau_d2t,     &
                       days_limb, tau_limb, tau_amf)


   !==================================================================
   !       call slat_geos5_tamp_v1(im, tamp_mpa, xlatd, tau_ngw)
   !
   ! 2020 updates of MERRA/GEOS tau_ngw for the C96-QBO FV3GFS-127L runs
   !==================================================================
   call slat_geos5_2020(im, tamp_mpa, xlatd, tau_ngw)

   call ngwflux_update(im, tau_amf, xlatd, rain, tau_ngw)

   call cires_ugwpv1_ngw_solv2(im, levs, kdt, dtp, tau_ngw, tgrs,        &
                  ugrs, vgrs, q1, prsl, prsi, zl, zi, prslk,             &
                  xlatd, pdudt_ngw, pdvdt_ngw, pdtdt_ngw, kdis_ngw, zngw)

   ! Convert Dt/Dt to D(theta)Dt and add to rthblten
   do k = 1,levs
      rthblten(:,k) = rthblten(:,k) + pdtdt_ngw(:,k)/prslk(:,k)
   enddo

   ! Update u,v tendencies
   do k = 1,levs
      rublten(:,k) = rublten(:,k) + pdudt_ngw(:,k)
      rvblten(:,k) = rvblten(:,k) + pdvdt_ngw(:,k)
   enddo


   ! Save optional diagnostics
   if ( ugwp_diags ) then
      dudt_ngw = pdudt_ngw
      dvdt_ngw = pdvdt_ngw
      dtdt_ngw = pdtdt_ngw
   endif



return
end subroutine ugwpv1_ngw_run


end module bl_ugwpv1_ngw

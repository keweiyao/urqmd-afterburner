ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c     HEAVY MESON RELATED PARAMETERS, COMMOM BLOCKS
c
c         last change: Mar 30, Yingru XU 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      integer NUMSAMP    ! events containing particles
      parameter (NUMSAMP=1)


      integer MAXP
      parameter (MAXP = 2000000)   ! max number of Dmeson per event

c! event header reader
      integer hq_event, hq_itim, hq_ntim
      real*8  hq_imppar, hq_angle
      common /eventheader/ hq_event, hq_itim, hq_imppar, hq_angle

      integer hq_npart, noversamples, hq_per_event
      common /outputhq/ hq_npart, noversamples, hq_per_event

c! for Dmeson information
c! put Dmeson list and light hardron list into one
!      integer hq_num(MAXP), hq_id(MAXP)
!      real*8  hq_px(MAXP), hq_py(MAXP), hq_pz(MAXP), hq_p0(MAXP),
!     &        hq_rx(MAXP), hq_ry(MAXP), hq_rz(MAXP), hq_r0(MAXP),
!     &        hq_mass(MAXP), hq_ipT(MAXP), hq_wt(MAXP)

!      common /HQmeson/ hq_num,hq_id, hq_px, hq_py, hq_pz, hq_p0, hq_rx, 
!     &                hq_ry, hq_rz, hq_r0, hq_mass, hq_ipT, hq_wt
      

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      program o2u
c
c     Author   : Steffen A. Bass
c     Date     : 06/05/98
c
c     oscar to urqmd file14 converter
c                    
c     input : oscar file (stdin)
c             
c
c     output: urqmd file14 (fort.14)
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
      implicit none
      include 'ucoms.f'
      include 'ucoms_HQ.f'

      integer iret,tstep,procev,i

      character*1 echar
      character*77 file10, file20
      

      procev=0

cccccccc array initializations  ccccccccccccccccc

      call sseed(123987)

      do 111 i=1,numcto
         CTOdc(i)='  '
 111  continue
      do 112 i=1,numctp
         CTPdc(i)='  '
 112  continue

c default settings for CTParam and CTOption cccccccccccccccccccccccccccccc
      CTParam(1)=1.d0  
      CTParam(2)=0.52d0 
      CTParam(3)=.5d0 
      CTParam(4)=0.3d0 
      CTParam(5)=0d0 
      CTParam(6)=0.37d0    
      CTParam(7)=0.d0 
      CTParam(8)=0.093d0 
      CTParam(9)=0.35d0 
      CTParam(10)=0.25d0 
      CTParam(11)=0.d0 
      CTParam(12)=.5d0  
      CTParam(13)=.27d0 
      CTParam(14)=.49d0 
      CTParam(15)=.27d0 
      CTParam(16)=1.0d0 
      CTParam(17)=1.6d0 
      CTParam(18)=.85d0 
      CTParam(19)=1.55d0
      CTParam(22)=1.0d0
      CTParam(25)=.9d0 
      CTParam(26)=50d0 
      CTParam(27)=1d0 
      CTParam(28)=1d0 
      CTParam(29)=0.4 
      CTParam(30)=1.5 
      CTParam(31)=1.6d0 
      CTParam(32)=0d0
      CTParam(33)=2.5d0
      CTParam(34)=0.1
      CTParam(35)=3.0
      CTParam(36)=0.275d0
      CTParam(37)=0.42d0
      CTParam(38)=1.08d0
      CTParam(39)=0.8d0
      CTParam(40)=0.5d0
      CTParam(41)=0.0
      CTParam(42)=0.55d0
      CTParam(43)=5.d0
      CTParam(44)=.8d0
      CTParam(45)=0.5
      CTParam(46)=800000
      CTParam(47)=1.0
      CTParam(48)=2.0

      CTParam(50)=1d0 
      CTParam(51)=1d0 
      CTParam(52)=1d0
      CTParam(53)=1d0
      CTParam(54)=1d0 
      CTParam(55)=1d0
      CTParam(56)=1d0
      CTParam(57)=1d0
      CTParam(58)=1.d0
      CTOption(1)=0  
      CTOption(2)=0
      CTOption(3)=0  
      CTOption(4)=0  
      CTOption(5)=0  
      CTOption(6)=0  
      CTOption(7)=0  
      CTOption(8)=0  
      CToption(9)=0  
      CTOption(10)=0 
      CTOption(11)=0 
      CTOption(12)=0 
      CTOption(13)=0 
      CTOption(14)=0 
      CTOption(15)=0 
      CTOption(16)=0 
      CTOption(17)=0 
      CTOption(18)=0 
      CTOption(19)=0  
      CTOption(20)=0
      CTOption(21)=0
      CTOption(22)=1
      CTOption(23)=0
      CTOption(24)=1
      CTOption(25)=0
      CTOption(26)=0
      CTOption(27)=0 
      CTOption(28)=0
      CTOption(29)=2
      CTOption(30)=1
      CTOption(31)=0
      CTOption(32)=0
      CTOption(33)=0
      CTOption(34)=1
      CTOption(35)=1
      CTOption(36)=0
      CTOption(37)=0
      CTOption(38)=0
      CTOption(39)=0
      CTOption(40)=0
      CTOption(41)=0
      CTOption(42)=0
      CTOption(43)=0

ccccccccccccccccccccccccccccccccccccccccccccccccc
! read in the light hadron fileName
c      file10 = '    '
c      call getenv('ftn10',file10)
c      if (file10(1:4) .ne. '    ') then
c          open(UNIT=10,FILE=file10,STATUS='old',FORM='formatted')
c      else
c          write(6,*) "No light hadron list provided"
c          return
c      endif

! read in heavy meson fileName
      file20 = '    '
      call getenv('ftn20',file20)
      if (file20(1:4) .ne. '    ') then
          open(UNIT=20,FILE=file20,STATUS='old',FORM='formatted')
      else
          write(6,*) "No heavy meson list provided"
      endif


! read in headers      

      if (file20(1:4) .ne. '    ') then
          call read_HQmeson_header(iret)
          if (iret .eq. 0) stop
      endif

! a bit more comment on this part
! for each event, since right now all the oversampled light hadrons are store into different oversampled events
! while the Dmesons are stored in a larger one event list
! current way to solve this is to evenly distributed Dmesons into each oversampled events (if we believe the assumption
! that each oversampled events have similar multiplicities)

      call readInputFromCML()
      hq_per_event = int(hq_npart / noversamples) 

      

 1    continue

	  event = 0
      call read_event(iret)
      if(iret.eq.0) stop

      if (file20(1:4) .ne. '    ') then
          call read_HQmeson_event(iret, procev)
          if(iret .eq. 0) stop
      endif




cdebug
c      if(procev.gt.100) stop

      procev=procev+1

cdebug
c      if(procev.le.101) goto 1

c     process the event
      call procevent(tstep)




      call write_uheader(14)
      call file14out(tstep)

      event = event + 1

      goto 1

      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine write_uheader(iunit)

      implicit none

      include 'ucoms.f'
      include 'comres.f'

c
      integer iunit,i,ttime,out_time
      integer id
      integer itotcoll,iinelcoll
      real*8 sigmatot,otime


      character*20 aa,ah,ai,ak
      character*36 ae,abt
      character*31 aee
      character*15 ab,aj,al,am
      character*13 ac,ag,pds,tds
      character*12 ad
      character*7 af
      character*9 ag2
      character*1 add
      character*171 apav
      character*2 apa,aop


     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c              output formats
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c fileheader
 101  format(a20,3i7,a15,i2)
 301  format(a13,a13,i4,i4,a12,a13,i4,i4,a1)
c 305  format(a36,3f10.7)
 304  format(a36,3f6.2,a31,1f9.2)
 302  format (a7,i9,a13,i12,a9,a20,i7,a20,f11.3)
c 303  format(a20,i3,a15,e10.4,a15,e10.4,a15,e10.4)
 102  format(a2,15(i3,a2))
c 103  format(a2,12(e10.4,a2))
 306  format(a171)

 305  format(a36,3f11.7)
 303  format(a20,i3,a15,e11.4,a15,e11.4,a15,e11.4)
 103  format(a2,12(e11.4,a2))


csab changed e16.8 to D24.16
c standard particle information vector
 201  format(9e16.8,i11,2i3,i9,i5,i4)
cLHC 201  format(9e24.16,i11,2i3,i9,i5,i4)

c special output for cto40 (restart of old event)
! 210  format(9e16.8,i11,2i3,i9,i5,i10,3e16.8,i8)
 210  format(9e16.8,i11,2i3,i9,i5,i10,5e16.8)

c collsision stats for file14
 202  format(8i8)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

c
      aa='UQMD   version:     '
      ab='  output_file '
      abt='transformation betas (NN,lab,pro) '
      ac='projectile:  '
      ad='   target: '
      add=' '
      ae='impact_parameter_real/min/max(fm):  '
      aee='  total_cross_section(mbarn):  '
      af='event# '
      ag=' random seed:' 
      ah='equation_of_state: '
      ai=' total_time(fm/c): '
      aj='  E_lab(GeV/u):'
      ak=' Delta(t)_O(fm/c): '
      al='  sqrt(s)(GeV):'
      am='  p_lab(GeV/u):'
      apa='pa'
      aop='op'

      apav='pvec: '//
     & 'r0              rx              ry              rz          '//
     & '    p0              px              py              pz      '//
     & '        m          ityp 2i3 chg lcl#  ncl or'    
      

      ag2=' readin  '
         
      pds='(mass, char) '
      tds='(mass, char) '


c cross section of the projectile-target system not given in oscar
      sigmatot = 0d0
ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      otime=0.d0
      ttime=0

      write(*,101) aa,version, sigver, laires, ab,iunit
      write(*,301) ac,pds, Ap, Zp, ad,tds, At, Zt,add
      write(*,305) abt,betann,betatar,betapro
      write(*,304) ae,bimp,bmin,bdist,aee,sigmatot
      write(*,303) ah,eos,aj,ebeam,al,ecm,am,pbeam
      write(*,302) af,event,ag,ranseed,ag2,ai,ttime,ak,otime
      write(*,102) aop,(CTOption(i),CTOdc(i),i=1,15)
      write(*,102) aop,(CTOption(i),CTOdc(i),i=16,30)
      write(*,102) aop,(CTOption(i),CTOdc(i),i=31,45)
      write(*,103) apa,(CTParam(i),CTPdc(i),i=1,12)
      write(*,103) apa,(CTParam(i),CTPdc(i),i=13,24)
      write(*,103) apa,(CTParam(i),CTPdc(i),i=25,36)
      write(*,103) apa,(CTParam(i),CTPdc(i),i=37,48)
      write(*,306) apav
c 
      return

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      entry file14out(out_time)
c
c     Unit     : Collision Term
c     Author   : Steffen A. Bass (new source)
c     Date     : 01/09/95
c     Revision : 0.1 beta - uncompleted
c
c     This subroutine writes the standard output-file (unit 14)
c                    
c     input : 
c             timestep  : timestep of output
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c

c
 
      itotcoll=ctag-dectag
      iinelcoll=itotcoll-NBlColl-NElColl
      write(*,*) npart,out_time
      write(*,202) itotcoll,NElColl,iinelcoll,NBlColl,dectag,
     @     NHardRes,NSoftRes,NDecRes

c now write particle-output

      do 31 i=1,npart
         write(*,210) r0(i),rx(i),ry(i),rz(i),p0(i),
     @        px(i),py(i),
     @        pz(i),fmass(i),
     @        ityp(i),iso3(i),charge(i),
     @        lstcoll(i),ncoll(i),origin(i),
     @        dectime(i),thad(i),xtotfac(i)
     &        ,t_ipT(i), t_iy(i)

 31   continue

c 
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine read_event(iret)

      implicit none
      include 'ucoms.f'

      character comment
      integer i,iret

      iret=1

      ! number of particles in event
      read(*,*,err=299,end=299) comment, lq_npart

      ! particle data
      do 99 i=1,lq_npart
         read(*,*) t_ityp(i),
     .        t_r0(i), t_rx(i), t_ry(i), t_rz(i),
     .        t_p0(i), t_px(i), t_py(i), t_pz(i)
         t_fmass(i) = sqrt(t_p0(i)**2
     .        - t_px(i)**2 - t_py(i)**2 - t_pz(i)**2)
 99   continue

      return

 299  continue
      iret=0
      return

	  end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c added by Yingru Xu, Mar 30, 2017
c>>>>>>>>>>>>>>> read in heavy meson header
      subroutine read_HQmeson_header(iret)
      implicit none
      include 'ucoms_HQ.f'

      integer iret, nstep, dum_NUMSAMP
      integer Ap, At, Zp, Zt
      real*8 ebeam
      character*12 oscar_tag, file_tag
      character*8 model_tag, version_tag
      character*1 cdummy1
      character*3 cdummy3
      character*4 reffram


      iret = 1

      read (20,481,err=599,end=599) oscar_tag
      read (20,481,err=599,end=599) file_tag

 481  format (a12)


      read (20,490,err=599,end=599)
     &             model_tag, version_tag, cdummy1, Ap, cdummy1,
     &             Zp, cdummy3, At, cdummy1, Zt, cdummy1,
     &             reffram, ebeam, nstep


 490   format (2(a8,2x),a1,i3,a1,i6,a3,i3,a1,i6,a1,2x,a4,2x,
     &     e10.4,2x,i8)



      hq_event = 0
      hq_npart = 0
      read (20,500,err=599,end=599)
     &    hq_event,hq_npart,hq_imppar,hq_angle,hq_itim,dum_NUMSAMP

 500  format(i10,2x,i10,2x,f8.3,2x,f8.3,2x,i4,2x,i4,2X,i7)

       return

 599  continue
       iret = 0
       write(6,*) "ERROR while read in heavy meson header "
       write(6,*) "terminating ..."
       return

      end
c<<<<<<<<<<<<< end of read in heavy meson header

c>>>>>>>>>>>>>>>>>>read in heavy meson list
      subroutine read_HQmeson_event(iret, ievent)
      implicit none
      include 'ucoms_HQ.f'
      include 'ucoms.f'


      integer i,j,iret, ievent, hq_this_event
      double precision dummy, init_px, init_py, init_pz, init_p0
! now read in heavy meson particles

      hq_this_event = hq_per_event

      if (ievent .eq. (noversamples-1)) then
          hq_this_event = hq_npart - hq_per_event*ievent
      endif

!      write(6,*) "read in ", hq_this_event,
!     &       " heavy meson in event ", ievent

      npart = lq_npart + hq_this_event
!      write(6,*) "light hadrons: ", lq_npart
!      write(6,*) "total hadrons: ", npart

      do 899 i=lq_npart+1, npart
        read(20,8921) j,t_ityp(i),
     &    t_px(i), t_py(i), t_pz(i), t_p0(i), t_fmass(i),
     &    t_rx(i), t_ry(i), t_rz(i), t_r0(i),
     &    dummy, dummy, dummy, dummy, init_px,
     &    init_py, init_pz, init_p0
		t_ipT(i) = sqrt(init_px**2 + init_py**2)
		t_iy(i) = 0.5*log((init_p0+init_pz)/(init_p0-init_pz))
       !!write(6,*) ievent, j, t_ityp(i)

 899  continue

 8921 format(i10,2x,i10,17(2x,d12.6))
      return

 8199 continue
      iret = 0
      write (6,*) "ERROR while read in heavy meson list..."
      write (6,*) "terminating ..."
      return

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine readInputFromCML()
      ! read inputs from command line
      implicit none
      include 'ucoms_HQ.f'

      character*60 buffer
      character*20 varName

      integer DResult
      integer QNum, ArgIndex

      QNum = iargc()

      Do ArgIndex = 1, QNum
        call getarg(ArgIndex, buffer)
        call processAssignment(buffer, "=", varName, DResult)

        if (varName .eq. "nsamples") noversamples = DResult
      enddo

      end


      subroutine processAssignment(string, seperator, varName, DResult)

      Implicit None
      Character :: seperator
      Character (*) :: string, varName
      Character*60 :: LHS, RHS
      Integer:: DResult

      Integer:: break_here, i, cha

      varName = ""
      break_here = index(string, seperator)
      LHS = adjustl(string(:break_here-1))
      RHS = adjustl(string(break_here+1:))

      Do i=1, len_trim(LHS)
          cha = ichar(LHS(i:i))
          if (cha >=65 .AND. cha < 90) then
              varName(i:i) = char(cha +32)
          else
              varName(i:i) = LHS(i:i)
          endif
      enddo

      Read(RHS, fmt='(I10)') DResult

      end







cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c end of Yingru's modification


C     Last change:      REG  27 Apr 2006 and 5 Jan 2006
C     Previous change:  BCM   4 Oct 2002    1:59 pm
C
C THIS SUBROUTINE PERFORMS THE ALLOCATION OF AR-NONSEASONAL ROOTS
C TO THE COMPONENTS
C
C      INPUT PARAMETERS
C
C         P : DIMENSION OF AR NONSEASONAL MODEL (NUMBER OF ROOTS)
C       IMZ : IMAGINARY PART OF ROOTS
C       REZ : REAL PART OF ROOTS
C        AR : PERIOD OF THE ROOTS
C     EPSHI : IS A CONSTANT (SEE MANUAL)
C        MQ : FREQUENCY
C     CYCNS : NON-STATIONARY CYCLE DENOMINATOR (true signs)
C    NCYCNS : DIMENSION OF CYCNS
C     PSINS : NON-STATIONARY SEASONAL DENOMINATOR (true signs)
C    NPSINS : DIMENSION OF PSINS
C      CYCS : STATIONARY CYCLE DENOMINATOR (true signs)
C     NCYCS : DIMENSION OF CYCS
C     CHINS : NON-STATIONARY TREND DENOMINATOR (true signs)
C    NCHINS : DIMENSION OF CHINS
C      CHIS : STATIONARY TREND DENOMINATOR (true signs)
C     NCHIS : DIMENSION OF CHIS
C     MODUL : MODULUS OF THE ROOTS
C      PSIS : STATIONARY SEASONAL DENOMINATOR (true signs)
C     NPSIS : DIMENSION OF PSIS
C
      subroutine F1RST(p,imz,rez,ar,epsphi,mq,cycns,ncycns,psins,npsins,
     $                 cycs,ncycs,chins,nchins,chis,nchis,modul,psis,
     $                 npsis,rmod,root0c,rootPIc,rootPIs,IsCloseTOTD)
C
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer p,mq,ncycns,npsins,ncycs,nchins,nchis,npsis
      real*8 imz(64),rez(64),ar(64),epsphi,cycns(5),psins(27),cycs(17),
     $       chins(8),chis(5),modul(64),psis(16),rmod
      logical root0c,rootPIc,rootPIs,IsCloseToTD
C
C.. Local Scalars ..
      integer i,intocycle,neps,ny
      real*8 k,RmodS
C
C.. Local Arrays ..
      real*8 dum(80)
C
C.. External Calls ..
      logical IsCloseTD
      external CONV,IsCloseTD
C
C.. Intrinsic Functions ..
      intrinsic ABS, DBLE
C
C ... Executable Statements ...
C
C
      if (p .eq. 0) return
C
C TALE CHECK E' CORRETTO PERCHE' LE RADICI SONO IN ORDINE
C PRIMA LE COMPLESSE POI LE REALI. DUNQUE SE LA PRIMA E' REALE
C ALLORA SONO TUTTE REALI.
C
      ny=2
      if (nPSIns.gt.1 .or. nPSIs.gt.1) then
        rmodS=rmod
      else
        rmodS=0.9d0
      end if
      if (-1.0d-13.lt.imz(1) .and. imz(1).lt.1.0d-13) then
       do i = 1,p
        dum(1) = 1.0d0
        dum(2) = -rez(i)
        if (rez(i) .le. 0.0d0) then
         if (ABS(1.0d0+rez(i)) .lt. 1.0d-06) then
          if (mq .eq. 1) then
           rootPIc=.TRUE.
           call CONV(dum,2,cycns,ncycns,cycns,ncycns)
          else
           rootPIc=.TRUE.
           call CONV(dum,2,psins,npsins,psins,npsins)
          end if
         else if (mq .eq. 1) then
          rootPIc=.TRUE.
          call CONV(dum,2,cycs,ncycs,cycs,ncycs)
         else if (ABS(rez(i)) .ge. RmodS) then
          rootPIc=.TRUE.
          call CONV(dum,2,psis,npsis,psis,npsis)
         else
          rootPIc=.TRUE.
          call CONV(dum,2,cycs,ncycs,cycs,ncycs)
         end if
        else if (ABS(1.0d0-rez(i)) .lt. 1.0d-06) then
         call CONV(dum,2,chins,nchins,chins,nchins)
        else if (ABS(rez(i)) .ge. rmod) then
         call CONV(dum,2,chis,nchis,chis,nchis)
        else
         rootPIc=.TRUE.
         call CONV(dum,2,cycs,ncycs,cycs,ncycs)
        end if
       end do
C
C RADICI COMPLESSE
C
      else
       dum(1) = 1.0d0
       dum(2) = -2*rez(1)
       dum(3) = rez(1)**2 + imz(1)**2
       if (mq .eq. 1) then
        if ((modul(1).gt.rmod) .and. 
     $        (ABS(ar(1)).lt.360d0/dble(ny*MQ)))then
         if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
          call CONV(dum,3,chins,nchins,chins,nchins)
         else
          call CONV(dum,3,chis,nchis,chis,nchis)
         end if
        else
        if (ABS((modul(1)-1.0d0)) .lt. 1.0d-6) then
         call CONV(dum,3,cycns,ncycns,cycns,ncycns)
        else
         call CONV(dum,3,cycs,ncycs,cycs,ncycs)
        end if
	      endif
       else
C
C
C
        k = DBLE(360/mq)
        i = 1
C
C HERE WE INTRODUCE THE NEW USE OF EPSPHI
C
        if ((mq.eq.12) .or. (mq.eq.6) .or. (mq.eq.4) .or. (mq.eq.2))
     $     then
c         if (ABS(ar(1)) .le. epsphi) then
         if ((modul(1).gt.rmod) .and. 
     $        (ABS(ar(1)).lt.360d0/dble(ny*MQ)))then
          if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
           call CONV(dum,3,chins,nchins,chins,nchins)
          else
           call CONV(dum,3,chis,nchis,chis,nchis)
          end if
         else
*          if (mq .eq. 12) then
*           neps = 3
*          end if
*          if (mq .eq. 6) then
*           neps = 4
*          end if
*          if (mq .eq. 4) then
*           neps = 6
*          end if
*          if (mq .eq. 3) then
*           neps = 8
*          end if
*          if (mq .eq. 2) then
*           neps = 10
*          end if
          if ((mq.ne.12) .and. (mq.ne.6) .and. (mq.ne.4) .and. 
     $        (mq.ne.3).and. (mq.ne.2) .and. (mq.ne.1)) then
           neps = 3
          end if
          neps = 1
          intocycle = 1
          do i = 1,mq/2
           if (mq.ne.12 .or. i.ne.4 .or. epsphi.le.2.5d0) then
            if ((ABS(ar(1)).gt.((k*i)-(neps*epsphi))) .and.
     $         (ABS(ar(1)).lt.((k*i)+(neps*epsphi))).and.
     $          modul(1).ge.RmodS) then
               intocycle = 0
            end if
           else
            if ((ABS(ar(1)).gt.((k*i)-(neps*2.5d0))) .and.
     $         (ABS(ar(1)).lt.((k*i)+(neps*2.5d0))).and.
     $          modul(1).ge.RmodS) then
              intocycle = 0
            end if
           end if
          end do
          if (intocycle .eq. 0) then
           if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
            call CONV(dum,3,psins,npsins,psins,npsins)
           else
            call CONV(dum,3,psis,npsis,psis,npsis)
           end if
          else if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
           IsCloseToTD=IsCloseTD(ar(1),MQ)
           call CONV(dum,3,cycns,ncycns,cycns,ncycns)
          else
           IsCloseToTD=IsCloseTD(ar(1),MQ)
           call CONV(dum,3,cycs,ncycs,cycs,ncycs)
          end if
         end if
C
C HERE ENDS THE NEW USE OF EPSPHI
C
        else if ((ABS(ar(1)).le.k-epsphi).or.
     $     (ABS(ar(1)).ge.k+epsphi).or.
     $     (modul(1).lt.RmodS))  then
         if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
          call CONV(dum,3,cycns,ncycns,cycns,ncycns)
         else
          call CONV(dum,3,cycs,ncycs,cycs,ncycs)
         end if
        else if (ABS(modul(1)-1.0d0) .lt. 1.0d-6) then
         call CONV(dum,3,psins,npsins,psins,npsins)
        else
         call CONV(dum,3,psis,npsis,psis,npsis)
        end if
       end if
C
C
C
       if (p .le. 2) return
       dum(1) = 1.0d0
       dum(2) = -rez(3)
       if (rez(3) .gt. 0.0d0) then
        if (ABS(1.0d0-rez(3)) .lt. 1.0d-06) then
         call CONV(dum,2,chins,nchins,chins,nchins)
         return
        end if
        if (ABS(rez(3)) .ge. RmodS) then
         call CONV(dum,2,chis,nchis,chis,nchis)
        else
         root0c=.TRUE.
         call CONV(dum,2,cycs,ncycs,cycs,ncycs)
        end if
       else if (ABS(1.0d0+rez(3)) .lt. 1.0d-06) then
        if (mq .eq. 1) then
         root0c=.TRUE.
         call CONV(dum,2,cycns,ncycns,cycns,ncycns)
        else
         root0c=.TRUE.
         call CONV(dum,2,psins,npsins,psins,npsins)
        end if
       else
        if (mq .eq. 1) then
         root0c=.TRUE.
         call CONV(dum,2,cycs,ncycs,cycs,ncycs)
         return
        end if
        if (ABS(rez(3)) .ge. rmod) then
         root0c=.TRUE.
         call CONV(dum,2,psis,npsis,psis,npsis)
        else if (ABS(rez(3)) .lt. rmod) then
         root0c=.TRUE.
         call CONV(dum,2,cycs,ncycs,cycs,ncycs)
        end if
       end if
      end if
      return
      end
c
c
      logical function isCloseTD(w,MQ)
      implicit none
c     INPUT PARAMETERS
      real*8 w
      integer MQ
c     LOCAL PARAMETERS
      real*8 dist,wtd1,wtd2
      real*8 dist1,dist2,pi
c-------------------------------------
      pi=3.1416
      IsCloseTD=.FALSE.
      Dist=-1.0d0
c-------------------------------------
      if (MQ.eq.12) then
       Wtd1=0.6964D0*180.0d0
       Dist1=0.5d0*(Wtd1-2.0d0*180.0d0/3.0d0)
       Dist2=(0.12d0/pi)*180.0
c       wtd2=0.8640D0*180.0D0
C        if ((abs(w).gt. (Wtd1-dist1)).and.(abs(w).lt.(Wtd1+dist2))) then
       if ((abs(w).lt. (0.732d0*180.0d0)).and.
     $     (abs(w).gt.(0.680556d0*180.0d0))) then
        IsCloseTD=.TRUE.
       end if 
c-------------------------------------
      else if (MQ.eq.4) then
       Dist=0.02D0*180.0D0
       WTD1=0.0892D0*180.0D0
c       Wtd2=0.1785D0*180.0D0
       if (abs(abs(w)-wtd1).lt.Dist) then
        isCloseTD=.TRUE.
       end if
      end if
c-------------------------------------
      return
      end
C
C   THIS SUBROUTINE COMPUTES TOTAL ESTIMATION ERROR, REVISION ERRORS
C   STANDARD ERROR OF RECENT ESTIMATES AND FORECASTS, AND OF THE RATES OF
C   GROWTH
C
C        INPUT PARAMETERS
C
C          MQ : FREQUENCY
C       PSIEP : PSI-WEIGTHS (B,F) OF TREND
C       PSIEA : PSI-WEIGTHS (B,F) OF SEASONALLY ADJUSTED
C       PSIEC : PSI-WEIGTHS (B,F) OF CYCLE
C      FEETRE : ACF OF FINAL ESTIMATION ERROR OF TREND
C      FEEADJ : ACF OF FINAL ESTIMATION ERROR OF SEASONALLY ADJUSTED
C      FEECYC : ACF OF FINAL ESTIMATION ERROR OF CYCLE
C       PSIES : PSI-WEIGTHS (B,F) OF SEASONAL
C      PSITOT : PSI-WEIGTHS (B,F) TOTAL
C           Z : ORIGINAL SERIES AND FORECAST
C       TREND : TREND COMPONENT
C          SA : SEASONALLY ADJUSTED SERIES
C       CYCLE : CYCLICAL COMPONENT
C          SC : SEASONAL COMPONENT
C       NFILT : DIMENSION OF PSIEP,PSIES,PSIEC,PSIEA,PSITOT
C         SQF : STANDARD ERROR OF RESIDUALS
C          NZ : DIMENSION OF THE SERIES
C         MQ2 : 2*FREQUENCY
C        LAMD : 0 TRANSFORMED DATA, 1 NOT TRANSFORMED
C       TITLE : NAME OF THE SERIES
C        NCYC : DIMENSION OF CYCLE DENOMINATOR
C        NPSI : DIMENSION OF SEASONAL DENOMINATOR
C        LFOR : DIMENSION OF FORECAST
C     NOSERIE : 1 NOSERIE INPUTED, 0 OTHERWISE
C          IR : IRREGULAR COMPONENT
C          OZ : ORIGINAL SERIES
C          PG : 0 FILES FOR GRAPH, 1 NO FILES
C         OUT : CONTROL OF PRINTOUT
C        ITER : ITERATION MODE (IF <> 0 NOPRINT ON THE SCREEN)
C    NOADMISS : 2 APPROXIMATED MODEL, NO APPROXIMATION OTHERWISE
C
C
      subroutine SECOND(sigpt1,sigat1,nlen,sigptac,sigatac,
     $                  sigptaf,sigataf,sigptmq,sigatmq,sigxtmq,rcetre,
     $                  rceadj,teetre,teeadj,nelen,mq,psiep,psiea,psiec,
     $                  feetre,feeadj,feecyc,psies,psitot,z,trend,sa,
     $                  cycle,sc,nfilt,sqf,nz,mq2,lamd,title,ncyc,npsi,
     $                  lfor,noserie,ir,oz,pg,out,iter,bias,
C     $                  forbias,forsbias,fortbias,tramo,maxbias,smtr,
     $                  forbias,forsbias,fortbias,tramo,
     $                  ncycth,ioneout,nthclass,ntcclass,ntfclass,
     $                  overmaxbias,Nchcyc,alpha,rceCyc,IsCloseToTD,
     $                  varwnc)
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      integer nfl
      parameter (nfl = mp*2)
      real*8 maxbias
      parameter (maxbias=0.5d0)
C
      INCLUDE 'seatop.cmn'
C
C.. Formal Arguments ..
      integer nlen,nelen,mq,nfilt,nz,mq2,lamd,ncyc,npsi,lfor,noserie,
C     $        nannua,pg,out,iter,bias,tramo,smtr,ncycth,ioneout,
     $        pg,out,iter,bias,tramo,ncycth,ioneout,
     $        nthclass,ntcclass,ntfclass,overmaxbias,Nchcyc
      character title*80
      real*8 sigpt1(0:24),sigat1(0:24),sigptac(24),sigatac(24),
     $       sigptaf(24),sigataf(24),sigptmq(2),sigatmq(2),sigxtmq(2),
     $       rcetre(0:12),rceadj(0:12),teetre(0:12),teeadj(0:12),
     $       psiep(nfl),psiea(nfl),psiec(nfl),feetre(0:12),feeadj(0:12),
     $       feecyc(0:12),psies(nfl),psitot(nfl),z(*),trend(mpkp),
     $       sa(mpkp),cycle(mpkp),sc(mpkp),sqf,ir(mpkp),
     $       oz(mpkp),forbias(kp),forsbias(kp),fortbias(kp),
     &       alpha,dvec(1),rcecyc(0:12),varwnc
      logical IsCloseToTD

C
C.. Local Scalars ..
      integer i,ii,j,k,lon,ndiv,nstart,ntitle,nyr,vabs
      character fname*30,subtitle*50
      logical bool,noseas,noS,noC
      real*8 aa,ac,amean,ap,at,aux1,aux2,ba,
     $       bias1c,bias2c,bias3c,bpp,bser,c1,c2,c3,ca,cp,pamean,pmean,
     $       por,ppmean,psmean,pzmean,rfactse,sefea,sefec,sefep,smean,
     $       v3fd,v3fp,vca
      real*8 vcp,vdd,vfa,vfp,vmi,vmig,vmigu,vpc,vpp,vraa,vrpp,vx,xx,
     $       ymax,ymin,zmean
C
C.. Local Arrays ..
      real*8 adum(12),atdum(12),bdum(12),btdum(12),
     $     splot(2*kp+1,3),st1d(0:13),st1p(0:13),st3d(0:13),st3p(0:13),
     $     stdf(12),stdt(12),stpf(12),stpt(12),teecyc(0:12),tmp(kp),
     $     tmp1(kp),tmp2(kp),vreadj(0:5),vrecyc(0:5),vresea(0:12),
     $     vretre(0:5)
C
C.. External Functions ..
      real*8 DMEAN
      integer ISTRLEN
      real*8 RAIZ
      external DMEAN, ISTRLEN, RAIZ
C
C.. External Calls ..
      external ABIASC, BIASCORR, Seasign, SMRFACT, USRENTRY
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. Intrinsic Functions ..
      intrinsic EXP, LOG, SQRT
      include 'sfcast.i'
      include 'serrlev.i'
      include 'sesfcast.i'
      include 'stream.i'
      include 'revs.i'
*      include 'indhtml.i'
      include 'transcad.i'
C
C ... Executable Statements ...
C
*      write(*,*)'  lfor = ',lfor
      lon = mq
*      do k = 0,lon
*       rcetre(k) = 0.0d0
*       rceadj(k) = 0.0d0
*       rcecyc(k) = 0.0d0
*       do i = k+1,nfilt
*        rcetre(k) = rcetre(k) + psiep(i)*psiep(i-k)
*        rceadj(k) = rceadj(k) + psiea(i)*psiea(i-k)
*        rcecyc(k) = rcecyc(k) + psiec(i)*psiec(i-k)
*       end do
*      end do
*      do k = 1,lon
*       if (rcetre(0) .gt. 1.0d-13) then
*        rcetre(k) = rcetre(k) / rcetre(0)
*       end if
*       if (rceadj(0) .gt. 1.0d-13) then
*        rceadj(k) = rceadj(k) / rceadj(0)
*       end if
*       if (rcecyc(0) .gt. 1.0d-13) then
*        rcecyc(k) = rcecyc(k) / rcecyc(0)
*       end if
*      end do
C
C  COMPUTE TOTAL ESTIMATION ERROR  (ACF)
C
      teeadj(0) = feeadj(0) + rceadj(0)
      teecyc(0) = feecyc(0) + rcecyc(0)
      teetre(0) = feetre(0) + rcetre(0)
C
      do i = 1,lon
       teetre(i) = feetre(i)*feetre(0) + rcetre(i)*rcetre(0)
       teeadj(i) = feeadj(i)*feeadj(0) + rceadj(i)*rceadj(0)
       teecyc(i) = feecyc(i)*feecyc(0) + rcecyc(i)*rcecyc(0)
      end do
C
      do k = 1,lon
       if (teetre(0) .gt. 1.0d-13) then
        teetre(k) = teetre(k) / teetre(0)
       end if
       if (teeadj(0) .gt. 1.0d-13) then
        teeadj(k) = teeadj(k) / teeadj(0)
       end if
       if (teecyc(0) .gt. 1.0d-13) then
        teecyc(k) = teecyc(k) / teecyc(0)
       end if
      end do
      call USRENTRY(feetre,0,lon,0,12,1102)
      call USRENTRY(rcetre,0,lon,0,12,1103)
      call USRENTRY(teetre,0,lon,0,12,1104)
      call USRENTRY(feeadj,0,lon,0,12,1105)
      call USRENTRY(rceadj,0,lon,0,12,1106)
      call USRENTRY(teeadj,0,lon,0,12,1107)
      nelen = lon
      if (Out .eq. 0) then
C
C
C  OUTPUT RESULTS OF -ACF- OF ESTIMATION ERRORS
C
C
 7000   format (
     $ ///,' PART 3 : ERROR ANALYSIS',/,' -----------------------',//)
        write (Nio,7000)
      end if
C
C
      if (Lfinit) then
C  Modified by REG on 01/05/2006
        write (Nio,7001) infMSEs(3), infMSEs(2), ' (SEMI-INFINITE)',
     $                   infRevs(3), infRevs(2),
     $                   curMSEs(3), curMSEs(2)
 7001   format (
     $  //,30x,'ESTIMATION ERROR VARIANCE'/,30x,'  (In units of Var(a))'
     $  ,//34x,'TREND-CYCLE',2x,'SA SERIES',//2x,'FINAL ESTIMATION'
     $  ,15x,f6.3,10x,f6.3,/,2x,'ERROR',a,//2x,'REVISION IN CON-'
     $  ,15x,f6.3,10x,f6.3,/,2x,'CURRENT ERROR',//2x,'TOTAL ESTIMATION'
     $  ,15x,f6.3,10x,f6.3,/,2x,'ERROR (CONCURRENT',/,2x,'ESTIMATOR)')
      else if (out.eq.0)THEN
 7003   format (
     $  //,' ',25x,'FINAL ESTIMATION ERROR',21x,'REVISION IN',
     $  ' CONCURRENT ESTIMATOR'//' ACF (LAG)',12x,' TREND-CYCLE  ',5x,
     $  'SA SERIES',14x,' TREND-CYCLE  ',4x,'SA SERIES'/)
        write (Nio,7003)
        do i = 1,lon
 7004   format (1x,i7,2x,12x,f8.3,10x,f8.3,17x,f8.3,8x,f8.3)
         write (Nio,7004) i, feetre(i), feeadj(i), rcetre(i), rceadj(i)
        end do
 7005   format (/,' ','VAR.(*)',2x,12x,f8.3,10x,f8.3,17x,f8.3,8x,f8.3)
        write (Nio,7005) feetre(0), feeadj(0), rcetre(0), rceadj(0)
 7006   format (
     $  ////6x,'TOTAL ESTIMATION ERROR (CONCURRENT ESTIMATOR)',/6x,
     $  '---------------------------------------------',//8x,'ACF (LAG)'
     $  ,5x,' TREND-CYCLE  ',4x,'SA SERIES'/)
        write (Nio,7006)
        do i = 1,lon
 7007   format (2x,i10,11x,f8.3,8x,f8.3)
         write (Nio,7007) i, teetre(i), teeadj(i)
        end do
 7008   format (/,' ',6x,'VAR.(*)',9x,f8.3,8x,f8.3)
        write (Nio,7008) teetre(0), teeadj(0)
        write (Nio,7009)
      end if
 7009 format (//,' ',' (*) IN UNITS OF VAR(A)')
C
C
C  COMPUTE THE VARIANCE OF REVISION FOR DIFFERENT PERIODS
C
      vretre(0) = rcetre(0)
      vrecyc(0) = rcecyc(0)
      vreadj(0) = rceadj(0)
      do i = 1,5
       vretre(i) = vretre(i-1)
       vrecyc(i) = vrecyc(i-1)
       vreadj(i) = vreadj(i-1)
       nstart = nfilt + 1 - mq*(i-1)
       do j = 1,mq
        vretre(i) = vretre(i) - psiep(nstart-j)*psiep(nstart-j)
        vrecyc(i) = vrecyc(i) - psiec(nstart-j)*psiec(nstart-j)
        vreadj(i) = vreadj(i) - psiea(nstart-j)*psiea(nstart-j)
       end do
      end do
      do i=1,5
        if ( vreadj(i) .lt. 1.0D-16) then
           vreadj(i)=0.0d0
        end if
        if (vrecyc(i) .lt. 1.0D-16 ) then
           vrecyc(i)=0.0D0
        end if
        if (vretre(i) .lt. 1.0D-16) then
           vretre(i)=0.0D0
        end if
      end do
C
      if (out .eq. 0) then
 7016   format (
     $   ///,' ','VARIANCE OF THE REVISION ERROR (*)',/,' ',
     $   '------------------------------',//' ADDITIONAL',12x,
     $   '  TREND-CYCLE ',4x,' SA SERIES'/'  PERIODS'//)
C
        write (Nio,7016)
        do i = 0,5
         j = i * mq
 7017    format (2x,i5,10x,2(7x,g11.4))
         write (Nio,7017) j, vretre(i), vreadj(i)
        end do
      end if    
      call USRENTRY(vretre,0,5,0,5,1515)
      call USRENTRY(vreadj,0,5,0,5,1516)
C
C  COMPUTE THE REDUCTION IN THE STD. ERR. OF REVISION AFTER
C  ADDITIONAL YEARS (DISPLAY AND DON'T STORE)
C
      if (out .eq. 0) then
 7020   format (
     $ ///,' ',' PERCENTAGE REDUCTION IN THE STANDARD ERROR',
     $ ' OF THE REVISION AFTER ADDITIONAL YEARS'/
     $ ' (COMPARISON WITH CONCURRENT ESTIMATORS)'///)
        write (Nio,7020)
      end if
C  Modified by REG on 04/27/2006 and 01/05/2006
      IF(Lfinit)then
C----------------------------------------------------------------------
C     Output alternate revisions after additional years.
C----------------------------------------------------------------------
       do i = 1,5
        if (out .eq. 0) then
          write (Nio,7021) i, relRevs(3,i), relRevs(2,i)
        end if
        tmp(i)=relRevs(3,i)
        tmp1(i)=relRevs(2,i)
       end do
      else
       do i = 1,5
        at = vretre(0)
        ac = vrecyc(0)
        aa = vreadj(0)
        if (at .gt. 1.0d-13) then
         at = (1.0-RAIZ(vretre(i)/vretre(0))) * 100.0
        end if
        if (ac .gt. 1.0d-13) then
         ac = (1.0-RAIZ(vrecyc(i)/vrecyc(0))) * 100.0
        end if
        if (aa .gt. 1.0d-13) then
         aa = (1.0-RAIZ(vreadj(i)/vreadj(0))) * 100.0
        end if
        tmp(i) = at
        tmp1(i) = aa
C----------------------------------------------------------------------
C     Output SEATS revisions after additional years.
C----------------------------------------------------------------------
        if (out .eq. 0) then
 7021    format (3x,'AFTER',i2,' YEAR',7x,g11.4,7x,g11.4)
         write (Nio,7021) i, at, aa
        end if
       end do
C
C  Modified by REG on 01/05/2006
      end if
      call setCovt1(tmp(1))
      call setCovsa1(tmp1(1))
      call setCovt5(tmp(5))
      call setCovsa5(tmp1(5))
      call USRENTRY(tmp,1,5,1,kp,1517)
      call USRENTRY(tmp1,1,5,1,kp,1518)
C
C  COMPUTE THE VARIANCE OF REVISION ERROR FOR THE SEASONAL
C
      do j = 0,mq
       vresea(j) = 0.0d0
      end do
      if (npsi .ne. 1) then
       do i = 1,nfilt
        vresea(0) = vresea(0) + psies(i)*psies(i)
       end do
       do i = 1,mq
        vresea(i) = vresea(i-1) + psies(nfilt+i)*psies(nfilt+i)
       end do
       if (out .eq. 0) then
 7023    format (
     $  ///,' ','VARIANCE OF THE REVISION ERROR FOR THE ',
     $  'SEASONAL COMPONENT (ONE YEAR AHEAD ADJUSTMENT)',/,' ',
     $  '---------------------------------------',
     $  '----------------------------------------------',//
     $  ' PERIODS AHEAD',10x,'VARIANCE (*)'/)
         write (Nio,7023)
         do i = 0,mq
 7024     format (2x,i8,15x,g11.4)
          write (Nio,7024) i, vresea(i)
         end do
       end if
       call USRENTRY(vresea,0,mq,0,12,1519)
C
       vpc = 0.0d0
       do i = 0,mq-1
        vpc = vpc + vresea(i)
       end do
       vpc = vpc / mq
       vpc = (1.0-SQRT(vresea(0)/vpc)) * 100.0
       if (out .eq. 0) then
 7025    format (
     $  ///,' ','AVERAGE PERCENTAGE REDUCTION IN RMSE FROM',
     $  '  CONCURRENT ADJUSTMENT',g11.4)
         write (Nio,7025) vpc
       end if
       dvec(1)=vpc
       call USRENTRY(dvec,1,1,1,1,1520)
      end if
      if (out .eq. 0) then
        write (Nio,7009)
      end if
C
C
C   COMPUTE THE STANDARD ERROR OF RECENT ESTIMATE AND FORECAST
C
C    TOTAL (SET@) AND DUE TO REVISION (SER@)
C
C
C
      mq2 = lfor
*      mq2 = 2*mq
      Serc(-mq2) = 0.0d0
      Serp(-mq2) = 0.0d0
      Sera(-mq2) = 0.0d0
      Sers(-mq2) = 0.0d0
      do i = 1,nfilt-mq2
       Serp(-mq2) = Serp(-mq2) + psiep(i)*psiep(i)
       Sera(-mq2) = Sera(-mq2) + psiea(i)*psiea(i)
       Sers(-mq2) = Sers(-mq2) + psies(i)*psies(i)
       Serc(-mq2) = Serc(-mq2) + psiec(i)*psiec(i)
      end do
      do k = -mq2+1,mq2
       Serp(k) = Serp(k-1) + psiep(nfilt+k)*psiep(nfilt+k)
       Sera(k) = Sera(k-1) + psiea(nfilt+k)*psiea(nfilt+k)
       Sers(k) = Sers(k-1) + psies(nfilt+k)*psies(nfilt+k)
       Serc(k) = Serc(k-1) + psiec(nfilt+k)*psiec(nfilt+k)
      end do
C
      do k = -mq2,mq2
       Setp(k) = SQRT(feetre(0)+Serp(k)) * sqf
       Seta(k) = SQRT(feeadj(0)+Sera(k)) * sqf
       Sets(k) = SQRT(feeadj(0)+Sers(k)) * sqf
       Setc(k) = SQRT(feecyc(0)+Serc(k)) * sqf
      end do
      do k = -mq2,mq2
       Serp(k) = SQRT(Serp(k)) * sqf
       Sera(k) = SQRT(Sera(k)) * sqf
       Sers(k) = SQRT(Sers(k)) * sqf
       Serc(k) = SQRT(Serc(k)) * sqf
      end do
      Seser(1) = 1.0d0
      do i = 1,mq2-1
       Seser(i+1) = Seser(i) + psitot(nfilt+1+i)*psitot(nfilt+1+i)
      end do
      do i = 1,mq2
       Seser(i) = SQRT(Seser(i)) * sqf
      end do
C
      if (noserie .eq. 1) return
C
      call USRENTRY(trend,nz-mq2,nz,1,mpkp,1121)
      call USRENTRY(Setp,-mq2,0,-kp,kp,1122)
      call USRENTRY(Serp,-mq2,0,-kp,kp,1123)
      call USRENTRY(sa,nz-mq2,nz,1,mpkp,1124)
      call USRENTRY(Seta,-mq2,0,-kp,kp,1125)
      call USRENTRY(Sera,-mq2,0,-kp,kp,1126)
      call USRENTRY(sc,nz-mq2,nz,1,mpkp,1127)
      call USRENTRY(Sets,-mq2,0,-kp,kp,1128)
      call USRENTRY(Sers,-mq2,0,-kp,kp,1129)
      if (varwnc.gt.1.0D-10 .and.(ncycth.gt.0 .and. ncyc.gt.1)) then
       call USRENTRY(cycle,nz-mq2,nz,1,mpkp,1144)
       call USRENTRY(Setc,-mq2,0,-kp,kp,1145)
       call USRENTRY(Serc,-mq2,0,-kp,kp,1146)
      end if
      sefep = SQRT(feetre(0)) * sqf
      sefea = SQRT(feeadj(0)) * sqf
      sefec = SQRT(feecyc(0)) * sqf
      Sefes = sefea
      dvec(1)=sefep
      call USRENTRY(dvec,1,1,1,1,1130)
      dvec(1)=sefea
      call USRENTRY(dvec,1,1,1,1,1131)
      dvec(1)=sefes
      call USRENTRY(dvec,1,1,1,1,1132)
      if (varwnc.gt.1.0D-10 .and.(ncycth.gt.0 .and. ncyc.gt.1)) then
       dvec(1)=sefec
       call USRENTRY(dvec,1,1,1,1,1147)
      end if
      if (out .eq. 0) then
       if (lamd .eq. 0) then  
 7026    format (
     $  //,' ',12x,'LOGS DECOMPOSITION OF THE SERIES: RECENT',
     $  ' ESTIMATES'/' ',12x,'---------------------------------------',
     $  '-----------'//1x,'PERIOD',5x,'SERIES  ',22x,' TREND-CYCLE',37x,
     $  'SA SERIES',//30x,'ESTIMATE',10x,'STANDARD ERROR',20x,'ESTIMATE'
     $  ,10x,'STANDARD ERROR'/46x,'TOTAL',6x,'   OF  REVISION',27x,
     $  'TOTAL',6x,'   OF  REVISION'/)
          write (Nio,7026)
       else
 7027    format (
     $  //,' ',12x,'ADDITIVE DECOMPOSITION OF THE SERIES:',
     $  ' RECENT ESTIMATES'/' ',12x,
     $  '---------------------------------------','---------------'//1x,
     $  'PERIOD',5x,'SERIES  ',22x,' TREND-CYCLE',37x,'SA SERIES',//30x,
     $  'ESTIMATE',10x,'STANDARD ERROR',20x,'ESTIMATE',10x,
     $  'STANDARD ERROR'/46x,'TOTAL',6x,'   OF  REVISION',27x,'TOTAL'
     $  ,6x,'   OF  REVISION'/)
          write (Nio,7027)
       end if
       do i = -MAX(8,2*mq),0
         j = nz + i
 7028    format (
     $  2x,i4,2x,g11.4,7x,g11.4,4x,g11.4,4x,g11.4,12x,g11.4,
     $  4x,g11.4,4x,g11.4)
         write (Nio,7028)
     $        i, z(j), trend(j), Setp(i), Serp(i), sa(j), Seta(i),
     $        Sera(i)
       end do
      end if
      if (out .eq. 0) then
 7029   format (
     $ //,' STANDARD ERROR OF',23x,g11.4,42x,g11.4,/' FINAL ESTIMATOR')
        write (Nio,7029) sefep, sefea
      end if
c   rober: 4/3 continuar por aqui
      if (out.eq.0 .or. lamd.ne.1) then
c        Nota:por que esta esta condicion aqui???!          
       if ((out.ne.0) .and. (ITER .eq. 0) .and. (lamd.eq.0)) then
        goto 5000
       else if (varwnc.lt.1.0D-10 .or.
     $         (ncycth.eq.0 .and. ncyc.eq.1)) then
        if (out.eq.0) then
 7030    format (
     $  /,/1x,'PERIOD',20x,'SEASONAL',//18x,'ESTIMATE',10x,
     $  'STANDARD ERROR'/33x,'TOTAL',6x,'   OF  REVISION'/)
         write (Nio,7030)
         do i = -MAX(8,2*mq),0
          j = nz + i
 7031     format (
     $   2x,i4,7x,g11.4,4x,g11.4,4x,g11.4,12x,g11.4,4x,g11.4,4x,g11.4)
          write (Nio,7031) i, sc(j), Sets(i), Sers(i)
         end do
 7032    format(//,' STANDARD ERROR OF',15x,g11.4,/,' FINAL ESTIMATOR')
         write (Nio,7032) Sefes
        end if
       else
        if (out.eq.0) then
 7033    format (
     $  /,/1x,'PERIOD',20x,'SEASONAL',45x,'  TRANS.  ',//18x,'ESTIMATE'
     $  ,10x,'STANDARD ERROR',20x,'ESTIMATE',10x,'STANDARD ERROR'/33x,
     $  'TOTAL',6x,'   OF  REVISION',27x,'TOTAL',6x,'   OF  REVISION'/)
         write (Nio,7033)
         do i = -MAX(8,2*mq),0
          j = nz + i
          write (Nio,7031)
     $         i, sc(j), Sets(i), Sers(i), cycle(j), Setc(i), Serc(i)
         end do
 7034    format (
     $   //,' STANDARD ERROR OF',10x,g11.4,42x,g11.4,/,
     $      ' FINAL ESTIMATOR')
         write (Nio,7034) Sefes, sefec
        end if
       end if
      end if
C
C
      if (out .eq. 0) then
        if (lamd .eq. 0) then
 7035    format (
     $  //,' ',12x,'FORECAST OF THE STOCHASTIC SERIES ',
     $  'AND COMPONENTS (LOGS)'/,' ',12x,
     $  '----------------------------------','---------------------'/
     $  /1x,'PERIOD',5x,'SERIES  ',32x,' TREND-CYCLE',37x,'SA SERIES',//
     $  10x,'FORECAST',8x,'S.E.',10x,'FORECAST',10x,'STANDARD ERROR'
     $  ,20x,'FORECAST',10x,'STANDARD ERROR'/56x,'TOTAL',6x,
     $  '   OF  REVISION',27x,'TOTAL',6x,'   OF  REVISION'/)
         write (Nio,7035)
        else
 7036    format (
     $  //,' ',12x,'FORECAST OF THE STOCHASTIC SERIES ','AND COMPONENTS'
     $  /,' ',12x,'----------------------------------','--------------'/
     $  /1x,'PERIOD',5x,'SERIES  ',32x,' TREND-CYCLE',37x,'SA SERIES',//
     $  10x,'FORECAST',8x,'S.E.',10x,'FORECAST',10x,'STANDARD ERROR'
     $  ,20x,'FORECAST',10x,'STANDARD ERROR'/56x,'TOTAL',6x,
     $  '   OF  REVISION',27x,'TOTAL',6x,'   OF  REVISION'/)
         write (Nio,7036)
        end if
      end if
c  Ya se escribe antes
c      if (out .eq. 0) then
c       if (HTML .eq. 1) then
c        write (Nio,'("<p><strong>THE SE ARE THOSE OF THE TOTAL",
c     $              " ESTIMATION ERROR = REVISION ERROR AND ",
c     $              "FINAL ESTIMATION ERROR.</strong></p>")')
c       else
c        write (Nio,
c     $'(/,4x,''THE SE ARE THOSE OF THE TOTAL ESTIMATION ERROR ='',
c     $/,4x,''REVISION ERROR AND FINAL ESTIMATION ERROR.'',/)')
c       end if
c      end if
      if (npsi .eq. 1) then
       do i = 1,mq2
        if (IscloseToTD) then
         sa(nz+i) = z(nz+i) - cycle(nz+i)
        else
         sa(nz+i) = z(nz+i)
        end if
       end do
C   LINES OF CODE COMMENTED FOR X-13A-S : 1      
C      else if ((smtr.eq.0) .and. (ncyc.eq.1)) then
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      else if ((ncyc.eq.1)) then
C   END OF CODE BLOCK
       do i = 1,mq2
        sa(nz+i) = trend(nz+i)
       end do
      end if
      call USRENTRY(z,nz+1,nz+mq2,1,mpkp,1133)
      call USRENTRY(Seser,1,mq2,1,kp,1134)
      call USRENTRY(sa,nz+1,nz+mq2,1,mpkp,1138)
      call USRENTRY(Seta,1,mq2,-kp,kp,1139)
      call USRENTRY(Sera,1,mq2,-kp,kp,1140)
      call USRENTRY(sc,nz+1,nz+mq2,1,mpkp,1141)
      call USRENTRY(Sets,1,mq2,-kp,kp,1142)
      call USRENTRY(Sers,1,mq2,-kp,kp,1143)
      if (varwnc.gt.1.0D-10 .and.(ncycth.gt.0 .and. ncyc.gt.1)) then
       call USRENTRY(cycle,nz+1,nz+mq2,1,mpkp,1170)
       call USRENTRY(Setc,1,mq2,-kp,kp,1171)
       call USRENTRY(Serc,1,mq2,-kp,kp,1172)
      end if
      if (Nsfcast .eq. 0) then
       call USRENTRY(trend,nz+1,nz+mq2,1,mpkp,1135)
       call USRENTRY(Setp,1,mq2,-kp,kp,1136)
       call USRENTRY(Serp,1,mq2,-kp,kp,1137)
       if (out .eq. 0) then
 7037    format (
     $   2x,i4,2x,g11.4,2x,g11.4,4x,g11.4,4x,g11.4,4x,g11.4,
     $   12x,g11.4,4x,g11.4,4x,g11.4)
         do i = 1,mq2
          j = nz + i
          write (Nio,7037)
     $         i, z(j), Seser(i), trend(j), Setp(i), Serp(i), sa(j),
     $         Seta(i), Sera(i)
         end do
       end if
      else
       rfactse = Sqfsave / sqf
       do i = 1,mq2
        Rfact(i) = Sfcast(i) / z(nz+i)
       end do
       call SMRFACT(Rfact,mq2,mq)
       if (out .eq. 0) then
         do i = 1,mq2
          j = nz + i
          write (Nio,7037)
     $         i, Sfcast(i), Sesfcast(i), trend(j)*Rfact(i),
     $         Setp(i)*rfactse, Serp(i)*rfactse, sa(j), Seta(i), Sera(i)
         end do
       end if
       do i = 1,mq2
        tmp(i) = trend(nz+i) * Rfact(i)
        tmp1(i) = Setp(i) * rfactse
        tmp2(i) = Serp(i) * rfactse
       end do
       call USRENTRY(tmp,1,mq2,1,kp,1135)
       call USRENTRY(tmp1,1,mq2,1,kp,1136)
       call USRENTRY(tmp2,1,mq2,1,kp,1137)
      end if
      if (varwnc.lt.1.0D-10 .or.(ncycth.eq.0 .and. ncyc.eq.1)) then
       if (out .eq. 0) then
 7038    format (
     $  //,' ',/1x,'PERIOD',20x,' SEASONAL',//18x,'FORECAST',10x,
     $  'STANDARD ERROR',/33x,'TOTAL',6x,'   OF  REVISION'/)
         write (Nio,7038)
       end if
       if (out .eq. 0) then
c rober: Nfscast no hace nada el codigo es el mismo para las 2 posibilidades del if
c     REVISAR!!        
        if (Nsfcast .eq. 0) then
         do i = 1,mq2
           j = nz + i
 7039      format (2x,i4,7x,g11.4,4x,g11.4,4x,g11.4)
           write (Nio,7039) i, sc(j), Sets(i), Sers(i)
         end do
        else
         do i = 1,mq2
          j = nz + i
          write (Nio,7039) i, sc(j), Sets(i), Sers(i)
         end do
        end if
       end if
       if ((out.eq.0) .and. (Nsfcast.eq.1)) then
         write (Nio,'(/,30x,''DUE TO THE APPROXIMATION, THE S.E.'',/,
     $                  30x,''OF THE COMPONENT MAY BE UNRELIABLE'',/)')
       end if
      else
       if (out .eq. 0) then
 7040    format (
     $  //,' ',/1x,'PERIOD',20x,' SEASONAL',45x,'TRANS.',//18x,
     $  'FORECAST',10x,'STANDARD ERROR',20x,'FORECAST',10x,
     $  'STANDARD ERROR'/33x,'TOTAL',6x,'   OF  REVISION',27x,'TOTAL'
     $  ,6x,'   OF  REVISION'/)
         write (Nio,7040)
       end if
       if (out .eq. 0) then
        if (Nsfcast .eq. 0) then
         do i = 1,mq2
          j = nz + i
 7041     format (
     $    2x,i4,7x,g11.4,4x,g11.4,4x,g11.4,12x,g11.4,4x,g11.4,4x,g11.4)
          write (Nio,7041)
     $          i, sc(j), Sets(i), Sers(i), cycle(j), Setc(i), Serc(i)
         end do
        else
         do i = 1,mq2
          j = nz + i
          write (Nio,7041)
     $          i, sc(j), Sets(i), Sers(i), cycle(j), Setc(i), Serc(i)
         end do
        end if
       end if
       if ((out.eq.0) .and. (Nsfcast.eq.1)) then
        write (Nio,'(/,30x,''DUE TO THE APPROXIMATION, THE S.E.'',/,
     $    30x,''OF THE COMPONENT MAY BE UNRELIABLE'',/)')
       end if
      end if
C
C
C HERE INTRODUCE THE CHECK ON THE SEASONAL SIGNIFICANCE
C
C       if ((Nchcyc .gt. 1) .and. (ABS(Sefes) .gt. 1.0d-8)) then
 5000  call SEASIGN(Npsi,lamd,alpha,sc,Sets,Sefes,nz,mq2,
     $              mq,nthclass,ntcclass,ntfclass,out)
c     end if
C
C
      zmean = DMEAN(nz,z)
      pmean = DMEAN(nz,trend)
      amean = DMEAN(nz,sa)
      smean = DMEAN(nz,sc)
      pzmean = 0.0d0
      ppmean = 0.0d0
      pamean = 0.0d0
      psmean = 0.0d0
      if (mq .eq. 1) then
       do i = nz-11,nz
        pzmean = pzmean + z(i)/12.0
        ppmean = ppmean + trend(i)/12.0
        pamean = pamean + sa(i)/12.0
        psmean = psmean + sc(i)/12.0
       end do
      else
       do i = nz-mq*3+1,nz
        pzmean = pzmean + z(i)
        ppmean = ppmean + trend(i)
        pamean = pamean + sa(i)
        psmean = psmean + sc(i)
       end do
       ndiv = mq * 3
       pzmean = pzmean / ndiv
       ppmean = ppmean / ndiv
       pamean = pamean / ndiv
       psmean = psmean / ndiv
      end if
      if (out .eq. 0) then
 7044  format (//,' ','SAMPLE MEANS',/,' ','------------'/)
       write (Nio,7044)
       if (mq .eq. 1) then
 7045   format (
     $  15x,'COMPLETE PERIOD',4x,'LAST 12 OBSERVATIONS'/' SERIES  '
     $  ,6x,g11.4,6x,g11.4/' TREND-CYCLE',3x,g11.4,6x,g11.4/' SA SERIES'
     $  ,5x,g11.4,6x,g11.4/' SEASONAL',6x,g11.4,6x,g11.4)
        write (Nio,7045)
     $        zmean, pzmean, pmean, ppmean, amean, pamean, smean, psmean
       else
 7046   format (
     $  15x,'COMPLETE PERIOD',4x,'LAST THREE YEARS'/' SERIES  '
     $  ,6x,g11.4,6x,g11.4/' TREND-CYCLE',3x,g11.4,6x,g11.4/' SA SERIES'
     $  ,5x,g11.4,6x,g11.4/' SEASONAL',6x,g11.4,6x,g11.4)
        write (Nio,7046)
     $        zmean, pzmean, pmean, ppmean, amean, pamean, smean, psmean
       end if
      end if
C
      do i = 1,2*kp+1
       do j = 1,3
        splot(i,j) = 0.0d0
       end do
      end do
      do i = kp-mq2,kp+mq2
       do j = 1,3
        splot(i,j) = z(nz-kp+i)
        if ((nz-kp+i.gt.nz) .and. (Nsfcast.eq.1)) then
         splot(i,j) = Sfcast(i-kp)
        end if
       end do
      end do
      do i = 1,mq2
       splot(kp+i,1) = splot(kp+i,1) - Seser(i)*alpha
       splot(kp+i,2) = splot(kp+i,2) + Seser(i)*alpha
      end do
      ymin = splot(kp-mq2,1)
      ymax = splot(kp-mq2,1)
      do j = 1,3
       do i = kp-mq2,kp+mq2
        bser = splot(i,j)
        if (bser .le. ymin) then
         ymin = bser
        end if
        if (bser .ge. ymax) then
         ymax = bser
        end if
       end do
      end do
*      if ((pg .eq. 0).and.(iter.eq.0).and.(out.lt.2)) then
*       if (lamd.eq.0) then
*        fname = 'FORXL.T5'
*        write(subtitle,'("FORECAST: ",A," COMPONENT")')
*     $        transLcad(1:nTransLcad)
*        call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*        fname = 'FORX.T5'
*        subtitle = 'FORECAST OF STOCHASTIC SERIES'
*        do j = 1,3
*         do i = kp-mq2,kp+mq2
*          splot(i,j) = EXP(splot(i,j))
*         end do
*        end do
*        call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*       else
*        fname = 'FORX.T5'
*        subtitle = 'FORECAST OF STOCHASTIC SERIES'
*        call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*       end if
*      end if
C
C
      ntitle = ISTRLEN(title)
*      if ((iter.ne.0) .and. (ioneout.eq.0) .and. (tramo.le.0) .and. 
*     $    (out.eq.0).and.(pg.eq.0)) then
*       fname = title(1:ntitle) // '.FX'
*       subtitle = 'FORECAST OF STOCHASTIC SERIES'
*       if (lamd .eq. 0) then
*        do j = 1,3
*         do i = kp-mq2,kp+mq2
*          splot(i,j) = EXP(splot(i,j))
*         end do
*        end do
*       end if
*       call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
**       write (27,'(A)') fname
*      end if
C
C
      do i = 1,2*kp+1
       do j = 1,3
        splot(i,j) = 0.0d0
       end do
      end do
      do i = kp-mq2,kp+mq2
       splot(i,3) = trend(nz-kp+i)
       if ((nz-kp+i.gt.nz) .and. (Nsfcast.eq.1)) then
        splot(i,3) = trend(nz-kp+i) * Rfact(i-kp)
       end if
       splot(i,1) = splot(i,3) - Setp(i-kp)*alpha
       splot(i,2) = splot(i,3) + Setp(i-kp)*alpha
      end do 
      ymin = splot(kp-mq2,1)
      ymax = splot(kp-mq2,1)
      do j = 1,3
       do i = kp-mq2,kp+mq2
        bser = splot(i,j)
        if (bser .le. ymin) then
         ymin = bser
        end if
        if (bser .ge. ymax) then
         ymax = bser
        end if
       end do
      end do
*      if ((pg .eq. 0).and.(iter.eq.0)) then
*       if (lamd.eq.0)  then
*        if (out.lt.2) then
*         fname = 'FORTC.T5'
*         subtitle = 'FORECAST: TREND-CYCLE COMPONENT'
*         call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*        end if
*       else
*        if (lamd .eq. 1) then
*         subtitle = 'FORECAST: TREND-CYCLE'
*         if (tramo .gt. 0) then
*          if (out.lt.2) then         
*           fname = 'FORT.T5'
*           call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)        
*          end if
*         else
*          if (out.lt.3) then
*           fname = 'FTRFIN.T5'
*           call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*          end if
*         end if
*        end if
*       end if
*      end if
      noseas = .true.
      do i = 1,2*kp+1
       do j = 1,3
        splot(i,j) = 0.0d0
       end do
      end do
      do i = kp-mq2,kp+mq2
       if ((lamd.eq.0) .and. (sc(nz-kp+i).gt.1.0d-9)) then
        noseas = .false.
       end if
       if ((lamd.eq.1) .and. (sc(nz-kp+i).gt.1.0d-9)) then
        noseas = .false.
       end if
       splot(i,3) = sc(nz-kp+i)
       splot(i,1) = splot(i,3) - Sets(i-kp)*alpha
       splot(i,2) = splot(i,3) + Sets(i-kp)*alpha
      end do
      ymin = splot(kp-mq2,1)
      ymax = splot(kp-mq2,1)
      do j = 1,3
       do i = kp-mq2,kp+mq2
        bser = splot(i,j)
        if (bser .le. ymin) then
         ymin = bser
        end if
        if (bser .ge. ymax) then
         ymax = bser
        end if
       end do
      end do
*      if ((pg.eq.0) .and. (.not.noseas) .and.(lamd.eq.1).and.
*     &   (iter.eq.0)) then
*       if ((out.eq.0).or.(out.eq.1).and.(tramo.le.0)) then
*        fname = 'FORSC.T5'
*        subtitle = 'FORECAST: SEASONAL COMPONENT'
*        call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*       end if
*      end if
CC
C
CC
*      if ((pg.eq.0).and.(iter.eq.0).and. 
*     $ ((out.lt.2).or.(out.eq.2).and.(tramo.le.0))) then
*         do i = 1,2*kp+1
*        do j = 1,3
*         splot(i,j) = 0.0d0
*        end do
*       end do
*       do i = kp-mq2,kp+mq2
*        splot(i,3) = sa(nz-kp+i)
*        splot(i,1) = splot(i,3) - Seta(i-kp)*alpha
*        splot(i,2) = splot(i,3) + Seta(i-kp)*alpha
*       end do
*       fname = 'FORSA.T5'
*       subtitle = 'FORECAST: STOCHASTIC SA SERIES'
*       call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*      end if
CC
C
CC
      if (varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. ncyc.ne.1)) then
       do i = 1,2*kp+1
        do j = 1,3
         splot(i,j) = 0.0d0
        end do
       end do
       do i = kp-mq2,kp+mq2
        splot(i,3) = cycle(nz-kp+i)
        splot(i,1) = splot(i,3) - Setc(i-kp)*alpha
        splot(i,2) = splot(i,3) + Setc(i-kp)*alpha
       end do
       ymin = splot(kp-mq2,1)
       ymax = splot(kp-mq2,1)
       bser = 0.0d0
       do j = 1,3
        do i = kp-mq2,kp+mq2
         bser = splot(i,j)
         if (bser .le. ymin) then
          ymin = bser
         end if
         if (bser .ge. ymax) then
          ymax = bser
         end if
        end do
       end do
       bool = .false.
       do i = kp-mq2,kp+mq2
        do j = 1,3
         if (.not.dpeq(splot(i,j), 0.0d0)) then
          bool = .true.
         end if
        end do
       end do
*       if ((pg.eq.0).and.(iter.eq.0).and.(bool).and.(lamd.eq.1)) then
*        if ((out.lt.2).and.(tramo.le.0)) then
*         fname = 'FORYC.T5'
*         subtitle = 'FORECAST: TRANSITORY COMPONENT'
*         call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*        end if
*       end if
      end if
C
C   LINES OF CODE ADDED FOR X-13A-S : 5
      lon = mq
      if (mq .lt. 6) then
       lon = 2 * lon
      end if
      nlen = lon
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
c      if (out .ne. 3) then
C
C
C BEGIN COMPUTATION FOR RATES OF GROWTH
C
C TABLE T11
C
C   LINES OF CODE COMMENTED FOR X-13A-S : 4
C       lon = mq
C       if (mq .lt. 6) then
C        lon = 2 * lon
C       end if
       if (lamd .eq. 0) then
        por = 1.0d0
       end if
       if (lamd .eq. 1) then
        por = 0.01d0
       end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       c2 = mq * 100.0
C       if (nannua .eq. 0) then
C        c2 = 100.0
C       end if
C   END OF CODE BLOCK 
C   LINES OF CODE ADDED FOR X-13A-S : 1
       c2 = 100.0
C   END OF CODE BLOCK 
       c1 = c2 * sqf
       st1p(0) =
     $   c1 *
     $   RAIZ(2*teetre(0)*(1.0-teetre(1))-psiep(nfilt)*psiep(nfilt))
       st1d(0) =
     $   c1 *
     $   RAIZ(2*teeadj(0)*(1.0-teeadj(1))-psiea(nfilt)*psiea(nfilt))
       sigpt1(0) = st1p(0) * por
       sigat1(0) = st1d(0) * por
       do k = 1,lon
        st1p(k) = st1p(k-1) * st1p(k-1)
        st1d(k) = st1d(k-1) * st1d(k-1)
        aux1 = (psiep(nfilt-k)-psiep(nfilt+1-k)) *
     $         (psiep(nfilt-k)-psiep(nfilt+1-k))
        aux2 = (psiea(nfilt-k)-psiea(nfilt+1-k)) *
     $         (psiea(nfilt-k)-psiea(nfilt+1-k))
        aux1 = c1 * c1 * aux1
        aux2 = c1 * c1 * aux2
        st1p(k) = st1p(k) - aux1
        st1d(k) = st1d(k) - aux2
        st1p(k) = RAIZ(st1p(k))
        st1d(k) = RAIZ(st1d(k))
        sigpt1(k) = st1p(k) * por
        sigat1(k) = st1d(k) * por
       end do
       st1p(lon+1) = c1 * RAIZ(2.*feetre(0)*(1.-feetre(1)))
       st1d(lon+1) = c1 * RAIZ(2.*feeadj(0)*(1.-feeadj(1)))
       sigpt1(lon+1) = st1p(lon+1) * por
       sigat1(lon+1) = st1d(lon+1) * por
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       nlen = lon
C
C TABLE T31
C
C AT THIS POINT WE DESTROY THE STANDARD ERROR DUE TO REVISION
C
       if (mq .ge. 6) then
        c3 = 100.0 * sqf
        Serc(0) = rcetre(3)*rcetre(0) + psiep(nfilt+1)*psiep(nfilt-2)
        do k = 1,lon
         Serc(k) = Serc(k-1) - psiep(nfilt+2-k)*psiep(nfilt-k-1)
        end do
        v3fp = 2 * feetre(0) * (1-feetre(3))
        do k = 0,mq2
         Serp(k) = (Serp(-k)/sqf)**2
         Sera(k) = (Sera(-k)/sqf)**2
        end do
C
C
C
        st3p(0) =
     $    c3 *
     $    RAIZ(v3fp+rcetre(0)+psiep(nfilt+1)*psiep(nfilt+1)+Serp(2)-
     $         2*Serc(0))
        do k = 1,lon
         st3p(k) = c3 * RAIZ(v3fp+Serp(k-1)+Serp(k+2)-2*Serc(k))
        end do
        st3p(lon+1) = c3 * RAIZ(v3fp)
        Serc(0) = rceadj(3)*rceadj(0) + psiea(nfilt+1)*psiea(nfilt-2)
        do k = 1,lon
         Serc(k) = Serc(k-1) - psiea(nfilt+2-k)*psiea(nfilt-k-1)
        end do
        v3fd = 2 * feeadj(0) * (1-feeadj(3))
        st3d(0) =
     $    c3 *
     $    RAIZ(v3fd+rceadj(0)+psiea(nfilt+1)*psiea(nfilt+1)+Sera(2)-
     $         2*Serc(0))
        do k = 1,lon
         st3d(k) = c3 * RAIZ(v3fd+Sera(k-1)+Sera(k+2)-2*Serc(k))
        end do
        st3d(lon+1) = c3 * RAIZ(v3fd)
       end if
C
C TASAS ACUMULADAS
C
       if (mq.eq.12 .or. mq.eq.4 .or. mq.eq.6) then
        do i = 1,mq
         adum(i) = 2. * teetre(0) * (1.-teetre(i))
         bdum(i) = 2. * feetre(0) * (1.-feetre(i))
         atdum(i) = 2. * teeadj(0) * (1.-teeadj(i))
         btdum(i) = 2. * feeadj(0) * (1.-feeadj(i))
         aux1 = 0.0
         aux2 = 0.0
         do k = 1,i
          aux1 = aux1 + psiep(nfilt+1-k)*psiep(nfilt+1-k)
          aux2 = aux2 + psiea(nfilt+1-k)*psiea(nfilt+1-k)
         end do
         adum(i) = adum(i) - aux1
         c2 = 100.0
         stpt(i) = mq * (c2/i) * RAIZ(adum(i)*sqf**2)
         stpf(i) = mq * (c2/i) * RAIZ(bdum(i)*sqf**2)
         atdum(i) = atdum(i) - aux2
         stdt(i) = mq * (c2/i) * RAIZ(atdum(i)*sqf**2)
         stdf(i) = mq * (c2/i) * RAIZ(btdum(i)*sqf**2)
         sigptac(i) = stpt(i) * por
         sigatac(i) = stdt(i) * por
         sigptaf(i) = stpf(i) * por
         sigataf(i) = stdf(i) * por
        end do
       end if
C
C TASA T(1 MQ)
C
       if (mq .ne. 1) then
        vx = 1.0d0
        do i = 1,mq/2-1
         vx = vx + psitot(nfilt+1+i)*psitot(nfilt+1+i)
        end do
        vfp = 2. * feetre(0) * (1.-feetre(mq))
        vfa = 2. * feeadj(0) * (1.-feeadj(mq))
        vrpp = 2. * rcetre(0) * (1.-rcetre(mq))
        vraa = 2. * rceadj(0) * (1.-rceadj(mq))
        ap = 0.0d0
        aa = 0.0d0
        bpp = 0.0d0
        ba = 0.0d0
        cp = 0.0d0
        ca = 0.0d0
        do i = 0,mq/2-1
         ap = ap + psiep(nfilt+1+i)*psiep(nfilt+1+i)
         aa = aa + psiea(nfilt+1+i)*psiea(nfilt+1+i)
        end do
        ii = -mq/2 
        if (mq .eq. 3) then
         ii = -1
        end if
        do i = ii,-1
         bpp = bpp + psiep(nfilt+1+i)*psiep(nfilt+1+i)
         ba = ba + psiea(nfilt+1+i)*psiea(nfilt+1+i)
        end do
        do i = 0,mq/2-1
         cp = cp + psiep(nfilt+1+i)*psiep(nfilt+1+i-mq)
         ca = ca + psiea(nfilt+1+i)*psiea(nfilt+1+i-mq)
        end do
        vpp = vrpp + ap - bpp - 2.*cp
        vdd = vraa + aa - ba - 2.*ca
        vcp = vfp + vpp
        vca = vfa + vdd
C
C
C
C      IF (LAMD.EQ.0) THEN
C        IF (OUT.EQ.1) WRITE(NIO,8790)MQ
C 8790   FORMAT(///,10X,'(CENTERED) ESTIMATOR OF THE PRESENT',/,10X,
C     *  'RATE OF ANNUAL GROWTH, T(1',I3,')',
C     *  /,10X,'(LINEAR APPROXIMATION)')
C      END IF
C      IF (LAMD.EQ.1) THEN
C        IF (OUT.EQ.1) WRITE(NIO,8550)
C 8550 FORMAT(///,10X,'(CENTERED) ESTIMATOR OF THE PRESENT',/,10X,
C     *'ANNUAL GROWTH')
C      END IF
C      IF (OUT.EQ.1) WRITE(NIO,8791)
C 8791 FORMAT(/,8X,'STANDARD',6X,'TREND-CYCLE',2X,'SEAS. ADJ.',
C     *5X,'ORIGINAL',/,10X,'ERROR',20X,'SERIES',9X,'SERIES',/)
C
C CONCURRENT ESTIMATOR T(1,MQ)
C
        vmi = RAIZ(vcp*sqf*sqf) * 100.
        vmig = RAIZ(vca*sqf*sqf) * 100.
        vmigu = RAIZ(vx*sqf*sqf) * 100.
        sigptmq(1) = vmi * por
        sigatmq(1) = vmig * por
        sigxtmq(1) = vmigu * por
C      IF (OUT.EQ.1) WRITE(NIO,8792) VMI*POR,VMIG*POR,VMIGU*POR
C 8792 FORMAT(6X,'CONCURRENT',3X,F10.3,3X,F10.3,5X,F10.3,/,6X,
C     *'ESTIMATOR',/)
c        if (out .ne. 0) then
C
C FINAL ESTIMATOR T(1,MQ)
C
         vmi = RAIZ(vfp*sqf*sqf) * 100.
         vmig = RAIZ(vfa*sqf*sqf) * 100.
         sigptmq(2) = vmi * por
         sigatmq(2) = vmig * por
         sigxtmq(2) = 0.0d0
c        end if
       end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
c      end if
C     IF ((LAMD.EQ.0).AND.(OUT.EQ.0).AND.(MQ.GE.6)) THEN
C        WRITE(NIO,2144)ST1P(0)*POR,ST1D(0)*POR,ST3P(0)*POR,
C     $                 ST3D(0)*POR,VMI*POR,VMIG*POR,VMIGU*POR,MQ
C      end if
C      IF ((LAMD.EQ.0).AND.(OUT.EQ.0).AND.(MQ.LT.6)) THEN
C        WRITE(NIO,2145)ST1P(0)*POR,ST1D(0)*POR,
C     $                 VMI*POR,VMIG*POR,VMIGU*POR,MQ
C      end if
C      IF ((LAMD.EQ.1).AND.(OUT.EQ.0).AND.(MQ.GE.6)) THEN
C        WRITE(NIO,2146)ST1P(0)*POR,ST1D(0)*POR,ST3P(0)*POR,
C     $                 ST3D(0)*POR,VMI*POR,VMIG*POR,VMIGU*POR,MQ
C      end if
C      IF ((LAMD.EQ.1).AND.(OUT.EQ.0).AND.(MQ.LT.6)) THEN
C        WRITE(NIO,2147)ST1P(0)*POR,ST1D(0)*POR,
C     $                 VMI*POR,VMIG*POR,VMIGU*POR,MQ
C      end if
C 2144   FORMAT(//,6X,'STANDARD ERROR OF THE CONCURRENT RATES OF ',
C     $  'GROWTH ESTIMATORS',/,4X,'(In points of annualized percent ',
C     $'growth. Linear approximations)',//,33X,'TREND-CYCLE',2X,
C     $'SA SERIES',8X,'ORIGINAL SERIES',//,2X,'PERIOD TO PERIOD RATE ',
C     $5X,G11.3,5X,G11.3,/2X,'OF GROWTH OF THE',/,2X,'SERIES (T11)',
C     $//,2X,'RATE OF GROWTH OF ',9X,G11.3,5X,G11.3,/,2X,
C     $'A 3-PERIOD CENTERED',/,2X,'MOVING AVERAGE (T31)',//,
C     $2X,'(CENTERED) ESTIMATOR',7X,G11.3,6X,G11.3,
C     $10X,G11.3,/2X,'OF THE ANNUAL GROWTH',/,2X '(T 1',I3,')')
C 2145   FORMAT(//,6X,'STANDARD ERROR OF THE CONCURRENT RATES OF ',
C     $  'GROWTH ESTIMATORS',/,4X,'(In points of annualized percent ',
C     $'growth. Linear approximations)',//,33X,'TREND-CYCLE',2X,
C     $'SA SERIES',8X,'ORIGINAL SERIES',//,2X,'PERIOD TO PERIOD RATE ',
C     $5X,G11.3,5X,G11.3,/,2X,'OF GROWTH OF THE',/,2X,'SERIES (T11)',
C     $//,2X,'(CENTERED) ESTIMATOR',7X,G11.3,6X,G11.3,
C     $10X,G11.3,/2X,'OF THE ANNUAL GROWTH',/,2X '(T 1',I3,')')
C 2146   FORMAT(//,6X,'STANDARD ERROR OF THE CONCURRENT RATES OF ',
C     $  'ESTIMATORS',/,4X,'(In points of annualized percent ',
C     $'growth. Linear approximations)',//,33X,'TREND-CYCLE',2X,
C     $'SA SERIES',8X,'ORIGINAL SERIES',//,2X,'PERIOD TO PERIOD GROWTH',
C     $4X,G11.3,5X,G11.3,/2X,'OF THE SERIES (T11)',//,
C     $2X,'PERIOD  GROWTH OF ',9X,G11.3,5X,G11.3,/,2X,
C     $'A 3-PERIOD OF THE',/,2X,'CENTERED SERIES (T31)',//,
C     $2X,'(CENTERED) ESTIMATOR',7X,G11.3,
C     $5X,G11.3,10X,G11.3,/2X,'OF THE ANNUAL GROWTH'/,2X,'(T 1',I3,')')
C 2147   FORMAT(//,6X,'STANDARD ERROR OF THE CONCURRENT RATES OF ',
C     $  'ESTIMATORS',/,4X,'(In points of annualized percent ',
C     $'growth. Linear approximations)',//,33X,'TREND-CYCLE',2X,
C     $'SA SERIES',8X,'ORIGINAL SERIES',//,2X,'PERIOD TO PERIOD GROWTH',
C     $4X,G11.3,5X,G11.3,/2X,'OF THE SERIES (T11)',//,
C     $2X,'(CENTERED) ESTIMATOR',7X,G11.3,
C     $5X,G11.3,10X,G11.3,/2X,'OF THE ANNUAL GROWTH'/,2X,'(T 1',I3,')')
      noC=varwnc.lt.1.0d-10 .and.ncycth.eq.0.and.Ncyc.eq.1
      if (lamd .eq. 1) then
C
C ADDITIVE CASE, LAMD = 1.   SIGMA $ SIGLT ALREADY CALCULATED
C
       do i = 1,nz
        if (IsCloseToTD) then
         sa(i) = oz(i) - sc(i) - cycle(i)
        else
         sa(i) = oz(i) - sc(i)
        end if
       end do
      else if (bias .eq. -1) then
       call BIASCORR(forbias,forsbias,fortbias,trend,sc,z,cycle,ir,sa,
     $               mq,lfor,npsi,noC)
      else
       bias1c = 1.0d0
       bias3c = 1.0d0
       if (bias .eq. 1) then
        bias1c = 0.0d0
        bias2c = 0.0d0
        nyr = (nz/mq) * mq
        do i = 1,nz
         if (i .le. nyr) then
          bias1c = bias1c + EXP(sc(i))
         end if
         bias2c = bias2c + EXP(ir(i))
        end do
        bias1c = bias1c / nyr
        bias2c = bias2c / nz
        bias3c = bias1c * bias2c
        call ABIASC(mq,lfor,oz,trend,z,sc,forbias,forsbias,fortbias,
     $              bias1c,bias3c,xx,npsi,noc)
        if (xx .gt. maxbias) then
          overmaxbias=1
C         write (Nio,'(/,2x,''DIFFERENCES IN ANNUAL AVERAGES'',/2x,
C     $              ''EXCEED THE ALLOWED LIMIT ('',f8.2,'')'',/,2x,
C     $              ''PARAMETER BIAS SET=-1'')') maxbias
C         bias = -1
C         call BIASCORR(forbias,forsbias,fortbias,trend,sc,z,cycle,ir,sa,
C     $                 mq,lfor,npsi)
C         goto 5001
        end if
       end if
       do i = 1,nz
        sc(i) = EXP(sc(i)) / bias1c
        cycle(i) = EXP(cycle(i))
        if (IsCloseToTD) then
         sa(i) = EXP(z(i)) / (sc(i)*cycle(i))
        else
         sa(i) = EXP(z(i)) / sc(i)
        end if
        trend(i) = EXP(trend(i)) * bias3c
        if (IsCloseToTD) then
         ir(i) = LOG(sa(i)/trend(i))
        else
         ir(i) = LOG(sa(i)/(trend(i)*cycle(i)))
        end if
        sc(i) = 100.0d0 * sc(i)
        cycle(i) = 100.0d0 * cycle(i)
        ir(i) = 100.0d0 * EXP(ir(i))
       end do
       do i = 1,lfor
        k = nz + i
        if (npsi.ne.1) then
          sc(k) = EXP(sc(k)) / bias1c
        else
          sc(k) = exp(sc(k))
        endif
        if (npsi.ne.1 .or.  .not.noC)then
          trend(k) = EXP(trend(k)) * bias3c
        else
          trend(k) = EXP(trend(k)) 
        endif
        cycle(k) = EXP(cycle(k))
        if (IsCloseToTD)then
         sa(k) = EXP(z(k)) / (sc(k)*cycle(k))
        else
         sa(k) = EXP(z(k)) / sc(k)
        end if
        cycle(k) = cycle(k) * 100.0d0
        sc(k) = 100.0d0 * sc(k)
       end do
       do i = 1,59
        if (npsi.ne.1) then
          forsbias(i) = EXP(forbias(i)) / (EXP(forsbias(i))/bias1c)
        else
          forsbias(i) = EXP(forbias(i)) / (EXP(forsbias(i)))
        endif
        if (npsi.ne.1 .or.  .not.noC)then
          fortbias(i) = EXP(fortbias(i)) * bias3c
        else
          fortbias(i) = EXP(fortbias(i)) 
        endif
        forbias(i) = EXP(forbias(i))
       end do
      end if
C
C antes era out=2 !
 5001 if (out .eq. 0) then
       write (Nio,'(6X,''BIAS PARAMETER ='',I2)') bias
      end if
      do i = 1,2*kp+1
       do j = 1,3
        splot(i,j) = 0.0d0
       end do
      end do
      do i = kp-mq2,kp+mq2
       if (lamd .eq. 0) then
        splot(i,3) = LOG(trend(nz-kp+i))
        if ((nz-kp+i.gt.nz) .and. (Nsfcast.eq.1)) then
         splot(i,3) = LOG(trend(nz-kp+i)) * Rfact(i-kp)
        end if
        splot(i,1) = splot(i,3) - Setp(i-kp)*alpha
        splot(i,2) = splot(i,3) + Setp(i-kp)*alpha
       else
        splot(i,3) = trend(nz-kp+i)
        if ((nz-kp+i.gt.nz) .and. (Nsfcast.eq.1)) then
         splot(i,3) = trend(nz-kp+i) * Rfact(i-kp)
        end if
        splot(i,1) = splot(i,3) - Setp(i-kp)*alpha
        splot(i,2) = splot(i,3) + Setp(i-kp)*alpha
       end if
      end do
c      ymin = splot(kp-mq2,1)
c      ymax = splot(kp-mq2,1)
c      do j = 1,3
c       do i = kp-mq2,kp+mq2
c        bser = splot(i,j)
c        if (bser .le. ymin) then
c         ymin = bser
c        end if
c        if (bser .ge. ymax) then
c         ymax = bser
c        end if
c       end do
c      end do
*      if ((iter.eq.0).and.(pg.eq.0).and.(lamd.eq.0)) then      
*       subtitle = 'FORECAST: TREND-CYCLE'
*       do j = 1,3
*        do i = kp-mq2,kp+mq2
*         splot(i,j) = EXP(splot(i,j))
*        end do
*       end do
*       if (tramo .gt. 0) then
*        if (out.lt.2) then
*         fname = 'FORT.T5'
*         call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1) 
*        end if
*       else
*        if (out.lt.3) then 
*         fname = 'FTRFIN.T5'
*         call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*        end if
*       end if  
*      end if
C
C
*      if ((iter.ne.0) .and. (ioneout.eq.0) .and. (tramo.le.0) .and. 
*     $    (out.lt.2).and.(pg.eq.0)) then
*       fname = title(1:ntitle) // '.FTR'
*       subtitle = 'FORECAST: TREND-CYCLE'
*       if (lamd .eq. 0) then
*        do j = 1,3
*         do i = kp-mq2,kp+mq2
*          splot(i,j) = EXP(splot(i,j))
*         end do
*        end do
*       end if
*       call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
**       write (27,'(A)') fname
*      end if
C
C
      noseas = .true.
      do i = 1,2*kp+1
       do j = 1,3
        splot(i,j) = 0.0d0
       end do
      end do
      do i = kp-mq2,kp+mq2
       if (lamd.eq.0)THEN
        if(LOG(sc(nz-kp+i)/100.0d0).gt.1.0d-9) noseas = .false.
       end if
       if ((lamd.eq.1) .and. (sc(nz-kp+i).gt.1.0d-9)) then
        noseas = .false.
       end if
       if (lamd .eq. 0) then
        splot(i,3) = LOG(sc(nz-kp+i)/100.0d0)
        splot(i,1) = splot(i,3) - Sets(i-kp)*alpha
        splot(i,2) = splot(i,3) + Sets(i-kp)*alpha
       else
        splot(i,3) = sc(nz-kp+i)
        splot(i,1) = splot(i,3) - Sets(i-kp)*alpha
        splot(i,2) = splot(i,3) + Sets(i-kp)*alpha
       end if
      end do
c c     ymin = splot(kp-mq2,1)
c      ymax = splot(kp-mq2,1)
c      do j = 1,3
c       do i = kp-mq2,kp+mq2
c        bser = splot(i,j)
c        if (bser .le. ymin) then
c         ymin = bser
c        end if
c        if (bser .ge. ymax) then
c         ymax = bser
c        end if
c       end do
c      end do
*      if ((pg.eq.0) .and. (.not.noseas) .and.(lamd.eq.0).and.
*     $ (iter.eq.0)) then       
*       if ((out.lt.2).or. (out.eq.2).and.(tramo.le.0)) then
*        subtitle = 'FORECAST: SEASONAL FACTORS'
*        do j = 1,3
*         do i = kp-mq2,kp+mq2
*          splot(i,j) = 100.0d0 * EXP(splot(i,j))
*         end do
*        end do
*        if (tramo .gt. 0) then
*         fname = 'FORSF.T5'
*        else
*         fname = 'FSFIN.T5'
*        end if
*        call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*       end if
*      end if
CC
C
CC
*      if ((pg.eq.0).and.(iter.eq.0).and.
*     &    ((out.lt.2).or.(out.eq.2).and.(tramo.le.0))) then
*       do i = 1,2*kp+1
*        do j = 1,3
*         splot(i,j) = 0.0d0
*        end do
*       end do
*       do i = kp-mq2,kp+mq2
*        splot(i,3) = sa(nz-kp+i)
*        splot(i,1) = splot(i,3) - Seta(i-kp)*alpha
*        splot(i,2) = splot(i,3) + Seta(i-kp)*alpha
*       end do
*       fname = 'FORSA.T5'
*       subtitle = 'FORECAST: STOCHASTIC SA SERIES'
*       call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*      end if
CC
C
CC
C
C
      if (varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. ncyc.ne.1)) then
       do i = 1,2*kp+1
        do j = 1,3
         splot(i,j) = 0.0d0
        end do
       end do
       do i = kp-mq2,kp+mq2
        if (lamd .eq. 0) then
         splot(i,3) = LOG(cycle(nz-kp+i)/100.0d0)
         splot(i,1) = splot(i,3) - Setc(i-kp)*alpha
         splot(i,2) = splot(i,3) + Setc(i-kp)*alpha
        else
         splot(i,3) = cycle(nz-kp+i)
         splot(i,1) = splot(i,3) - Setc(i-kp)*alpha
         splot(i,2) = splot(i,3) + Setc(i-kp)*alpha
        end if
       end do
       bool = .false.
       do i = kp-mq2,kp+mq2
        do j = 1,3
         if (.not.dpeq(splot(i,j), 0.0d0)) then
          bool = .true.
         end if
        end do
       end do
*       if  (bool)  then
*        if (iter.eq.0) then
*         if (out.lt.2) then
*          fname = 'FORYC.T5'
*          write(subtitle,'("FORECAST: ",A," COMPONENT")') 
*     $          transLcad(1:nTransLCad)
*          call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*          if (lamd .eq. 0) then
*           if (tramo .gt. 0) then
*            fname = 'FORYF.T5'
*           else
*            fname = 'FTRAFIN.T5'
*           end if
*           write(subtitle,'("FORECAST: ",A," FACTORS")') 
*     $           transLcad(1:nTransLCad)
*           do j = 1,3
*            do i = kp-mq2,kp+mq2
*             splot(i,j) = 100.0d0 * EXP(splot(i,j))
*            end do
*           end do
*           call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
*          end if
*         end if
*        else
*         if (ioneout.ne.0 .or. tramo.ge.0.or. out.ne.0) return
*          if (lamd .eq. 0) then
*           do j = 1,3
*            do i = kp-mq2,kp+mq2
*             splot(i,j) = EXP(splot(i,j))
*            end do
*           end do
*          end if
*          fname = title(1:ntitle) // '.FCY'
*          write(subtitle,'("FORECAST: ",A)') transLCad(1:ntransLcad)
*          call PLOTFCAST2(fname,subtitle,splot,mq2,nz,1)
**          write (27,'(A)') fname
*         end if
*        end if
       end if
      end
C
C
C
      subroutine SMRFACT(rfact,mq2,mq)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer kp
      parameter (kp = PFCST)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 rfact(kp)
C.. In/Out Status: Read, Not Written ..
      integer mq2
C.. In/Out Status: Read, Not Written ..
      integer mq
C
C.. Local Scalars ..
      integer i,k
      real*8 sum
C
C.. Local Arrays ..
      real*8 rs(kp),rstmp(-kp:kp)
C
C.. Intrinsic Functions ..
      intrinsic MOD
C
C ... Executable Statements ...
C
      do i = 1,mq2
       rstmp(i) = rfact(i)
      end do
      if (MOD(mq,2) .eq. 1) then
       do i = 0,(mq-1)/2-1
        rstmp(-i) = 1.0d0
       end do
       do i = 0,(mq-1)/2
        sum = 0.0d0
        do k = 0,2*i
         sum = sum + rstmp(mq2-k)
        end do
        rs(mq2-i) = (1.0d0/(2.0d0*i+1)) * sum
       end do
       do i = mq2-(mq-1)/2,1,(-1)
        sum = 0.0d0
        do k = -(mq-1)/2,(mq-1)/2
         sum = sum + rstmp(i+k)
        end do
        rs(i) = (1/(mq*1.0d0)) * sum
       end do
      else
       do i = 0,mq/2-1
        rstmp(-i) = 1.0d0
       end do
       rs(mq2) = rstmp(mq2)
       do i = 1,mq/2
        sum = 0.0d0
        do k = 1,2*i-1
         sum = sum + rstmp(mq2-k)
        end do
        rs(mq2-i) = (1/(4.d0*i)) * (rstmp(mq2)+2.d0*sum+rstmp(mq2-2*i))
       end do
       do i = mq2-(mq/2)-1,1,(-1)
        sum = 0.0d0
        do k = -(mq/2)+1,(mq/2)-1
         sum = sum + rstmp(i+k)
        end do
        rs(i) = (1/(2.d0*mq)) * (rstmp(i-mq/2)+2.0d0*sum+rstmp(i+mq/2))
       end do
      end if
      do i = 1,mq2
       rfact(i) = rs(i)
      end do
      end
C
C
C
      subroutine Seasign(Npsi,lamd,alpha,sc,sets,sefes,nz,
     $                   mq2,mq,nthclass,ntcclass,ntfclass,out)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
C
C.. Formal Arguments ..
      integer nz,mq2,mq,nthclass,ntcclass,ntfclass,out,Npsi,lamd
      real*8 sc(mpkp),sets(-kp:kp),sefes,alpha
C
C.. Local Scalars ..
      integer i,j,k,ntc90,ntc95,nth3,ntcseas,ntf90,ntf95,ntfseas,
     $        nth90,nth95,nthseas
      real*8 const90,const95,const3,a1,a2,a3,a4,a5,a6,a7,a8,dvec
C
C.. Local Arrays ..
      real*8 tcseas(kp),tfseas(kp),thseas(kp)
      dimension a1(1),a2(1),a3(1),a4(1),a5(1),a6(1),a7(1),a8(1),dvec(1)
C
C.. External Calls ..
      external CLASSIFY
C
C.. Intrinsic Functions ..
      intrinsic ABS
      include 'stream.i'
C
C ... Executable Statements ...
C      
      nth95=0
      ntc95=0
      nth3=0
      ntf95=0
      nth90=0
      ntc90=0
      ntf90=0
      if (lamd .eq. 0) then
       a1(1) = EXP(Sefes*alpha) * 100.0
       a2(1) = EXP(Sets(0)*alpha) * 100.0
       a3(1) = EXP(Sefes*1.037d0) * 100.0
       a4(1) = EXP(Sets(0)*1.037d0) * 100.0
       a5(1) = EXP(-Sefes*alpha) * 100.0
       a6(1) = EXP(-Sets(0)*alpha) * 100.0
       a7(1) = EXP(-Sefes*1.037d0) * 100.0
       a8(1) = EXP(-Sets(0)*1.037d0) * 100.0
       call USRENTRY(a5,1,1,1,1,1507)
       call USRENTRY(a1,1,1,1,1,1508)
       call USRENTRY(a6,1,1,1,1,1509)
       call USRENTRY(a2,1,1,1,1,1510)
       call USRENTRY(a7,1,1,1,1,1511)
       call USRENTRY(a3,1,1,1,1,1512)
       call USRENTRY(a8,1,1,1,1,1513)
       call USRENTRY(a4,1,1,1,1,1514)
      else
       a1(1) = Sefes * alpha
       a2(1) = Sets(0) * alpha
       a3(1) = Sefes * 1.037d0
       a4(1) = Sets(0) * 1.037d0
       dvec(1)=-a1(1)
       call USRENTRY(dvec,1,1,1,1,1507)
       call USRENTRY(a1,1,1,1,1,1508)
       dvec(1)=-a2(1)
       call USRENTRY(dvec,1,1,1,1,1509)
       call USRENTRY(a2,1,1,1,1,1510)
       dvec(1)=-a3(1)
       call USRENTRY(dvec,1,1,1,1,1511)
       call USRENTRY(a3,1,1,1,1,1512)
       dvec(1)=-a4(1)
       call USRENTRY(dvec,1,1,1,1,1513)
       call USRENTRY(a4,1,1,1,1,1514)
      end if      
      if (Npsi.gt.1) then
       If (out.eq.0) then
         write (nio,'(//,2x,''SIGNIFICANCE OF SEASONALITY'',/,2x,
     $       ''---------------------------'',/)')
       end if
       if (ABS(sefes) .lt. 1.0d-8) then
        If (out.eq.0) then
          write (nio,'(//,4x,''FINAL ESTIMATION ERROR VARIANCE '',
     &                   ''OF SA IS ZERO''
     &            ,/,4x,''SEASONAL SIGNIFICANCE TEST SKIPPED'')')
        end if 
        call setSsh(nth95)
        call setSsp2(ntc95)
        call setSsf(ntf95)
        call setESS(nth95,nth3,MQ)
        return
       end if
      else 
        call setSsh(nth95)
        call setSsp2(ntc95)
        call setSsf(ntf95)
        call setESS(nth95,nth3,MQ)
        return    
      end if
      const90 = 1.645d0
      const95 = 1.96d0
      const3  = 3.0d0
      do i = -mq2,-mq-1
       j = nz + i
       k = i + mq2 + 1
       thseas(k) = ABS(sc(j)/sefes)
      end do
      nthseas = mq
      do i = -mq+1,0
       j = nz + i
       k = i + mq
       tcseas(k) = ABS(sc(j)/sets(i))
      end do
      ntcseas = mq
      do i = 1,mq
       j = nz + i
       tfseas(i) = ABS(sc(j)/sets(i))
      end do
      ntfseas = mq
      call CLASSIFY(thseas,nthseas,mq,nthclass)
      call CLASSIFY(tcseas,ntcseas,mq,ntcclass)
      call CLASSIFY(tfseas,ntfseas,mq,ntfclass)
      if (ntfclass .eq. -1) then
       nthclass = -1
       ntcclass = -1
      end if
      if (ntcclass .eq. -1) then
       nthclass = -1
      end if
      if (ntfclass .eq. 0) then
       if (nthclass .eq. 1) then
        nthclass = 0
       end if
       if (ntcclass .eq. 1) then
        ntcclass = 0
       end if
      end if
      if (ntcclass.eq.0 .and. nthclass.eq.1) then
       nthclass = 0
      end if
      if (out.eq.0) then
         write (nio,'(4x,''Significance of seasonality is '',
     $      ''assessed using the variances of the'',/,4x,
     $      ''total estimation error, which includes the '',
     $      ''error in the preliminary estimator'',/,4x,
     $ ''(the revision error) and the error in the final estimator.'')')
         write (nio,'(4x,''Because the S.E. of the seasonal '',
     $     ''component estimator varies (it reaches a minimum'',/,4x,
     $     ''for historical estimation and a maximum for the most '',
     $     ''distant forecast), the significance of seasonality'',/,4x,
     $     ''will be different for different periods.'',/,4x,
     $     ''An extreme example would be a series showing '',
     $     ''significant seasonality for historical estimates,'',/,4x,
     $     ''that is poorly captured concurrently, and useless for '',
     $     ''forecasting.'')')   
      end if
C
C
C
      do i = 1,nthseas
       if (thseas(i) .ge. const90) then
        nth90 = nth90 + 1
       end if
       if (thseas(i) .ge. const95) then
        nth95 = nth95 + 1
       end if
       if (thseas(i) .ge. const3) then
        nth3  = nth3 + 1
       end if
      end do
      do i = 1,ntcseas
       if (tcseas(i) .ge. const90) then
        ntc90 = ntc90 + 1
       end if
       if (tcseas(i) .ge. const95) then
        ntc95 = ntc95 + 1
       end if
      end do
      do i = 1,ntfseas
       if (tfseas(i) .ge. const90) then
        ntf90 = ntf90 + 1
       end if
       if (tfseas(i) .ge. const95) then
        ntf95 = ntf95 + 1
       end if
      end do
      call setSsh(nth95)
      call setSsp2(ntc95)
      call setSsf(ntf95)
      call setESS(nth95,nth3,MQ)
c     call usrentry(ntf95*1.0d0,1,1,1,1,1038)

      if (out.eq.0) then 
        write (nio,
     $'(//,8x,''SEASONAL'',22x,''NUMBER OF PERIODS IN A YEAR THAT''
     $,/,8x,''COMPONENT'',21x,''HAVE SIGNIFICANT SEASONALITY'',/,44x,
     $       ''90%'',10x,''95%'')')
        write (nio,'(/,4X,''HISTORICAL ESTIMATOR'',19X,I3,10X,I3)')
     $       nth90, nth95
        write (nio,'(/,4X,''PRELIMINARY ESTIMATOR'',18X,I3,10X,I3)')
     $       ntc90, ntc95
        write (nio,'(4X,''FOR LAST YEAR'')')
        write (nio,'(/,4X,''FORECAST FOR NEXT YEAR'',17X,I3,10X,I3)')
     $       ntf90, ntf95
C
        write (nio,'(//,4x,''For the present series :'',/,4x,
     $           ''------------------------'',/)')
        if (nthclass .eq. -1) then
         write (nio,'(6x,''FINAL OR HISTORICAL ESTIMATION SHOWS '',
     $    ''CLEARLY SIGNIFICANT SEASONALITY IN THE SERIES.'',/)')
        else if (nthclass .eq. 0) then
         write (nio,'(6x,''FINAL OR HISTORICAL ESTIMATION SHOWS '',
     $ ''BORDERLINE SIGNIFICANT SEASONALITY IN THE SERIES.'',/)')
        else
         write (nio,'(6x,''FINAL OR HISTORICAL ESTIMATION SHOWS '',
     $             ''NOT SIGNIFICANT SEASONALITY IN THE SERIES.'',/)')
        end if
        if (ntcclass .eq. -1) then
         write (nio,'(6x,''CONCURRENT AND PRELIMINARY '',
     $             ''ESTIMATION SHOW CLEARLY SIGNIFICANT SEASONALITY '',
     $             ''FOR RECENT PERIODS (LAST YEAR).'',/)')
        else if (ntcclass .eq. 0) then
         write (nio,'(6x,''CONCURRENT AND PRELIMINARY '',
     $     ''ESTIMATION SHOW BORDERLINE SIGNIFICANT SEASONALITY '',
     $     ''FOR RECENT PERIODS (LAST YEAR).'',/)')
        else
         write (nio,'(6x,''CONCURRENT AND PRELIMINARY '',
     $             ''ESTIMATION SHOW NOT SIGNIFICANT SEASONALITY '',
     $             ''FOR RECENT PERIODS (LAST YEAR).'',/)')
        end if
        if (ntfclass .eq. -1) then
         write (nio,'(6x,''ONE-YEAR AHEAD FORECAST FUNCTION '',
     $             ''CONTAINS CLEARLY SIGNIFICANT SEASONALITY.'',/)')
        else if (ntfclass .eq. 0) then
         write (nio,'(6x,''ONE-YEAR AHEAD FORECAST FUNCTION '',
     $             ''CONTAINS BORDERLINE SIGNIFICANT SEASONALITY.'',/)')
        else
         write (nio,'(6x,''ONE-YEAR AHEAD FORECAST FUNCTION '',
     $             ''CONTAINS NOT SIGNIFICANT SEASONALITY.'',/)')
        end if
        if (((nthclass.eq.0).and.(ntcclass.eq.1).and.(ntfclass.eq.1))
     $ .or.((nthclass.eq.1).and.(ntcclass.eq.1).and.(ntfclass.eq.1)))
     $    then
         write (nio,'(/,20x,''"SEASONALITY IS NOT PRESENT OR IS '',
     $''TOO WEAK TO BE ACCURATELY CAPTURED.'',/,20x,
     $''THE SERIES, POSSIBLY, SHOULD NOT BE SEASONALLY ADJUSTED.'',
     $/,20x,''TO OBTAIN THE TREND-CYCLE, '',
     $''SIMPLY LET THE SEASONAL COMPONENT BE ADDED TO'',/,20x,
     $''THE IRREGULAR IN THE PRESENT RUN, OR TRY A '',
     $''NON-SEASONAL MODEL."'',/)')
       end if
      end if
      if (lamd.eq.0) then 
       if (out .eq. 0) then
 7042    format (
     $  ///' CONFIDENCE INTERVAL AROUND A SEASONAL FACTOR OF 100',/
     $  ' ---------------------------------------------------',//28x,
     $  'FINAL ESTIMATOR',24x,'CONCURRENT ESTIMATOR'/'    95%'/
     $  ' CONFIDENCE',12x,2g11.4,20x,2g11.4,/' INTERVAL'//'    70%'/
     $  ' CONFIDENCE',12x,2g11.4,20x,2g11.4,/' INTERVAL'/)
         write (Nio,7042) a5(1), a1(1), a6(1), a2(1),
     &                    a7(1), a3(1), a8(1), a4(1)
       end if
      else
       if (out .eq. 0) then
 7043    format (
     $  ///' CONFIDENCE INTERVAL AROUND A SEASONAL COMPONENT OF 0',/
     $  ' ----------------------------------------------------',//20x,
     $  'FINAL ESTIMATOR',20x,'CONCURRENT ESTIMATOR'/'    90%'/
     $  ' CONFIDENCE',12x,2g11.4,20x,2g11.4,/' INTERVAL'//'    70%'/
     $  ' CONFIDENCE',12x,2g11.4,20x,2g11.4,/' INTERVAL'/)
         write (Nio,7043) -a1(1), a1(1), -a2(1), a2(1),
     &                    -a3(1), a3(1), -a4(1), a4(1)
       end if
      end if
      end
C
C
C
      subroutine CLASSIFY(val,nval,mq,svalue)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer kp
      parameter (kp = PFCST)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 val(kp)
C.. In/Out Status: Maybe Read, Not Written ..
      integer nval
C.. In/Out Status: Read, Not Written ..
      integer mq
C.. In/Out Status: Not Read, Overwritten ..
      integer svalue
C
C.. Local Scalars ..
      integer i
      real*8 count1,count2,count3
C
C ... Executable Statements ...
C
C
C sVALUE IS THE SIGNIFICANCE :
C                              -1 CLEARLY SIGNIFICANT
C                               0 BORDERLINE SIGNIFICANT
C                               1 NOT SIGNIFICANT
C
      svalue = 1
      if (mq .eq. 2) then
       do i = 1,nval
        if (val(i) .gt. (2.1d0)) then
         svalue = -1
        end if
       end do
       if (svalue .ne. 1) return
       do i = 1,nval
        if (val(i) .gt. (1.9d0)) then
         svalue = 0
        end if
       end do
      else if (mq .eq. 3) then
       count1 = 0
       count2 = 0
       do i = 1,nval
        if (val(i) .gt. (2.2d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (2.0d0)) then
         count2 = count2 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = -1
       else if (count2 .ge. 2) then
        svalue = -1
       end if
       if (svalue .ne. 1) return
       count1 = 0
       count2 = 0
       do i = 1,nval
        if (val(i) .gt. (2.0d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (1.8d0)) then
         count2 = count2 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = 0
       else if (count2 .ge. 2) then
        svalue = 0
       end if
      else if (mq .eq. 4) then
       count1 = 0
       count2 = 0
       count3 = 0
       do i = 1,nval
        if (val(i) .gt. (2.5d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (2.2d0)) then
         count2 = count2 + 1
        end if
        if (val(i) .gt. (2.0d0)) then
         count3 = count3 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = -1
       else if (count2 .ge. 2) then
        svalue = -1
       else if (count3 .ge. 3) then
        svalue = -1
       end if
       if (svalue .ne. 1) return
       count1 = 0
       count2 = 0
       do i = 1,nval
        if (val(i) .gt. (2.2d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (1.9d0)) then
         count2 = count2 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = 0
       else if (count2 .ge. 2) then
        svalue = 0
       end if
      else if (mq .eq. 6) then
       count1 = 0
       count2 = 0
       count3 = 0
       do i = 1,nval
        if (val(i) .gt. (5.0d0)) then
         count3 = count3 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = -1
       else if (count2 .ge. 2) then
        svalue = -1
       else if (count3 .ge. 3) then
        svalue = -1
       end if
       if (svalue .ne. 1) return
       count1 = 0
       count2 = 0
       do i = 1,nval
        if (val(i) .gt. (2.5d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (1.9d0)) then
         count2 = count2 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = 0
       else if (count2 .ge. 2) then
        svalue = 0
       end if
      else if (mq .eq. 12) then
       count1 = 0
       count2 = 0
       count3 = 0
       do i = 1,nval
        if (val(i) .gt. (3.0d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (2.5d0)) then
         count2 = count2 + 1
        end if
        if (val(i) .gt. (2.0d0)) then
         count3 = count3 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = -1
       else if (count2 .ge. 2) then
        svalue = -1
       else if (count3 .ge. 3) then
        svalue = -1
       end if
       if (svalue .ne. 1) return
       count1 = 0
       count2 = 0
       count3 = 0
       do i = 1,nval
        if (val(i) .gt. (2.5d0)) then
         count1 = count1 + 1
        end if
        if (val(i) .gt. (2.0d0)) then
         count2 = count2 + 1
        end if
        if (val(i) .gt. (1.85d0)) then
         count3 = count3 + 1
        end if
       end do
       if (count1 .ge. 1) then
        svalue = 0
        return
       end if
       if (count2 .ge. 2) then
        svalue = 0
       else if (count3 .ge. 3) then
        svalue = 0
       end if
      end if
      end
C
C
C

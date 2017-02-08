C     Last change:  BCM  29 Aug 2005 10:02 am
**==savacf.f    processed by SPAG 4.03F  at 10:31 on 29 Jul 1994
      SUBROUTINE acfdgn(Nefobs,A,Na,Mxlag,Nlagbl,Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     acfdgn() saves diagnostics related to the residual autocorrelation
c     function to the log file and to the unified diagnostics file.
c     Written by BCM July 2007
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
*      INCLUDE 'title.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
c     ------------------------------------------------------------------
      LOGICAL F,T
      INTEGER PR
      DOUBLE PRECISION PT5
      PARAMETER(F=.FALSE.,T=.true.,PR=PLEN/4,PT5=0.05D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION A(PLEN),smpac,seacf,tacflg
      INTEGER Nlagbl,i1,i2,i3,ilag,i,np,endlag,Nefobs,Na,Mxlag
      LOGICAL Ldiag
      DIMENSION smpac(PR),seacf(PR)
c-----------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
c     Compute residual ACF
c-----------------------------------------------------------------------
      IF(Mxlag.eq.0)THEN
       IF(Sp.eq.1)THEN
        Mxlag=10
       ELSE
        Mxlag=2*Sp
       END IF
       Mxlag=min(Mxlag,Nefobs/4)
      ELSE
c     ------------------------------------------------------------------
       Mxlag=min(Mxlag,Nefobs-1)
      END IF
c     ------------------------------------------------------------------
      np=0
      endlag=Opr(Nopr)-1
      DO ilag=1,endlag
       IF(.not.Arimaf(ilag))np=np+1
      END DO
c     ------------------------------------------------------------------
      CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,
     &         0,T,F)
c     ------------------------------------------------------------------
      i1=0
      i2=0
      IF(Ldiag.or.Svltab(LSLLBQ))THEN
       DO i=1,Nlagbl
        IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)i1=i1+1
        IF(dabs(smpac(i)/seacf(i)).gt.Acflim)i2=i2+1
       END DO
      END IF
c     ------------------------------------------------------------------
c     Print LB-Q information in log file
c     ------------------------------------------------------------------
      IF(Svltab(LSLLBQ))THEN
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,T,T)    
       IF(i1.eq.0)THEN
        WRITE(Ng,1140)'Ljung-Box'
       ELSE
        WRITE(Ng,1120)'Ljung-Box'
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)
     &      WRITE(Ng,1130)i,Qs(i),Dgf(i),Qpv(i)
       END DO
        WRITE(Ng,'()')
       END IF
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,F,T)    
      END IF
c     ------------------------------------------------------------------
c     Print LB-Q information in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       WRITE(Nform,1160)'lb',i1
       IF(i1.gt.0)THEN
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)
     &      WRITE(Nform,1150)'lb',i,Qs(i),Dgf(i),Qpv(i)
        END DO
       END IF
      END IF
c     ------------------------------------------------------------------
c     Now produce BP-Q information
c     ------------------------------------------------------------------
      i3=0
      IF(Ldiag.or.Svltab(LSLLBQ+1))THEN
       CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,
     +         1,T,F)
c     ------------------------------------------------------------------
       DO i=1,Nlagbl
        IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)i3=i3+1
       END DO
      END IF
c     ------------------------------------------------------------------
c     Print BP-Q information in log file
c     ------------------------------------------------------------------
      IF(Svltab(LSLLBQ+1))THEN
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,T,T)    
       IF(i3.eq.0)THEN
        WRITE(Ng,1140)'Box-Pierce'
       ELSE
        WRITE(Ng,1120)'Box-Pierce'
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)
     &      WRITE(Ng,1130)i,Qs(i),Dgf(i),Qpv(i)
        END DO
        WRITE(Ng,'()')
       END IF
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,F,T)    
      END IF
c     ------------------------------------------------------------------
c     Print BP-Q information in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       WRITE(Nform,1160)'bp',i3
       IF(i3.gt.0)THEN
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.PT5)
     &      WRITE(Nform,1150)'bp',i,Qs(i),Dgf(i),Qpv(i)
        END DO
       END IF
c     ------------------------------------------------------------------
c     Print significant ACF lags in udg file
c     ------------------------------------------------------------------
       WRITE(Nform,1110)Acflim
       WRITE(Nform,1170)'acf',i2
       IF(i2.gt.0)THEN
        DO i=1,Nlagbl
         tacflg=smpac(i)/seacf(i)
         IF(dabs(tacflg).gt.Acflim)
     &      WRITE(Nform,1180)'acf',i,smpac(i),seacf(i),tacflg
        END DO
       END IF
      END IF
c     ------------------------------------------------------------------
c     Print significant PACF lags in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       CALL pacf(Nefobs,Sp,smpac,seacf,Mxlag,F)
       i2=0
       DO i=1,Nlagbl
        IF(dabs(smpac(i)/seacf(i)).gt.Acflim)i2=i2+1
       END DO
       WRITE(Nform,1170)'pacf',i2
       IF(i2.gt.0)THEN
        DO i=1,Nlagbl
         tacflg=smpac(i)/seacf(i)
         IF(dabs(tacflg).gt.Acflim)
     &      WRITE(Nform,1180)'pacf',i,smpac(i),seacf(i),tacflg
        END DO
       END IF
      END IF
c     ------------------------------------------------------------------
 1110 FORMAT('acflimit: ',f7.4)
 1120 FORMAT(5x,'Summary of Significant ',a,' Q:',/,
     &       5x,'Lag',5x,'   Q   ',5x,' DF',5x,'  P',/,
     &       5x,'---',5x,'-------',5x,'---',5x,'-----')
 1130 FORMAT(5x,i3,5x,f7.3,5x,i3,5x,f6.3)
 1140 FORMAT(5x,'No significant ',a,' Qs',/)
 1150 FORMAT(a,'q$',i2.2,': ',f7.3,5x,i3,5x,f6.3)
 1160 FORMAT('n',a,'q: ',i3)
 1170 FORMAT('nsig',a,': ',i3)
 1180 FORMAT('sig',a,'$',i2.2,': ',f7.4,5x,f7.4,3x,f7.4)
c-----------------------------------------------------------------------
      RETURN
      END

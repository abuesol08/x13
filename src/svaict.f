      SUBROUTINE svaict(Savtd,Savlom,Saveas,Savusr,Lsvlog,Hvmdl,Lsumm,
     &                  Mdltxt,Nmdtxt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO
      PARAMETER(T=.TRUE.,F=.FALSE.,ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER rgstr*(30),rgabb*(6),Mdltxt*(9)
      INTEGER nrgchr,nrgabb,Lsumm,iaic,Nmdtxt
      LOGICAL Savtd,Savlom,Saveas,Savusr,Lsvlog,Hvmdl
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
      IF(Savtd)THEN
c-----------------------------------------------------------------------
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           '1-Coefficient Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           'Stock Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           '1-Coefficient Stock Trading Day')
        IF(iaic.gt.0)THEN
         CALL mktdlb(rgstr,nrgchr,Aicint,Aicstk,Tddate,Tdzero,Sp)
         IF(Lfatal)RETURN
         IF(Lsvlog)WRITE(Ng,1030)'   AICtd : '//rgstr(1:nrgchr)
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: ',rgstr(1:nrgchr)
        ELSE
         IF(Lsvlog)WRITE(Ng,1030)'   AICtd : none'
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.td',Dfaict
         IF(Rgaicd(PTDAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.td',Rgaicd(PTDAIC)
        END IF
       ELSE
        IF(Lsvlog)
     &     WRITE(Ng,1030)'   AICtd : ARIMA model not '//Mdltxt(1:Nmdtxt)
        IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savlom)THEN
       CALL mklnlb(rgstr,nrgchr,rgabb,nrgabb,Lomtst,Lndate,Lnzero,Sp)
       IF(Lsumm.gt.0)
     &    WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//'.reg: ',
     &                     rgstr(1:nrgchr)
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Length-of-Month')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           'Length-of-Quarter')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Leap Year')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)
     &      WRITE(Ng,1030)'   AIC'//rgabb(1:nrgabb)//' : accepted'
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//': yes'
        ELSE
         IF(Lsvlog)
     &      WRITE(Ng,1030)'   AIC'//rgabb(1:nrgabb)//' : rejected'
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//': no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.'//rgabb(1:nrgabb),Dfaicl
         IF(Rgaicd(PTDAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.'//rgabb(1:nrgabb),
     &                       Rgaicd(PLAIC)
        END IF
       ELSE
        IF(Lsvlog)WRITE(Ng,1030)'   AIC'//rgabb(1:nrgabb)//
     &                          ' : ARIMA model not '//Mdltxt(1:Nmdtxt)
        IF(Lsumm.gt.0)
     &     WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//': nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Saveas)THEN
       CALL mkealb(rgstr,nrgchr,Eastst,Easidx,Aicind,T)
       IF(Lfatal)RETURN
       IF(Lsumm.gt.0)
     &    WRITE(Nform,1010)'aictest.easter.reg: ',rgstr(1:nrgchr)
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
        IF(iaic.eq.0)
     &     iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
        IF(iaic.eq.0)
     &     iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)
     &      WRITE(Ng,1050)'AICeaster',rgstr(1:nrgchr),Aicind
         IF(Lsumm.gt.0)THEN
          WRITE(Nform,1010)'aictest.e: yes'
          WRITE(Nform,1020)'aictest.e.window: ',Aicind
         END IF
        ELSE
         IF(Lsvlog)WRITE(Ng,1030)'   AICeaster : rejected'
         IF(Lsumm.gt.0)THEN
          WRITE(Nform,1010)'aictest.e: no'
          WRITE(Nform,1020)'aictest.e.window: ',0
         END IF
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.e',Dfaice
         IF(Rgaicd(PEAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.e',Rgaicd(PEAIC)
        END IF
       ELSE
        IF(Lsvlog)
     &     WRITE(Ng,1030)'   AICeaster : ARIMA model not '//
     &                   Mdltxt(1:Nmdtxt)
        IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.e: nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savusr)THEN
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)WRITE(Ng,1030)'   AICuser : accepted'
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: yes'
        ELSE
         IF(Lsvlog)WRITE(Ng,1030)'   AICuser : rejected'
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.u',Dfaicu
         IF(Rgaicd(PUAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.e',Rgaicd(PUAIC)
        END IF
       ELSE
        IF(Lsvlog)
     &     WRITE(Ng,1030)'   AICuser : ARIMA model not '//
     &                   Mdltxt(1:Nmdtxt)
        IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(a:,a)
 1020 FORMAT(a,i6)
 1030 FORMAT(a,/)
 1040 FORMAT(a,': ',e20.10)
 1050 FORMAT(3x,a,' : ',a,'[',i2,']',/)
c-----------------------------------------------------------------------
      RETURN
      END

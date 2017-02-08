C     Last change:  BCM  19 Feb 1999   10:39 am
      SUBROUTINE easaic(Trnsrs,A,Nefobs,Na,Frstry,Lester,Lprtit,Lprt,
     &                  Lprtfm,Lsavlg,Lsumm,Lhiddn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Estimate a number of regARIMA model, each with either no easter
c     effect or an easter effect with length 1, 8, or 15.  This routine
c     chooses the model with the lowest value of AICC and prints out the
c     resulting model.
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,T=.true.,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
*      INCLUDE 'ssprep.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER eastr*(30),fmtsvl*(21)
      LOGICAL Lprt,Lprtit,Lester,argok,lhide,Lprtfm,Lsavlg,Lhiddn
      DOUBLE PRECISION A,aicbst,Trnsrs,aicno,aiceas,thiscv
      INTEGER Frstry,i,Na,Nefobs,begcol,ncol,easgrp,Lsumm,neachr,endlag,
     &        ilag
      DIMENSION A(PA),Trnsrs(PLEN)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL strinx,dpeq
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      IF(.not.Lprt)THEN
       lhide=Lhiddn
       Lhiddn=T
      END IF
      aiceas=DNOTST
c-----------------------------------------------------------------------
c     Set up format for saving AICC results to log file
c-----------------------------------------------------------------------
      IF(Lsavlg)THEN
       CALL mkealb(eastr,neachr,Eastst,Easidx,Easvec(Neasvc)+Easidx,F)
       CALL setchr(' ',21,fmtsvl)
       WRITE(fmtsvl,1011)MAX(neachr,4)+10
      END IF
c-----------------------------------------------------------------------
c     Start loop through model choices
c-----------------------------------------------------------------------
      DO i=1,Neasvc
c-----------------------------------------------------------------------
c     See if there is an easter effect in the model
c-----------------------------------------------------------------------
       easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
       IF(easgrp.eq.0)
     &    easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
       IF(easgrp.eq.0)
     &    easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
c-----------------------------------------------------------------------
c     If easter regressor in model, delete regressor from model
c-----------------------------------------------------------------------
       IF(easgrp.gt.0)THEN
        begcol=Grp(easgrp-1)
        ncol=Grp(easgrp)-begcol
        CALL dlrgef(begcol,Nrxy,ncol)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     If i > 1, add new easter regressor to model
c-----------------------------------------------------------------------
       IF(i.gt.1.or.easgrp.gt.0)THEN
        IF(i.gt.1)THEN
         CALL mkealb(eastr,neachr,Eastst,Easidx,Easvec(i)+Easidx,F)
         IF(.not.Lfatal)CALL addeas(Easvec(i)+Easidx,Easidx,Eastst)
        END IF
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                 Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c    If there are ARMA parameters that were set as initial values by
c    the user, reset Arimap to those values (BCM, 9-2010)
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Arimap(ilag)=Ap1(ilag)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Estimate model
c-----------------------------------------------------------------------
       argok=Lautom.or.Lautox
       CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &    CALL abend()
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
       IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &    Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &    Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &    ((Lautom.or.Lautox).and.(.not.argok)))THEN
        Lester=T
        RETURN
c-----------------------------------------------------------------------
c     If only a warning message would be printed out, reset the error
c     indicator variable to zero.
c-----------------------------------------------------------------------
       ELSE IF(Armaer.ne.0)THEN
        Armaer=0
       END IF
c-----------------------------------------------------------------------
c     Compute the likelihood statistics and AICC for the model
c-----------------------------------------------------------------------
       IF(i.eq.1)THEN
        IF(Lprt)WRITE(Mt1,1010)
       ELSE
        IF(Lprt)WRITE(Mt1,1020)eastr(1:neachr)
       END IF
       IF(i.eq.Neasvc)THEN
        CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,
     &              Lprtfm)
       ELSE
        CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,F)
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     See if this AICC is the smallest.  If so, update value and index
c     of best AICC.
c-----------------------------------------------------------------------
       IF(i.eq.1)THEN
        aicno=Aicc
        IF(Lsavlg)WRITE(Ng,fmtsvl)'AICC(no easter)',':',Aicc
        IF(Lsumm.gt.0)WRITE(Nform,1013)'noeaster',Aicc
       ELSE
        IF(Lsavlg)WRITE(Ng,fmtsvl)'AICC('//eastr(1:neachr)//')',':',Aicc
        IF(Lsumm.gt.0)WRITE(Nform,1014)'easter',Easvec(i),Aicc
        IF(i.eq.2.or.Aicc.lt.aiceas)THEN
         aiceas=Aicc
         Aicind=Easvec(i)
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      Dfaice=aicno-aiceas
      IF((.not.dpeq(Pvaic,DNOTST)).and.dpeq(Rgaicd(PEAIC),ZERO))THEN
       CALL chsppf(Pvaic,1,thiscv,Mt1)
       Rgaicd(PEAIC)=thiscv-2D0
      END IF
      IF(Dfaice.gt.Rgaicd(PEAIC))THEN
       aicbst=aiceas
      ELSE
       aicbst=Aicno
       Aicind=0
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lprt)Lhiddn=lhide
c-----------------------------------------------------------------------
c     Show Easter effect that aic prefers
c-----------------------------------------------------------------------
      IF(Lprt)THEN
       IF(Aicind.eq.0)THEN
        WRITE(Mt1,1030)Rgaicd(PEAIC)
        IF(Finhol)Finhol=F
       ELSE
        IF(Easidx.eq.0)THEN
         IF(Eastst.eq.1)THEN
          WRITE(Mt1,1040)Rgaicd(PEAIC),'Easter',Aicind
         ELSE
          WRITE(Mt1,1040)Rgaicd(PEAIC),'Stock Easter',Aicind
         END IF
        ELSE
         WRITE(Mt1,1040)Rgaicd(PEAIC),'Statistics Canada Easter',Aicind
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     If model with best AICC wasn't the last one estimated, redo model
c     estimation so the best model is returned.
c-----------------------------------------------------------------------
      IF(aicind.lt.Easvec(Neasvc))THEN
       easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
       IF(easgrp.eq.0)
     &    easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
       IF(easgrp.eq.0)
     &    easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
       begcol=Grp(easgrp-1)
       ncol=Grp(easgrp)-begcol
       CALL dlrgef(begcol,Nrxy,ncol)
c-----------------------------------------------------------------------
c     Add new Easter variable, if necessary
c-----------------------------------------------------------------------
       IF(.not.Lfatal.and.aicind.gt.0)
     &    CALL addeas(aicind+Easidx,Easidx,Eastst)
c-----------------------------------------------------------------------
c    If there are ARMA parameters that were set as initial values by
c    the user, reset Arimap to those values (BCM, 9-2010)
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Arimap(ilag)=Ap1(ilag)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Estimate model
c-----------------------------------------------------------------------
       IF(.not.Lfatal)
     &    CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(.not.Lfatal)
     &    CALL rgarma(T,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &    Lester=T
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(//,' Likelihood statistics for model without Easter')
 1011 FORMAT('(1x,a,t',i2,',a,1x,f15.4)')
 1013 FORMAT('aictest.e.aicc.',a,': ',e29.15)
 1014 FORMAT('aictest.e.aicc.',a,i2.2,': ',e29.15)
 1020 FORMAT(//,' Likelihood statistics for model with ',a)
 1030 FORMAT(//,'   *****   AICC (with aicdiff=',F7.4,
     &          ') prefers model without Easter   *****')
 1040 FORMAT(//,'   *****   AICC (with aicdiff=',F7.4,
     &          ') prefers model with ',a,'[',i2,']   *****')
c-----------------------------------------------------------------------
      RETURN
      END

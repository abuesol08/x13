C     Last change:  BCM  23 Mar 2005    9:23 am
      SUBROUTINE usraic(Trnsrs,A,Nefobs,Na,Frstry,Lester,Lprtit,Lprt,
     &                  Lprtfm,Lsavlg,Lsumm,Lhiddn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Estimate two regARIMA models, one with user-defined regressors and
c     one without.  This routine chooses the model with the lowest value
c     of AICC and prints out the resulting model.
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
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),ubkttl*(PCOLCR*PUREG)
      LOGICAL Lprt,Lprtit,Lester,argok,lhide,Lprtfm,Lsavlg,ubkfix,Lhiddn
      DOUBLE PRECISION A,aicnou,aicusr,Trnsrs,ubkx,ubkb,thiscv
      INTEGER Frstry,i,Na,Nefobs,nchr,iuser,nubk,ubktyp,ubkptr,begcol,
     &        endcol,igrp,Lsumm,endlag,ilag,nbu,nbno,aicdf
      DIMENSION A(PA),Trnsrs(PLEN),ubktyp(PUREG),ubkptr(0:PUREG),
     &          ubkx(PUSERX),ubkfix(PUREG),ubkb(PUREG)
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
c     Estimate model with user-defined regressors
c-----------------------------------------------------------------------
      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
      IF(Lfatal)RETURN
      argok=Lautom.or.Lautox
      CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
      IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &   CALL abend()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
      IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &   Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &   Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &   ((Lautom.or.Lautox).and..not.argok))THEN
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
      IF(Lprt)WRITE(Mt1,1010)
 1010 FORMAT(//,' Likelihood statistics for model with user-defined',
     &          ' regressors')
      CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,F)
      IF(Lfatal)RETURN
      aicusr=Aicc
      IF((.not.dpeq(Pvaic,DNOTST)).and.dpeq(Rgaicd(PUAIC),ZERO))nbu=Nb
      IF(Lsavlg)WRITE(Ng,1011)Aicc
 1011 FORMAT(' AICC(userreg)    : ',f15.4)
      IF(Lsumm.gt.0)WRITE(Nform,1012)'user',Aicc
c-----------------------------------------------------------------------
c     Make local backup copy of user defined regressors.
c-----------------------------------------------------------------------
      CALL copy(Userx,PUSERX,1,ubkx)
      CALL cpyint(Usrtyp,PUREG,1,ubktyp)
      CALL cpyint(Usrptr(0),PUREG+1,1,ubkptr(0))
      nubk=Ncusrx
      ubkttl=Usrttl
c-----------------------------------------------------------------------
c     remove the user defined regressors from the regression matrix.
c-----------------------------------------------------------------------
      igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
      DO WHILE (igrp.gt.0)
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       DO i=endcol,begcol,-1
        iuser=i-begcol+1
        ubkb(iuser)=B(i)
        ubkfix(iuser)=Regfx(i)
       END DO
       iuser=endcol-begcol+1
       CALL dlrgef(begcol,Nrxy,iuser)
       IF(Lfatal)RETURN
       igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
      END DO
      Ncusrx=0
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
c     Re-estimate model without user-defined regressors
c-----------------------------------------------------------------------
      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
      IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
      IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &   CALL abend()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
      IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &   Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &   Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &   ((Lautom.or.Lautox).and..not.argok))THEN
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
      IF(Lprt)WRITE(Mt1,1020)
 1020 FORMAT(//,' Likelihood statistics for model without user-defined',
     &          ' regressors')
      CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,Lprtfm)
      aicnou=Aicc
      IF((.not.dpeq(Pvaic,DNOTST)).and.dpeq(Rgaicd(PUAIC),ZERO))nbno=Nb
      IF(Lsavlg)WRITE(Ng,1021)Aicc
 1021 FORMAT(' AICC(no userreg) : ',f15.4)
      IF(Lsumm.gt.0)WRITE(Nform,1012)'nouser',Aicc
      IF(.not.Lprt)Lhiddn=lhide
c-----------------------------------------------------------------------
c     Show the regression model AICC prefers
c-----------------------------------------------------------------------
      Dfaicu=aicnou-aicusr
      IF((.not.dpeq(Pvaic,DNOTST)).and.dpeq(Rgaicd(PUAIC),ZERO))THEN
       aicdf=nbu-nbno
       CALL chsppf(Pvaic,aicdf,thiscv,Mt1)
       Rgaicd(PUAIC)=thiscv-2D0*dble(aicdf)
      END IF
      IF(Dfaicu.gt.Rgaicd(PUAIC))THEN
       IF(Lprt)WRITE(Mt1,1030)Rgaicd(PUAIC),'with'
c-----------------------------------------------------------------------
c     Add user-defined regressors back to model
c-----------------------------------------------------------------------
       CALL copy(ubkx,PUSERX,1,Userx)
       CALL cpyint(ubktyp,PUREG,1,Usrtyp)
       CALL cpyint(ubkptr(0),PUREG+1,1,Usrptr(0))
       Ncusrx=nubk
       Usrttl=ubkttl
c-----------------------------------------------------------------------
c     Restore user-defined regressors to the regression matrix
c-----------------------------------------------------------------------
       DO i=1,Ncusrx
        CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
        IF(.not.Lfatal)THEN
         IF(Usrtyp(i).eq.PRGTUH)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined Holiday',
     &                PRGTUH,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH2)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 2',PRGUH2,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH3)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 3',PRGUH3,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH4)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 4',PRGUH4,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH5)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 5',PRGUH5,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGTUS)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Seasonal',PRGTUS,ubkfix(i),F)
         ELSE
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined',PRGTUD,
     &                ubkfix(i),F)
         END IF
        END IF
       END DO
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
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,
     &                            argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))Lester=T
      ELSE
       IF(Lprt)WRITE(Mt1,1030)Rgaicd(PUAIC),'without'
      END IF
c-----------------------------------------------------------------------
 1012 FORMAT('aictest.u.aicc.',a,': ',e29.15)
 1030 FORMAT(//,'   *****   AICC (with aicdiff=',F7.4,
     &          ') prefers model ',a,' user-defined regressor   *****')
      RETURN
      END

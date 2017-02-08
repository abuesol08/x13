C     Last change:  BCM   7 May 1998    2:14 pm
      SUBROUTINE addusr(Rindex,Fxindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'urgbak.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.FALSE.)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),thisu*(PCOLCR)
      INTEGER begcol,disp,ncol,igrp,i,nchr,Rindex,icol,nusr,ucol,Fxindx
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     If there are user defined regressors left in the model, delete
c     them before adding all the user-defined regressors back in
c-----------------------------------------------------------------------
      IF(Ncusrx.gt.0)THEN
       igrp=Ngrp
       DO WHILE (igrp.ge.1)
        begcol=Grp(igrp-1)
        ncol=Grp(igrp)-begcol
        IF((Rgvrtp(begcol).ge.PRGTUH.and.Rgvrtp(begcol).le.PRGUH5).or.
     &      Rgvrtp(begcol).eq.PRGTUS.or.Rgvrtp(begcol).eq.PRGTUD)THEN
         DO icol=begcol,begcol+ncol-1
          CALL getstr(Colttl,Colptr,Nb,icol,thisu,nusr)
          IF(Lfatal)RETURN
          ucol=strinx(F,Usrtt2(Rindex),Usrpt2,1,Ncusx2(Rindex),
     &                thisu(1:nusr))
          Buser(ucol)=B(icol)
         END DO
         CALL dlrgef(begcol,Nrxy,ncol)
         IF(Lfatal)RETURN
         igrp=0
        ELSE
         igrp=igrp-1
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
c     Restore values of the user defined regression variables
c-----------------------------------------------------------------------
      disp=PUSERX*Rindex+1
      CALL copy(Userx2(disp),PUSERX,1,Userx)
      disp=(PUREG+1)*Rindex+1
      CALL cpyint(Usrpt2(disp),PUREG+1,1,Usrptr(0))
      disp=PUREG*Rindex+1
      CALL cpyint(Usrty2(disp),PUREG,1,Usrtyp)
      Ncusrx=Ncusx2(Rindex)
      Usrttl=Usrtt2(Rindex)
c-----------------------------------------------------------------------
c     Restore user-defined regressors to the regression matrix
c-----------------------------------------------------------------------
      disp=PUREG*Rindex
      DO i=1,Ncusrx
       IF(.not.(Fxuser(disp+i).and.Fxindx.eq.2))THEN
        CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
        IF(Lfatal)RETURN
        IF(Usrtyp(i).eq.PRGTUS)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Seasonal',Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGTUH)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday',Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH2)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 2',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH3)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 3',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH4)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 4',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH5)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 5',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined',
     &               PRGTUD,Fxuser(disp+i),F)
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

      SUBROUTINE bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,
     &                  Grpttl,Grp,Grpptr,Ngrptl,Rindex,Is1st)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Making backup copy of user defined regressors for regARIMA, m.
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'urgbak.cmn'
c-----------------------------------------------------------------------
      LOGICAL Regfx,Is1st
      CHARACTER Grpttl*(PGRPCR*PGRP),Usrttl*(PCOLCR*PUREG)
      DOUBLE PRECISION B,Userx
      INTEGER Usrtyp,Ncusrx,Usrptr,Grpptr,Ngrptl,Grp,disp,i,iuser,igrp,
     &        begcol,endcol,Rindex
      DIMENSION B(PB),Regfx(PB),Userx(PUSERX),Usrtyp(PUREG),Grp(0:PGRP),
     &          Grpptr(0:PGRP),Usrptr(0:PUREG)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     remove the user defined regressors from the regression matrix.
c-----------------------------------------------------------------------
      igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
      begcol=Grp(igrp-1)
      endcol=Grp(igrp)-1
      DO i=begcol,endcol
       iuser=i-begcol+1+(PUREG*Rindex)
       Buser(iuser)=B(i)
       Fxuser(iuser)=Regfx(i)
      END DO
c-----------------------------------------------------------------------
c     Make backup copy of user defined regressors.
c-----------------------------------------------------------------------
      IF(.not.Is1st)RETURN
      disp=(PUSERX*Rindex)+1
      CALL copy(Userx(disp),PUSERX,1,Userx2)
      disp=(PUREG*Rindex)+1
      CALL cpyint(Usrtyp(disp),PUREG,1,Usrty2)
      disp=((PUREG+1)*Rindex)+1
      CALL cpyint(Usrptr(0),PUREG+1,1,Usrpt2(disp))
      Ncusx2(Rindex)=Ncusrx
      Usrtt2(Rindex)=Usrttl
c-----------------------------------------------------------------------
      RETURN
      END

      SUBROUTINE sftest(Xpxinv,Regidx,Lprsft,Lsvsft,Lsvlog,Lxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     generate model-based f-tests for seasonality from chi square
c     statistics of seasonal regressors; also generate model-based
c     f-tests for combinations of seasonal regression groups,
c     such as change of regime regressors and user defined seasonal
c     regressors
c     (BCM July 2007)
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdldg.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER grpstr*(PGRPCR)
      DOUBLE PRECISION Xpxinv,chi2vl,pv
      LOGICAL Lxreg,Lprsft,Lsvsft,Lsvlog,lprthd,lprund
      INTEGER baselt,begcol,endcol,igrp,gsearg,gusea,Regidx,rgi2,rtype,
     &        k,df,df1,df2,iusr,utype,info,tbwdth,nchr,icol,ud1st,i,
     &        udlast
      DIMENSION gsearg(0:2),gusea(0:2),Regidx(PB),rgi2(PB),Xpxinv(PXPX)
c-----------------------------------------------------------------------
      DOUBLE PRECISION fvalue
      EXTERNAL fvalue
c-----------------------------------------------------------------------
      lprthd=T
      lprund=F
      tbwdth=71
c-----------------------------------------------------------------------
c     Compute number of regressors estimated
c-----------------------------------------------------------------------
      k=Nb
      IF(Iregfx.eq.2)THEN
       DO igrp=1,Ngrp
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        DO icol=begcol,endcol
         IF(regidx(icol).eq.NOTSET)k=k-1
        END DO
       END DO
      END IF
c-----------------------------------------------------------------------
c   Print out f-tests for individual groups of seasonal regressors
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       rtype=Rgvrtp(begcol)
       IF(rtype.eq.PRRTTS.or.rtype.eq.PRRTSE.or.rtype.eq.PRATTS.or.
     &    rtype.eq.PRATSE.or.rtype.eq.PRGTTS.or.rtype.eq.PRGTSE)THEN
        IF(Lprsft)lprund=T
        endcol=Grp(igrp)-1
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,grpstr,nchr)
        IF(Lfatal)RETURN
        info=0
        baselt=regidx(begcol)
        df=endcol-begcol+1
        IF(Iregfx.eq.2)THEN
         IF(baselt.eq.NOTSET)df=df-1
         DO icol=begcol+1,endcol
          IF(regidx(icol).eq.NOTSET)THEN
           df=df-1
          ELSE
           baselt=regidx(icol)
          END IF
         END DO
        END IF
        IF(baselt.ne.NOTSET)
     &     CALL chitst(Xpxinv,begcol,endcol,chi2vl,pv,regidx,T,info)
        CALL eltlen(igrp,Grp,Ngrp,df1)
        IF(Lfatal)RETURN
        df2=Nspobs-Mxdflg-k
        Sfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
        Sfpv=fvalue(Sfval,df1,df2)
        CALL prtft(Lprsft,lprthd,tbwdth,Lsvsft,Lsvlog,baselt,grpstr,
     &             nchr,'Seasonal',info,df1,df2,Sfval,Sfpv)
       END IF
      END DO
c-----------------------------------------------------------------------
c     Create pointer dictionaries for different tests we wish to
c     perform
c-----------------------------------------------------------------------
      DO icol=0,2
       gsearg(icol)=0
       gusea(icol)=0
      END DO
      ud1st=NOTSET
      udlast=NOTSET
      iusr=1
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       rtype=Rgvrtp(begcol)
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTUS)THEN
        IF(ud1st.eq.NOTSET)ud1st=begcol
        DO icol=begcol,endcol
         IF(Lxreg)THEN
          utype=Rgvrtp(icol)
         ELSE
          utype=Rgvrtp(icol)
         END IF
         IF(gusea(0).eq.0)THEN
          gusea(0)=gusea(0)+1
          gusea(1)=icol
         ELSE
          IF(.not.(udlast.eq.PRGTUS))gusea(0)=gusea(0)+1
         END IF
         gusea(2)=icol
         iusr=iusr+1
         IF(icol.lt.endcol)udlast=utype
        END DO
c-----------------------------------------------------------------------
       ELSE IF(rtype.eq.PRRTTS.or.rtype.eq.PRRTSE.or.
     &         rtype.eq.PRATTS.or.rtype.eq.PRATSE.or.
     &         rtype.eq.PRGTTS.or.rtype.eq.PRGTSE)THEN
        gsearg(0)=gsearg(0)+1
        IF(gsearg(0).eq.1)gsearg(1)=begcol
        gsearg(2)=endcol
       END IF
      END DO
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for seasonal regressors
c-----------------------------------------------------------------------
      IF(gsearg(0).ge.2)THEN
       CALL setint(NOTSET,Nb,rgi2)
       df=gsearg(2)-gsearg(1)+1
       baselt=regidx(gsearg(1))
       info=0
       DO icol=gsearg(1),gsearg(2)
        rtype=Rgvrtp(icol)
        IF(rtype.eq.PRRTTS.or.rtype.eq.PRRTSE.or.rtype.eq.PRATTS.or.
     &     rtype.eq.PRATSE.or.rtype.eq.PRGTTS.or.rtype.eq.PRGTSE)THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
       END DO
       CALL chitst(Xpxinv,gsearg(1),gsearg(2),chi2vl,pv,rgi2,F,info)
       df1=df
       df2=Nspobs-Mxdflg-df1
       Sfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Sfpv=fvalue(Sfval,df1,df2)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=9
       grpstr(1:nchr)='Combined '
       rtype=Rgvrtp(gsearg(1))
       IF(rtype.eq.PRRTTS.or.rtype.eq.PRATTS)THEN
        grpstr(nchr+1:nchr+14)='Trigonometric '
        nchr=nchr+14
       END IF
       grpstr(nchr+1:nchr+19)='Seasonal Regressors'
       nchr=nchr+19
       CALL prtft(Lprsft,lprthd,tbwdth,Lsvsft,Lsvlog,baselt,grpstr,
     &            nchr,'Seasonal',info,df1,df2,Sfval,Sfpv)
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined seasonal
c     regressors if there are more than one type of user defined
c     regressor defined.
c-----------------------------------------------------------------------
      IF(gusea(0).gt.0.and.(gusea(2)-gusea(1)).gt.0)THEN
       CALL setint(NOTSET,Nb,rgi2)
       df=gusea(2)-gusea(1)+1
       baselt=regidx(gusea(1))
       DO icol=gusea(1),gusea(2)
        iusr=icol-ud1st+1
        IF(Lxreg)THEN
         utype=Usxtyp(iusr)
        ELSE
         utype=Usrtyp(iusr)
        END IF
        IF(utype.eq.PRGTUS)THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
       END DO
       CALL chitst(Xpxinv,gusea(1),gusea(2),chi2vl,pv,rgi2,
     &             gusea(0).lt.2,info)
       df1=df
       df2=Nspobs-Mxdflg-df1
       Usfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Usfpv=fvalue(Usfval,df1,df2)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=32
       grpstr(1:nchr)='User-defined Seasonal Regressors'
       CALL prtft(Lprsft,lprthd,tbwdth,Lsvsft,Lsvlog,baselt,grpstr,
     &            nchr,'Seasonal',info,df1,df2,Usfval,Usfpv)
      END IF
c-----------------------------------------------------------------------
c     Print the tail line
c-----------------------------------------------------------------------
      IF(lprund)WRITE(Mt1,1020)('-',i=1,tbwdth)
      IF(Lsvlog)WRITE(Ng,1020)' '
c-----------------------------------------------------------------------
 1020 FORMAT(' ',120(a))
      RETURN
      END
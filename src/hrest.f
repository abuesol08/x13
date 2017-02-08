C     Last change:  BCM   5 Mar 1999    1:36 pm
      SUBROUTINE hrest(Iar,X,R,Hrp,Ipr,Ips,Iqr,Iqs,Iq,Iprs,Sp,Ndfobs,
     &                 Nefobs,Lprt,Info)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Hannen/Riesen estimation of ARMA models is performed as in
c       TRAMO/SEATS program by Gomez/Maravall.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      INTEGER PR
      PARAMETER(PR=PLEN/4,ONE=1D0,ZERO=0D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION X,R,Hrp,a1,a2,ahat,phat,pcf,psum,psum1,psum2,pv,
     &                 tmp,xmat,tmpchl,ptmp
      LOGICAL Lprt
      INTEGER i,j,jj,j2,k,kk,Iar,Ipr,Ips,Iqr,Iqs,Iq,Iprs,
     &        Nefobs,Sp,ncol,mxlg,Info,nc1,Ndfobs,i2
      DIMENSION X(PLEN),R(PR),Hrp(PARIMA),ahat(PLEN),phat(PR),pcf(PR),
     &          tmp(PLEN),xmat((PR+1)*PLEN),tmpchl(PXPX),ptmp(PARIMA),
     &          a1(PLEN),a2(PLEN)
c     ------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c     ------------------------------------------------------------------
      CALL setdp(ZERO,PXPX,tmpchl)
c     ------------------------------------------------------------------
c       If Ma terms in model, compute estimates of innovations
c     ------------------------------------------------------------------
      IF (Iq.gt.0) THEN
c     ------------------------------------------------------------------
c       Compute partial correlation coefficients
c     ------------------------------------------------------------------
c       pcf(1)=R(1)/C0
       pcf(1)=R(1)
       pv=C0*(ONE-pcf(1)*pcf(1))
       phat(1)=pcf(1)
       DO i=2,Iar
        psum=ZERO
        DO j=1,i-1
         psum=psum+phat(j)*(R(i-j)*C0)
         tmp(j)=phat(i-j)
        END DO
        pcf(i)=((R(i)*C0)-psum)/pv
        pv=pv*(ONE-pcf(i)*pcf(i))
        phat(i)=pcf(i)
        DO j=1,i-1
         phat(j)=phat(j)-pcf(i)*tmp(j)
        END DO
       END DO
c     ------------------------------------------------------------------
c       Create estimates of innovations ahat
c     ------------------------------------------------------------------
       DO i=1,Iar
        ahat(i)=X(i)
        DO j=1,Iar
         IF((i-j).GT.0)ahat(i)=ahat(i)-phat(j)*X(i-j)
        END DO
       END DO
       DO i=Iar+1,Ndfobs
        ahat(i)=X(i)
        DO j=1,Iar
         ahat(i)=ahat(i)-phat(j)*X(i-j)
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
      ncol=Ipr+Ips*(Ipr+1)+Iqr+Iqs*(Iqr+1)
      nc1=ncol+1
      mxlg=MAX0(Ipr+Sp*Ips,Iqr+Sp*Iqs)
      DO i=1+mxlg,Ndfobs
       i2=(i-mxlg-1)*nc1
       DO j=1,Ipr
        xmat(j+i2)=-X(i-j)
       END DO
       DO j=1,Ips
        jj=(Ipr+1)*j
        xmat(jj+i2)=-X(i-j*Sp)
        DO k=1,Ipr
         xmat(jj+k+i2)=-X(i-j*Sp-k)
        END DO
       END DO
       kk=Ipr+(Ipr+1)*Ips
       jj=0
       DO j=1,Iqr
        xmat(kk+j+i2)=ahat(i-j)
       END DO
       DO j=1,Iqs
        jj=kk+(Iqr+1)*j
        xmat(jj+i2)=ahat(i-j*Sp)
        DO k=1,Iqr
         xmat(jj+k+i2)=ahat(i-j*Sp-k)
        END DO
       END DO
       xmat(nc1+i2)=X(i)
      END DO
c     ------------------------------------------------------------------
      Nefobs=Ndfobs-mxlg
      CALL olsreg(xmat,Nefobs,nc1,nc1,ptmp,tmpchl,PXPX,Info)
      IF(Lfatal)RETURN
      IF(Info.gt.0)THEN
       Info=PSNGER
       RETURN
      END IF
c     ------------------------------------------------------------------
      DO i=1,Ipr
       Hrp(i)=ptmp(i)
      END DO
      DO i=1,Ips
       Hrp(Ipr+i)=ptmp((Ipr+1)*i)
      END DO
      DO i=1,Iqr
       Hrp(Iprs+i)=ptmp(Ipr+Ips*(Ipr+1)+i)
      END DO
      DO i=1,Iqs
       Hrp(Iprs+Iqr+i)=ptmp(Ipr+Ips*(Ipr+1)+(Iqr+1)*i)
      END DO
c     ------------------------------------------------------------------
      IF (Iq.GT.0) THEN
       DO i=1,Ndfobs
        tmp(i)=ZERO
       END DO
       DO i=1,Ndfobs
        psum=X(i)
        psum1=ZERO
        psum2=ZERO
        j2=(i-1)*nc1
        DO j=1,Ipr
         IF ((i-j).GT.0) THEN
          psum=psum+ptmp(j)*X(i-j)
          psum1=psum1-ptmp(j)*A1(i-j)
          xmat(j+j2)=-A1(i-j)
         ELSE
          xmat(j+j2)=ZERO
         END IF
        END DO
        DO j=1,Ips
         jj=(Ipr+1)*j
         IF ((i-j*Sp).GT.0) THEN
          psum=psum+ptmp(jj)*X(i-j*Sp)
          psum1=psum1-ptmp(jj)*A1(i-j*Sp)
          xmat(jj+j2)=-A1(i-j*Sp)
         ELSE
          xmat(jj+j2)=ZERO
         END IF
         DO k=1,Ipr
          IF ((i-j*Sp-k).GT.0) THEN
           psum=psum+ptmp(jj+k)*x(i-j*Sp-k)
           psum1=psum1-ptmp(jj+k)*A1(i-j*Sp-k)
           xmat(j2+jj+k)=-A1(i-j*Sp-k)
          ELSE
           xmat(j2+jj+k)=ZERO
          END IF
         END DO
        END DO
        kk=Ipr+(Ipr+1)*Ips
        DO j=1,Iqr
         IF ((i-j).GT.0) THEN
          psum=psum-ptmp(kk+j)*tmp(i-j)
          psum2=psum2-ptmp(kk+j)*A2(i-j)
          xmat(kk+j+j2)=A2(i-j)
         ELSE
          xmat(kk+j+j2)=ZERO
         END IF
        END DO
        DO j=1,Iqs
         jj=kk+(Iqr+1)*j
         IF ((i-j*Sp).GT.0) THEN
          psum=psum-ptmp(jj)*tmp(i-j*Sp)
          psum2=psum2-ptmp(jj)*A2(i-j*Sp)
          xmat(jj+j2)=A2(i-j*Sp)
         ELSE
          xmat(jj+j2)=ZERO
         END IF
         DO k=1,Iqr
          IF ((i-j*Sp-k).GT.0) THEN
           psum=psum-ptmp(jj+k)*tmp(i-j*Sp-k)
           psum2=psum2-ptmp(jj+k)*A2(i-j*Sp-k)
           xmat(jj+k+j2)=A2(i-j*Sp-k)
          ELSE
           xmat(jj+k+j2)=ZERO
          END IF
         END DO
        END DO
        tmp(i)=psum
        xmat(i*nc1)=psum
        A1(i)=psum1+tmp(i)
        A2(i)=psum2+tmp(i)
c     ------------------------------------------------------------------
        IF (DABS(psum).GT.1.D+10) THEN
         IF (Lprt) THEN
          WRITE(Mt1,'(/A/)')
     &    ' Some initial estimates cannot be obtained for HR estimation'
          WRITE(Mt1,'(A/)')
     &    '                    Default values used                     '
         END IF
         Info=-1
         RETURN
        END IF
       END DO
c     ------------------------------------------------------------------
       CALL olsreg(xmat,Ndfobs,nc1,nc1,ptmp,tmpchl,PXPX,Info)
       IF(Lfatal)RETURN
       IF(Info.gt.0)THEN
        Info=PSNGER
        RETURN
       END IF
       DO i=1,Ipr
        Hrp(i)=Hrp(i)+ptmp(i)
       END DO
       DO i=1,Ips
        Hrp(Ipr+i)=Hrp(Ipr+i)+ptmp((Ipr+1)*i)
       END DO
       DO i=1,Iqr
        Hrp(Iprs+i)=Hrp(Iprs+i)+ptmp(Ipr+Ips*(Ipr+1)+i)
       END DO
       DO i=1,Iqs
        Hrp(Iprs+Iqr+i)=Hrp(Iprs+Iqr+i)+ptmp(Ipr+Ips*(Ipr+1)+(Iqr+1)*i)
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

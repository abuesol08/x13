C     Last change:  BCM   3 Oct 97    7:12 am
**==ddot.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION ddot(N,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
C
C     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY.
C     DDOT = SUM FOR I = 0 TO N-1 OF  DX(LX+I*INCX) * DY(LY+I*INCY)
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER i,Incx,Incy,ix,iy,m,mp1,N,ns
      DOUBLE PRECISION Dx(*),Dy(*)
c      LOGICAL lflag
c      CALL UNDFL(lflag)
      ddot=0.D0
      IF(N.le.0)RETURN
      IF(Incx.eq.Incy)THEN
       IF(Incx.lt.1)THEN
       ELSE IF(Incx.eq.1)THEN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
        m=mod(N,5)
        IF(m.ne.0)THEN
         DO i=1,m
          ddot=ddot+Dx(i)*Dy(i)
         END DO
         IF(N.lt.5)RETURN
        END IF
        mp1=m+1
        DO i=mp1,N,5
         ddot=ddot+Dx(i)*Dy(i)+Dx(i+1)*Dy(i+1)+Dx(i+2)*Dy(i+2)+Dx(i+3)
     &        *Dy(i+3)+Dx(i+4)*Dy(i+4)
        END DO
        RETURN
       ELSE
        GO TO 10
       END IF
      END IF
C
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
      ix=1
      iy=1
      IF(Incx.lt.0)ix=(-N+1)*Incx+1
      IF(Incy.lt.0)iy=(-N+1)*Incy+1
      DO i=1,N
       ddot=ddot+Dx(ix)*Dy(iy)
       ix=ix+Incx
       iy=iy+Incy
      END DO
c      CALL UNDFL(lflag)
      RETURN
C
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C
   10 ns=N*Incx
      DO i=1,ns,Incx
       ddot=ddot+Dx(i)*Dy(i)
      END DO
      RETURN
      END

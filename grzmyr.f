C     Last change:  BCM   4 Sep 1998    3:34 pm
**==grzmyr.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE grzmyr(L)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
c-----------------------------------------------------------------------
      INTEGER i,iyr,L
c      DOUBLE PRECISION Ab2(61),Ser2(61,12)
c-----------------------------------------------------------------------
c      COMMON /grzg2 / Ab1,Ab2
c      COMMON /grzmon/ Ser1,Ser2
      i=0
      DO iyr=1,61
       IF(Ser1(iyr,L).le.999999998.D0)THEN
        i=i+1
        Ab1(i)=Ser1(iyr,L)
c        Ab2(i)=Ser2(iyr,L)
       END IF
      END DO
      RETURN
      END

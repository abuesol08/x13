      SUBROUTINE gettim(Hour,Minute,Second,Hundth)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Return Hour, Minute, Seconds, and Hundreth of a Second in current
c     time
c-----------------------------------------------------------------------
      CHARACTER ctime*11
      INTEGER Hour,Minute,Second,Hundth
c-----------------------------------------------------------------------
      CALL TIME(ctime)
      READ(ctime,1000)Hour,Minute,Second,Hundth
 1000 FORMAT(3(i2,1x),i2)
c-----------------------------------------------------------------------
      RETURN
      END

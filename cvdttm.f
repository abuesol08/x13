      CHARACTER*24 FUNCTION cvdttm(datstr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function to convert Lahey fortran (PC) date and time convention
c     to a common format for date and time information
c-----------------------------------------------------------------------
cdos
cdos      INTEGER PCUT2K
cdos      PARAMETER(PCUT2K=45)
c-----------------------------------------------------------------------
      CHARACTER datstr*24,cmonth*3
      INTEGER d,y,h,minute,s
cdos
cdos      INTEGER mon
cdos      DIMENSION cmonth(12)
c-----------------------------------------------------------------------
cdos
cdos      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
cdos     &            'Oct','Nov','Dec'/
c-----------------------------------------------------------------------
cdos
cdos      READ(datstr,1010)y,mon,d,h,minute,s
cdos 1010 FORMAT(2x,i4,2i2,2x,3i2)
cdos      IF(y.lt.2000)THEN
cdos       IF(y.le.PCUT2K)THEN
cdos        y=2000+y
cdos       ELSE
cdos        y=1900+y
cdos       END IF
cdos      END IF
cdos      WRITE(cvdttm,1020)cmonth(mon),d,y,h,minute,s
cunix
      READ(datstr,1010)cmonth,d,h,minute,s,y
 1010 FORMAT(4x,a3,4(1x,i2),1x,i4)
      WRITE(cvdttm,1020)cmonth,d,y,h,minute,s
 1020 FORMAT(1x,a3,1x,i2,', ',i4,2x,2(i2.2,'.'),i2.2,1x)
      RETURN
      END
c Jan 30, 2003  11.56.55

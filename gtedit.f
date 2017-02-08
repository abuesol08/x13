C     Last change:  BCM  14 May 1998    7:54 am
      SUBROUTINE gtedit(Plen,File,Y,Start,Chnl,Nobs,Ncol,Freq,Srsnam,
     &                  Lcomma,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read the Edit data file format
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'x11msc.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      INTEGER PCUT2K,YR,MO,DATLEN
      PARAMETER(F=.false.,YR=1,MO=2,PCUT2K=45,DATLEN=1000)
c-----------------------------------------------------------------------
      CHARACTER File*(*),Srsnam*(*),chrstr*(DATLEN)
      DOUBLE PRECISION Y
      LOGICAL Argok,Lcomma
      INTEGER Freq,i,i1,i2,itmp,itmp1,itmp2,Plen,Start,Chnl,Nobs,Ncol,
     &        nyy,npr,ncomma
      DIMENSION Y(Plen),Start(2)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      i=1
      i1=i+Ncol-1
      DO WHILE (i1.le.Plen)
c-----------------------------------------------------------------------
c     Read the month and year as integers, and the observations as
c     characters
c-----------------------------------------------------------------------
       IF(Lcomma)THEN
        READ(Chnl,1000,END=20,ERR=10)chrstr
 1000   FORMAT(a)
c-----------------------------------------------------------------------
c     For each column, convert the commas to periods, and internally
c     read the observations into the data vector.
c-----------------------------------------------------------------------
        CALL cvcmma(chrstr,ncomma)
        READ(chrstr,*,ERR=10)itmp1,itmp2,(Y(i2),i2=i,i1)
c-----------------------------------------------------------------------
c     Else, read the month, year, observation from file
c-----------------------------------------------------------------------
       ELSE
        READ(Chnl,*,END=20,ERR=10)itmp1,itmp2,(Y(i2),i2=i,i1)
       END IF
       IF(itmp1.lt.100)THEN
        IF(Yr2000.and.(yr.le.PCUT2K))THEN
         itmp1=itmp1+2000
        ELSE
         itmp1=itmp1+1900
        END IF
       END IF
c-----------------------------------------------------------------------
c     If this is the first observation, set the starting date.
c-----------------------------------------------------------------------
       IF(i.eq.1)THEN
        Start(YR)=itmp1
        Start(MO)=itmp2
        itmp=itmp1*Freq+itmp2
       ELSE
        itmp=itmp+1
        nyy=itmp/Freq
        npr=mod(itmp,Freq)
        IF(npr.eq.0)THEN
         nyy=nyy-1
         npr=Freq
        END IF
        IF(.not.((itmp1.eq.nyy).and.(itmp2.eq.npr)))THEN
         WRITE(STDERR,1120)nyy,npr,Srsnam,itmp1,itmp2
         WRITE(Mt2,1120)nyy,npr,Srsnam,itmp1,itmp2
 1120    FORMAT(' ERROR: Expected to find observation ',i4,':',i2,
     &          ' of series ',a,/,
     &          '        not ',i4,':',i2,'.  Check input file and ',
     &          'format.',/)
          Argok=F
          Nobs=0
          RETURN
         END IF
       END IF
c-----------------------------------------------------------------------
       i=i+Ncol
       i1=i+Ncol-1
      END DO
c-----------------------------------------------------------------------
      IF(i1.gt.Plen)THEN
       WRITE(STDERR,1010)File
       WRITE(Mt2,1010)File
 1010  FORMAT(/,' ERROR: Problem reading ',a,'.'/,
     &          '        Too many observations in file.',/)
       Argok=F
       Nobs=0
      END IF
c-----------------------------------------------------------------------
   10 WRITE(STDERR,1020)File
      WRITE(Mt2,1020)File
 1020 FORMAT(/,' ERROR:  Problem reading ',a,'.'/,
     &         '         Check your input file and format.',/)
      Argok=F
      Nobs=0
c-----------------------------------------------------------------------
   20 RETURN
      END
      

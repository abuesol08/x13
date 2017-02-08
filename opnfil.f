C     Last change:  BCM   1 Oct 1998   10:56 am
      SUBROUTINE opnfil(Lopen,Lgrfdr,Itbl,Fh,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     opnfil.f, Release 1, Subroutine Version 1.6, Modified 03 Nov 1994.
c-----------------------------------------------------------------------
c     Opens a file with the name of the spec file with the extension
c replaced with the table extension.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER fil*(PFILCR),fildes*(PTTLEN),gcode*(15)
      LOGICAL Lfrtop,Locok,Lopen,Lgrfdr
      INTEGER Fh,Itbl,nchr,ndescr,ncode
c     ------------------------------------------------------------------
c     Added by BCM 12/29/94 - common block to control printing of
c     file table.
c     ------------------------------------------------------------------
      LOGICAL Lexist,Lexout,Lexerr
      INTEGER Fhandl
      COMMON /filetb/ Fhandl,Lexist,Lexout,Lexerr,Lfrtop
c-----------------------------------------------------------------------
c     Generated by running strary and cutting and pasting the output.
c-----------------------------------------------------------------------
      INCLUDE 'tbllog.prm'
      INCLUDE 'filext.prm'
      INCLUDE 'gmeta.prm'
c     ------------------------------------------------------------------
      INCLUDE 'filext.var'
      INCLUDE 'gmeta.var'
c     ------------------------------------------------------------------
      IF(Lgrfdr)THEN
       nchr=Ngrfcr+4
       fil(1:nchr)=Curgrf(1:Ngrfcr)//'.'//tbxdic(Itbl)
       CALL getstr(GMTDIC,gmtptr,PGMT,Itbl,gcode,ncode)
       WRITE(Grfout,1000)gcode(1:ncode),fil(1:nchr)
 1000  FORMAT(a,t12,a)
      ELSE
       nchr=Nfilcr+4
       fil(1:nchr)=Cursrs(1:Nfilcr)//'.'//tbxdic(Itbl)
      END IF
C     ------------------------------------------------------------------
c     Get the file description from one of the data dictionaries
C     ------------------------------------------------------------------
      CALL getdes(Itbl,fildes,ndescr,F)
      IF(Lfatal)RETURN
C     ------------------------------------------------------------------
      IF(Lopen)THEN
       Fh=NOTSET
       CALL fopen(fil(1:nchr),fildes(1:ndescr),'UNKNOWN',Fh,Locok)
c     ------------------------------------------------------------------
      ELSE
       INQUIRE(FILE=fil(1:nchr),EXIST=lexist)
       IF(Fhandl.eq.0)RETURN
       IF(Lfrtop)THEN
        WRITE(Fhandl,1010)
 1010   FORMAT(/,
     &' FILE SAVE REQUESTS (* indicates file exists and will be overwrit
     &ten)')
        Lfrtop=F
       END IF
c     ------------------------------------------------------------------
       IF(lexist)THEN
        WRITE(Fhandl,1020)fil(1:nchr),'*',fildes(1:ndescr)
 1020   FORMAT('  ',a,a,' ',a)
        Locok=F
c     ------------------------------------------------------------------
       ELSE
        WRITE(Fhandl,1020)fil(1:nchr),' ',fildes(1:ndescr)
        Locok=T
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

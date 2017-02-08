C     Last change:  BCM  23 Jul 1998    3:36 pm
**==dtoc.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dtoc(Dnum,Str,Ipos)
c     -----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'stdio.i'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'units.cmn'
c     -----------------------------------------------------------------
      CHARACTER Str*(*),temp*22
      INTEGER Ipos,nleft
      DOUBLE PRECISION Dnum
c     -----------------------------------------------------------------
      nleft=len(Str(Ipos:))
c     -----------------------------------------------------------------
      IF(Svsize.gt.nleft)THEN
       WRITE(temp,Svfmt)Dnum
       WRITE(STDERR,1010)Dnum,nleft
       CALL errhdr
       WRITE(Mt2,1010)temp(1:Svsize),nleft
 1010  FORMAT(/,' ERROR: Cannot write ',a,' in ',i3,' spaces.',/)
       CALL abend
       RETURN
c     -----------------------------------------------------------------
      ELSE
       WRITE(Str(Ipos:),Svfmt)Dnum
       Ipos=Ipos+Svsize
      END IF
c     -----------------------------------------------------------------
      RETURN
      END


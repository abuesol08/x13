c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c force      FRC or FC
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c change in adjusted seasonal adjusted              E6A 
c change in rounded seasonal adjusted               E6R 
c seasonal adjusted, yr totals adj                  SAA
c rounded seasonal adjusted                         RND
c-----------------------------------------------------------------------
      INTEGER LFCSAA,LFCRND,LFCE6A,LFC6AP,LFCE6R,LFC6RP,LFRCCR,LFRCRR,
     &        LFRFAC
      PARAMETER(
     &          LFCSAA=204,LFCRND=205,LFCE6A=206,LFC6AP=207,LFCE6R=208,
     &          LFC6RP=209,LFRCCR=210,LFRCRR=211,LFRFAC=212)

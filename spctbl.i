c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c spectrum      SPC, SP
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c spectrum of original series       S0
c spectrum of residuals             RS
c spectrum, diff seas. adj. srs     S1
c spectrum, modified irregular      S2
c spectrum of sa series             S1S
c spectrum of mod irregular         S2S
c spectrum of extended residuals    ERS
c spectrum of ind sa series         S1I
c spectrum of ind mod irregular     S2I
c spectrum of composite series      S0C
c-----------------------------------------------------------------------
      INTEGER LSPCS0,LSPCRS,LSPCS1,LSPCS2,LSPS1S,LSPS2S,LSPERS,LSPS1I,
     &        LSPS2I,LSPS0C,LSPTS0,LSPTRS,LSPTS1,LSPTS2,LSPT1S,LSPT2S,
     &        LSPTER,LSPT1I,LSPT2I,LSPT0C,LSPCQS,LSPCTP
      PARAMETER(
     &          LSPCS0=92,LSPCRS=93,LSPCS1=94,LSPCS2=95,LSPS1S=96,
     &          LSPS2S=97,LSPERS=98,LSPS1I=99,LSPS2I=100,LSPS0C=101,
     &          LSPTS0=102,LSPTRS=103,LSPTS1=104,LSPTS2=105,LSPT1S=106,
     &          LSPT2S=107,LSPTER=108,LSPT1I=109,LSPT2I=110,LSPT0C=111,
     &          LSPCQS=112,LSPCTP=113)

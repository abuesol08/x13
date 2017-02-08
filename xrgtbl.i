c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c x11regress     XRG or XR
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c extreme values for X-11 regression                IRX
c X-11 regression model                             XRG
c trading day factors                               TDF
c holiday factors                                   HLF
c combined calendar factors                         CLF
c automatic outlier header for X-11 regression      OHD
c automatic outlier iterations for X-11 regression  OIT
c automatic outlier tests for X-11 regression       OTT
c final X-11 regression matrix                      XMX
c-----------------------------------------------------------------------
      INTEGER LXRGA4,LXRIRX,LXRXRG,LXRTDF,LXRTDC,LXRHLF,LXRCLF,LXRCLC,
     &        LXROHD,LXROIT,LXROTT,LXROFT,LXRXMX,LXRXCM,LXAICT
      PARAMETER(
     &          LXRGA4=213,LXRIRX=214,LXRXRG=216,LXRTDF=218,LXRTDC=220,
     &          LXRHLF=222,LXRCLF=224,LXRCLC=226,LXROHD=228,LXROIT=229,
     &          LXROTT=230,LXROFT=231,LXRXMX=232,LXRXCM=233,LXAICT=234)

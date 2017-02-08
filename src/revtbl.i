c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c revision       REV or RV
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c table of outliers identified during revision analysis      OT
c table of msr seasonal filters in revision period           R0
c revision of s.a. data                                      R1
c conc and final est. of s. a. data                          R1A
c revision of seasonal                                       R2
c conc and final est. of seasonal                            R2A
c revision of change in s.a.                                 R3
c conc and final est. of change in s.a.                      R3A
c revision of trend                                          R4
c conc and final est. of trend                               R4A
c likelihood revisions                                       R5
c forecast revisions                                         R6
c-----------------------------------------------------------------------
      INTEGER LRVHDR,LREVOT,LREVR0,LREVR1,LREVR2,LREVR3,LREVR4,LREVR5,
     &        LREVR6,LREVR7,LREVR8,LREVR9,LRVSSH,LRVR9A,LRVR9B
      PARAMETER(
     &          LRVHDR=235,LREVOT=236,LREVR0=237,LREVR1=238,LREVR2=241,
     &          LREVR3=244,LREVR4=247,LREVR5=250,LREVR6=253,LREVR7=256,
     &          LREVR8=257,LREVR9=259,LRVSSH=260,LRVR9A=261,LRVR9B=262)

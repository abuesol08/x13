c-----------------------------------------------------------------------
c	These tables must be consistant with the level variable
c in getprt, in dfttbl in gtinpt, and tbldic in opnfil.
c 	Variables  with LSL<spec> are the displacements for the tables
c found in the specs below, NSL<spec> are the number of tables used in
c each spec. The spec's that have savelog tables are
c
c regression     REG
c automdl        AUM
c estimate       EST
c x11            X11 
c history        REV 
c slidingspans   SSP 
c composite      CMP 
c-----------------------------------------------------------------------
      INTEGER LSLADJ,NSLADJ
      INTEGER LSLAUM,NSLAUM
      INTEGER LSLAXM,NSLAXM
      INTEGER LSLEST,NSLEST
      INTEGER LSLREG,NSLREG
      INTEGER LSLOTL,NSLOTL
      INTEGER LSLCHK,NSLCHK
      INTEGER LSLX11,NSLX11
      INTEGER LSLXRG,NSLXRG
      INTEGER LSLREV,NSLREV
      INTEGER LSLSSP,NSLSSP
      INTEGER LSLSPC,NSLSPC
      INTEGER LSLCMP,NSLCMP
      INTEGER LSLSET,NSLSET
c-----------------------------------------------------------------------
      PARAMETER (LSLADJ=  0,NSLADJ=  1,
     &           LSLAUM=  1,NSLAUM=  6,
     &           LSLAXM=  7,NSLAXM=  1,
     &           LSLEST=  8,NSLEST=  7,
     &           LSLREG= 15,NSLREG=  2,
     &           LSLOTL= 17,NSLOTL=  1,
     &           LSLCHK= 18,NSLCHK=  8,
     &           LSLX11= 26,NSLX11= 20,
     &           LSLXRG= 46,NSLXRG=  1,
     &           LSLREV= 47,NSLREV=  9,
     &           LSLSSP= 56,NSLSSP=  2,
     &           LSLSPC= 58,NSLSPC= 10,
     &           LSLCMP= 68,NSLCMP= 20,
     &           LSLSET= 88,NSLSET= 14)

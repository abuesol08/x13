c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c Seats      SET, ST
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c spectrum of sa series         S1
c spectrum of irregular         S2
c-----------------------------------------------------------------------
      INTEGER LSETRN,LSETAC,LSESEA,LSEPSS,LSEIRR,LSEPSI,LSESA,LSESAC,
     &        LSETRA,LSEPSC,LSECAF,LSEPSA,LSEFCD,LSES18,LSESEB,LSEWKF,
     &        LSEMDC,LSEPIN,LSESGS,LSESGC,LSETGS,LSETGC,LSESDC,LSETDC,
     &        LSESFS,LSESFC,LSETFS,LSETFC,LSEDOR,LSEDSA,LSEDTR,LSESSM,
     &        LSECYC,LSELTT,LSESSF,LSESSA,LSESTR,LSESCY,LSESE2,LSESE3
      PARAMETER(
     &          LSETRN=344,LSETAC=345,LSESEA=346,LSEPSS=347,LSEIRR=348,
     &          LSEPSI=349,LSESA=350,LSESAC=351,LSETRA=352,LSEPSC=353,
     &          LSECAF=354,LSEPSA=355,LSEFCD=356,LSES18=361,LSESEB=362,
     &          LSEWKF=363,LSEMDC=364,LSEPIN=365,LSESGS=369,LSESGC=370,
     &          LSETGS=371,LSETGC=372,LSESDC=373,LSETDC=374,LSESFS=375,
     &          LSESFC=376,LSETFS=377,LSETFC=378,LSEDOR=379,LSEDSA=380,
     &          LSEDTR=381,LSESSM=382,LSECYC=383,LSELTT=384,LSESSF=385,
     &          LSESSA=386,LSESTR=387,LSESCY=388,LSESE2=389,LSESE3=390)

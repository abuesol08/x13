c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c composite      CMP, CP
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c aggregate series                  A1
c aggheader                         AH
c aggtest                           AT
c indirect d8                       D8
c indirect d9                       D9
c indirect seasonal                 SF
c indirect final seasonal diff.    FSD
c indirect seasonally adjusted      SA
c indirect trend                    TRN
c indirect irregular                IRR
c indirect e1                       E1
c indirect e2                       E2
c indirect e3                       E3
c indirect change original srs      E5
c indirect change sa                E6
c indirect change adjusted sa       E6A
c indirect change rounded sa        E6R
c indirect mcd moving avg           F1
c indirect x11 diagnostics          F2
c indirect q stats                  F3
c indirect yearly totals            E4
c indirect ftest d8                 D8F
c indirect moving seasonality ratio D9A
c indirect residual seas f-test     RSF
c indirect adjusted fin seas adj    SAA
c indirect rounded fin seas adj     RND
c original series                   A1P
c series vrs. ind sa series         E0
c ratios of ind series              R1
c ratios of ind sa series           R2
c ind final seasonal factors        SP
c ind final seasonally adj.         AP
c ind final trend component         TP    
c ind final irregular component     IP
c-----------------------------------------------------------------------
      INTEGER LCMPA1,LCMPA3,LCMPB1,LCPB1P,LCPA18,LCPA19,LCMPAH,LCMPAT,
     &        LCMPD8,LCMPD9,LCMPSF,LCPIPS,LCPFSD,LCMPSA,LCPTRN,LCPIRR,
     &        LCPIPI,LCMPE1,LCMPE2,LCMPE3,LCMPE5,LCPE5P,LCMPE6,LCPE6P,
     &        LCPE6A,LCP6AP,LCPE6R,LCP6RP,LCMPE7,LCPE7P,LCMPE8,LCPE8P,
     &        LCPE11,LCPE18,LCPEEB,LCMPF1,LCMPF2,LCMPF3,LCMPE4,LCPD8F,
     &        LCPD9A,LCPRSF,LCPSAA,LCPRND,LCPA1P,LCMPE0,LCMPR1,LCMPR2,
     &        LCMPSP,LCMPAP,LCMPTP,LCMPIP,LCPILS,LCPIAO,LCPFCF,LCPCAF,
     &        LCPIPA,LCPCRI,LCPRRI,LCPFFC
      PARAMETER(
     &          LCMPA1=284,LCMPA3=285,LCMPB1=286,LCPB1P=287,LCPA18=288,
     &          LCPA19=289,LCMPAH=290,LCMPAT=291,LCMPD8=292,LCMPD9=293,
     &          LCMPSF=294,LCPIPS=295,LCPFSD=296,LCMPSA=297,LCPTRN=298,
     &          LCPIRR=299,LCPIPI=300,LCMPE1=301,LCMPE2=302,LCMPE3=303,
     &          LCMPE5=304,LCPE5P=305,LCMPE6=306,LCPE6P=307,LCPE6A=308,
     &          LCP6AP=309,LCPE6R=310,LCP6RP=311,LCMPE7=312,LCPE7P=313,
     &          LCMPE8=314,LCPE8P=315,LCPE11=316,LCPE18=317,LCPEEB=318,
     &          LCMPF1=319,LCMPF2=320,LCMPF3=321,LCMPE4=322,LCPD8F=323,
     &          LCPD9A=324,LCPRSF=325,LCPSAA=326,LCPRND=327,LCPA1P=328,
     &          LCMPE0=329,LCMPR1=330,LCMPR2=331,LCMPSP=332,LCMPAP=333,
     &          LCMPTP=334,LCMPIP=335,LCPILS=336,LCPIAO=337,LCPFCF=338,
     &          LCPCAF=339,LCPIPA=340,LCPCRI=341,LCPRRI=342,LCPFFC=343)

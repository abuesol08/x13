c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c regression     REG or RG
c x11            X11 or XE
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c ao outlier factors                                AO 
c level change factors                              LC 
c td factors (regression)                           TD 
c holiday factors (regression)                      HOL
c user defined regression factors                   USR
c regarima outlier adj original series              A11
c adjusted original                                 B1 
c modified original                                 MO 
c final modified original                           E1 
c mcd moving avgerage                               F1 
c initial trend                                     ITN 
c modified seasonally adjusted                      E2 
c initial unmodified si b                           B3 
c final modified irregular                          E3 
c initial replacement si b                          B4 
c initial modified si                               IMS 
c initial seasonal                                  ISF 
c change in original                                E5 
c initial seasonal adjusted                         ISA
c change in seasonal adjusted                       E6 
c preliminary trend                                 PTN
c henderson trend adjusted for extremes             E7
c unmodified si b                                   B8 
c final unmodified si                               D8 
c replacement si b                                  B9 
c modified si c                                     C9 
c final replacement si                              D9 
c preliminary seasonal                              PSF
c final seasonal                                    D10
c final seasonal difference                         FSD
c preliminary seasonal adjusted                     PSA
c final seasonal adjusted                           D11
c robust seasonal adjusted                          E11
c final trend                                       D12
c bias correction factors                           BCF
c preliminary irregular                             PI
c final irregular                                   D13
c irregular wts                                     IW
c extreme values                                    EV
c holiday factors                                   H1 
c prior td                                          A4
c irregular excluded from ts                        IRX
c td factors (X11)                                  TDF
c combined adjustment factors                       D16      
c combined adjustment differences                   FAD      
c combined td factos                                CTD
c td adjusted original                              TDO
c ftest, b1                                         B1F
c ftest, hol                                        H1F
c x11 diagnostic summary                            F2 
c q statistics                                      F3 
c yearly totals                                     E4 
c ftest, d8                                         D8F
c moving seasonality ratio                          D9A
c residual seasonality f-test                       RSF
c automatic s.f. selection                          ASF
c td by type of day                                 TDY
c modified original series plot                     B1P
c series vrs. sa series plot                        E0
c ratios of series plot                             RA1
c ratios of sa series plot                          RA2
c final seasonal factors plot                       SFP
c final seasonally adj. plot                        SAP
c final trend component plot                        TRP
c final irregular component plot                    IRP
c-----------------------------------------------------------------------
      INTEGER LX11MO,LX11E1,LX11F1,LXEITN,LX11E2,LX11B3,LX11E3,LX11B4,
     &        LXEIMS,LXEISF,LX11E5,LXEE5P,LXEISA,LX11E6,LXEE6P,LXEPTN,
     &        LX11E7,LXEE7P,LX11B8,LX11D8,LXED8B,LX11E8,LXEE8P,LX11B9,
     &        LX11C9,LX11D9,LXEB10,LXED10,LXEPSF,LXEFSD,LXEARS,LXESNS,
     &        LXEPSA,LXED11,LXESAC,LXEE11,LXED12,LXETAL,LXEBCF,LXETAC,
     &        LX11PI,LXED13,LXEPIR,LXEIAO,LX11IW,LX11EV,LX11H1,LXECHL,
     &        LXED16,LXEPAF,LXEFAD,LXED18,LXEE18,LXEEEB,LXETDO,LXEB1F,
     &        LX11F2,LX11F3,LX11E4,LXED8F,LXED9A,LXERSF,LXEASF,LXETDY,
     &        LX11E0,LXERA1,LXERA2,LXESFP,LXESAP,LXETRP,LXEIRP,LXESAF,
     &        LXETRF,LXEIWF
      PARAMETER(
     &          LX11MO=114,LX11E1=116,LX11F1=117,LXEITN=118,LX11E2=121,
     &          LX11B3=122,LX11E3=123,LX11B4=124,LXEIMS=125,LXEISF=127,
     &          LX11E5=130,LXEE5P=131,LXEISA=132,LX11E6=135,LXEE6P=136,
     &          LXEPTN=137,LX11E7=140,LXEE7P=141,LX11B8=142,LX11D8=143,
     &          LXED8B=144,LX11E8=145,LXEE8P=146,LX11B9=147,LX11C9=148,
     &          LX11D9=149,LXEB10=150,LXED10=152,LXEPSF=153,LXEFSD=154,
     &          LXEARS=155,LXESNS=156,LXEPSA=157,LXED11=159,LXESAC=160,
     &          LXEE11=161,LXED12=162,LXETAL=163,LXEBCF=164,LXETAC=165,
     &          LX11PI=166,LXED13=168,LXEPIR=169,LXEIAO=170,LX11IW=171,
     &          LX11EV=173,LX11H1=175,LXECHL=176,LXED16=177,LXEPAF=178,
     &          LXEFAD=179,LXED18=180,LXEE18=181,LXEEEB=182,LXETDO=183,
     &          LXEB1F=185,LX11F2=186,LX11F3=187,LX11E4=188,LXED8F=189,
     &          LXED9A=190,LXERSF=191,LXEASF=192,LXETDY=193,LX11E0=194,
     &          LXERA1=195,LXERA2=196,LXESFP=197,LXESAP=198,LXETRP=199,
     &          LXEIRP=200,LXESAF=201,LXETRF=202,LXEIWF=203)

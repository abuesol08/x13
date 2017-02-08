c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c M1                                         M1
c Q                                          Q
c Q without M2                               Q2
c Moving seasonality ratio                   MSR
c I/C Ratio                                  ICR
c F-test for stable seasonality, B1          FB1
c F-test for stable seasonality, D8          FD8
c F-test for moving seasonality, D8          MSF
c-----------------------------------------------------------------------
      INTEGER LSLM1,LSLMSR,LSLICR,LSLFB1,LSLFD8,LSLMSF,LSLIDS,LSLALX,
     &        LSLXTS
      PARAMETER(
     &          LSLM1= 27,LSLMSR= 40,LSLICR= 41,LSLFB1= 42,LSLFD8= 43,
     &          LSLMSF= 44,LSLIDS= 45,LSLALX= 46,LSLXTS= 47)

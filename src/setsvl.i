c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c seats model                                 smd
c normality test                              nrm
c total squared error                         tse
c component variance                          cvr
c concurrent estimate error                   cee
c percent reduction se                        prs
c average abs. diff. in annual averages       aad
c-----------------------------------------------------------------------
      INTEGER LSLSMD,LSLXMD,LSLSNR,LSLTSE,LSLCVR,LSLCEE,LSLPRS,LSLAAD,
     &        LSLOUE,LSLOUS,LSLSSG,LSLDW,LSLFRS,LSLALS
      PARAMETER(
     &          LSLSMD= 89,LSLXMD= 90,LSLSNR= 91,LSLTSE= 92,LSLCVR= 93,
     &          LSLCEE= 94,LSLPRS= 95,LSLAAD= 96,LSLOUE= 97,LSLOUS= 98,
     &          LSLSSG= 99,LSLDW=100,LSLFRS=101,LSLALS=102)

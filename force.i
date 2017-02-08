c     Lamda - Value of the parameter $\lambda$ used to determine the
c             weight matrix $C$ for the regression method.
c             Values range from -3.0 to 3.0, with a default of 0.
c     Rol - Value of the AR(1) parameter ($\rho$) used in the regression
c           method. Admissable values must be between 0 and 1,
c           inclusive. If Rol = 1, the modified Denton method is used.
c           The default is 0.9 for monthly series, 0.729 for quarterly
c           series ($(0.9)^3$).
c     Iyrt - constrain yearly totals of SA series
c            (0 - no constraint, 1 - denton method,
c             2 - regression)
c     Begyrt - starting month/quarter for forcing totals
c     Iftrgt - Indicator variable denoting which series is used as the
c              target for forcing the totals of the seasonally adjusted
c              series (0 - Original series, 1 - Original series adjusted
c              for calendar effects, 2 - Original series adjusted for
c              permanent prior adjustment factors, 3 - Original series
c              adjusted for both calendar effects and permanent prior
c              adjustment factors).
c     Mid - Indicator variable denoting whether the forcing adjustment
c           factors are generated as ratios (Mid = 0) or differences
c           (Mid = 1)
c     Lindfr - Logical variable which indicates when indirect seasonally 
c              adjusted series is to be forced
c     Lfctfr - Logical variable which indicates when forecasts are to be
c              included in the forcing operation
c-----------------------------------------------------------------------
      DOUBLE PRECISION Lamda,Rol 
      INTEGER Iyrt,Begyrt,Iftrgt,Mid
      LOGICAL Lindfr,Lfctfr
c-----------------------------------------------------------------------
      COMMON /frccmn/ Lamda,Rol,Iyrt,Begyrt,Iftrgt,Mid,Lindfr,Lfctfr

      SUBROUTINE  FUNMOD(IMAX ,IJMAX,FLUX,S   ,A    ,Q    ,RERA   ,
     +                   F    ,NOUT ,GMIN,GMAX,ICHECK)
C
C     FUNDAMENTAL MODE REBALANCING OF THERMAL FLUX
C
C
C          FLUX   : THE FLUX TO BE REBALANCED
C          S(G,G1): SCATTERING RATE FORM GROUP G TO GROUP G1
C          A      : TOTAL ABSORPTION RATE INCLUDING LEAKAGE
C          Q      : TOTAL SLOWING-DOWN SOURCES FROM EPITHERMAL GROUPS
C          RERA   : INVERSE OF TOTAL REACTION RATES
C          F      : FUNDAMENTAL MODE REBALANCING FACTORS
C          IMAX   : NO OF ENERGY GROUP
C          IJMAX  : NO OF MESH POINT FOR FLUX
C          NOUT   : LOGICAL UNIT NUMBER OF PRINTER
C          GMIN   : START GROUP NUMBER OF THERMAL RANGE
C          GMAX   : =IMAX
C
C
      INTEGER*4          GMIN,GMAX,G,G1
      REAL*4             FLUX(IJMAX,IMAX)
      REAL*4             S(IMAX,IMAX)
      REAL*4             A(IMAX),Q(IMAX),RERA(IMAX),F(IMAX)
C
      DATA        OMEGA / 1.200   /
      DATA        ITMAX / 150     /
CM    DATA        EPS   / 0.00005 /
      DATA        EPS   / 0.0001  /
C
      CALL CLEA( F    , IMAX , 1.0 )
      INEGA  = 0
C
      IF(ICHECK.EQ.1) RETURN
C
C     START OF PROCESS
C
      QTOT    = 0.0
      DO 20 G = GMIN , GMAX
      F(G)    = 1.0
      QTOT    = QTOT + Q(G)
      X       = A(G) - S(G,G)
      DO 10 G1= GMIN , GMAX
      X       = X    + S(G,G1)
   10 CONTINUE
CM    RERA(G) = 1.000 / X
      IF(ABS(X).GT.1.00E-30) THEN
                             RERA(G) = 1.000 / X
                             ELSE
                             INEGA   = 1
                             ENDIF
   20 CONTINUE
C
      IF(INEGA.EQ.1) RETURN
C
C     THE ITERATION PROCESS ON F()
C
      IT   = 0
      OM   = 1.000
C
   30 CONTINUE
      IT   = IT + 1
      ATOT = 0.0
C
      DO 50 G = GMIN , GMAX
      X       = Q(G) - S(G,G)*F(G)
      DO 40 G1= GMIN , GMAX
      X       = X    + S(G1,G)*F(G1)
   40 CONTINUE
      F(G)    = F(G) + OM*( X*RERA(G) - F(G) )
      ATOT    = ATOT + A(G)*F(G)
   50 CONTINUE
C
      X       =  QTOT / ATOT
      DO 60 G = GMIN , GMAX
      F(G)    = X*F(G)
   60 CONTINUE
C
      IF(ABS(X-1.000).LT.EPS)  GO TO 80
      OM      = OMEGA
CADD
      IF(IT.LE. 20) OM = 0.50000
      IF(IT.GT.100) OM = 0.50000
CEND
      IF(IT.LE.ITMAX)  GO TO 30
C
      IF(IT.GT.ITMAX) THEN
                      WRITE(NOUT,*) ' *** IT OVER IN F-MODE *** '
                      CALL CLEA( F , IMAX , 1.0 )
                      RETURN
                      ENDIF
C
      WRITE(NOUT,71)
      WRITE(NOUT,72) IT,X,(G,F(G),G=GMIN,GMAX)
      WRITE(NOUT,73) (G,Q(G),G=GMIN,GMAX)
      WRITE(NOUT,74) (G,A(G),G=GMIN,GMAX)
      DO 70 G1 = GMIN , GMAX
      WRITE(NOUT,75) G1,(G,S(G1,G),G=GMIN,GMAX)
   70 CONTINUE
C
      STOP  401
C
   71 FORMAT(1H1 ,15X,' ERROR STOP AT SUBR(FUNMOD) ]]]]] '//)
   72 FORMAT(31H NO CONVERGENCE IN F-MODE AFTER,I4,11H ITERATIONS/
     @       21H NORMALIZATION FACTOR,1PE12.5/
     @       20H THE PRESENT FACTORS/  6(I4,E14.4,2H *))
   73 FORMAT(22H0INPUT DATA FOR SOURCE/6(I4,1PE14.4,2H *))
   74 FORMAT(26H0INPUT DATA FOR ABSORPTION/6(I4,1PE14.4,2H *))
   75 FORMAT(37H0INPUT DATA FOR SCATTERING GROM GROUP,I4/
     @      ( 6(I4,1PE14.4,2H *) ) )
*  76 FORMAT(//29H CONVERGENCED IN F-MODE AFTER,I4,11H ITERATIONS/
*    @       21H IMAX IJMAX GMIN GMAX,4I12/
*    @       21H NORMALIZATION FACTOR,1PE12.5/
*    @       20H THE PRESENT FACTORS/  6(I4,E14.4,2H *))
C
C
C
   80 CONTINUE
C
*     IF(ICHECK.GE. 2) THEN
*       WRITE(NOUT,76) IT,IMAX,IJMAX,GMIN,GMAX,X,(G,F(G),G=GMIN,GMAX)
*                      IF(MOD(ICHECK,10).EQ.2) THEN
*                            WRITE(NOUT,73) (G,Q(G),G=GMIN,GMAX)
*                            WRITE(NOUT,74) (G,A(G),G=GMIN,GMAX)
CM                           DO 85 G1 = GMIN , GMAX
CM                           WRITE(NOUT,75) G1,(G,S(G1,G),G=GMIN,GMAX)
CM 85                        CONTINUE
*                            ENDIF
*                      ENDIF
C
      NEGA     = 0
      DO  90 G = GMIN , GMAX
CMOD  IF(F(G).LE.0.0) NEGA = 1
      IF(F(G).LE.0.01) NEGA = 1
   90 CONTINUE
C
      IF(NEGA.EQ.1) THEN
                    CALL CLEA( F , IMAX , 1.0 )
                    RETURN
                    ENDIF
C
      DO 100 G = GMIN , GMAX
      A(G)     = F(G)*A(G)
      DO  95 G1= GMIN , GMAX
      S(G,G1)  = F(G)*S(G,G1)
   95 CONTINUE
      DO 100 I = 1 , IJMAX
      FLUX(I,G)= F(G)*FLUX(I,G)
  100 CONTINUE
C
      RETURN
      END

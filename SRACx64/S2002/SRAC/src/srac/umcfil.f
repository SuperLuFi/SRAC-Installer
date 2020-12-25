      SUBROUTINE UMCFIL(EIN,SIN,SCOLD,EHOT,SHOT,DBLWRK,EMAX)
C
C     LOAD NEXT PAGE OR PAGES OF CROSS SECTIONS AT THE ORIGINAL
C     TEMPERATURE INTO CORE. INSURE THAT THE MAXIMUM ENERGY
C     SPACING REQUIRED FOR LINEAR-LINEAR INTERPOLATABLE DOPPLER
C     BROADENED DATA IS NOT EXCEEDED.
C
C     IF THERE IS AN UNRESOLVED RESONANCE REGION INSURE THAT THERE IS
C     AT LEAST TWO POINTS AT THE LOWER AND UPPER ENERGY LIMITS OF THE
C     UNRESOLVED REGION AND DEFINE INDICES TO LOWER AND UPPER ENERGY
C     LIMITS OF THE UNRESOLVED REGION. DO NOT ADD ADDITIONAL ENERGY
C     POINTS WITHIN THE UNRESOLVED RESONANCE REGION.
C
C***** EXPORT ******
      DOUBLE PRECISION OVPI,OVPI2,PI,DDX,DZERO
C***** EXPORT ******
      INTEGER COLD1,COLD2,COLD1P,COLD2P,HOT1,HOT2,UNRES1,UNRES2,
     1        UREACT
C
      COMMON /UMCIDX/ COLD1,COLD2,COLD1P,COLD2P,HOT1,HOT2,N2IN,
     1                HEATME,HEATER,LOHEAT,HIHEAT
      COMMON /UMCPIN/ ITAPE,MATNO,NPE,NPF,NPC,IZA,ISWUN,AM,ELOW,EHI,
     +                RFTEMP,QVALE,QVALF,QVALC,FULOW,FUHIGH
      COMMON /UMCINT/ NPMAX,IFISS,IENDUN,IDPOS,ISWTMP,MF,MT,NP,IMAX,
     +                NSYSI,NSYSO,RQTEMP,NPRT,IPRINT
C
      REAL*4  SCOLD(NPMAX),EHOT(NPMAX),SHOT(NPMAX),EIN(NPMAX),SIN(NPMAX)
      REAL*8  DBLWRK(NPMAX)
C
      DATA PI        /3.14159265358979D+00/
      DATA BOLTZM    /8.6164E-05/
C
C-----DEFINE ARITHMETIC STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
C
      TERP(X,X1,X2,Y1,Y2)=((X2-X)*Y1+(X-X1)*Y2)/(X2-X1)
C
C     AT BEGINNING OF SECTION SET INDICES TO FORCE IMMEDIATE READ AND TO
C     LOAD UP TO THREE PAGES (UP TO 6012 POINTS) OF DATA. IF THIS IS NOT
C     THE BEGINNING OF THE SECTION SET INDICES TO ONLY LOAD THE THIRD
C     PAGE (2004 POINTS) OF DATA.
C
C-----DEFINE MINIMUM ENERGY OF INTEREST (IN EV).
      EMIN  = 1.0E-05
C-----DEFINE MINIMUM ENERGY OF THRESHOLD (IN EV).
      ETHRES= 1.0
C-----DEFINE REQUIRED CONSTANTS.
      ATOP  = 4.0
      OVPI  = 1.0/DSQRT(PI)
      OVPI2 = 2.0*OVPI
C-----INITIALIZE FLAG TO DOPPLER BROADEN SECTION.
      TEMPK  = RQTEMP
      TEMPEF = RQTEMP - RFTEMP
CKSK  IF(TEMPEF.LE.0.0) RETURN
      IF(TEMPEF.LT.2.0) THEN
        IF(TEMPEF.GE.1.0) THEN
          WRITE(NSYSO,6031) RQTEMP,RFTEMP
          WRITE(NPRT ,6031) RQTEMP,RFTEMP
        ENDIF
        RETURN
      ENDIF
C
 6031 FORMAT(1H0,' ** WARNING ** DOPPLER BROADENING IS SKIPPED ]] '
     1      /1H ,' ** BECAUSE DIFFERENCE OF TEMPERATURE IS TOO SMALL ]]'
     2      /1H ,' ** REQUESTED TEMPERATURE      IS ',F10.3,' KELVIN'
     3      /1H ,' ** ORIGINAL PENDF TEMPERATURE IS ',F10.3,' KELVIN'/)
C 
      ALPHA  = AM/(BOLTZM*TEMPEF)
C
      UREACT = ISWUN
      EULOW  = FULOW
      EUHIGH = FUHIGH
      N2IN   = NP
      NFILL  = N2IN
C
      ERUSE = 0.001
      CON   = 2.0*SQRT(1.0/3.0)/3.0
      T     = 1.2
      DT    = 0.1
  340 T     = T-DT
      BOTTOM= T*T+T
      TOP   = BOTTOM+1.0
      ERS   = ABS(1.0-CON*TOP*SQRT(TOP)/BOTTOM)
      IF(ERS-ERUSE) 350,360,340
  350 IF(DT.LT.1.0E-06) GO TO 360
      T     = T + DT
      DT    = 0.1*DT
      GO TO 340
  360 DEMAX = T*T
C
C-----INITIALIZE INDICES TO READ UP TO THREE PAGES OF DATA POINTS.
C
      LOAD1 = 2
      LOAD2 = NPMAX
      HOT1  = 1
      COLD1 = 1
      IFILL = 1
C
      CALL CLEA(EHOT,NPMAX,0.0)
C-----SET UP LOOP OVER POINTS TO LOAD.
*     WRITE(NSYSO,6030) NFILL,IFILL,LOAD1,LOAD2
*     WRITE(NSYSO,6040) ALPHA,EIN(1),DEMAX,ATOP,EMAX,EIN(NFILL)
*6030 FORMAT(1H ,' ## NFILL IFILL LOAD1 LOAD2 ## ',6I6)
*6040 FORMAT(1H ,' ##ALPHA EIN(1) DEMAX ATOP EMAX EIN(NP)## ',1P7E12.5)
C
C     AT THE BEGINNING OF SECTION DECIDE WHETHER OR NOT TO DOPPLER
C     BROADEN. DO NOT DOPPLER BROADEN ANY SECTION IN WHICH THE
C     THRESHOLD IS HIGHER THAN 1,000,000* KT/A (I.E. SECTIONS WHICH
C     HAVE A HIGH ENERGY THRESHOLD, ABOVE WHICH BROADENING WILL HAVE
C     A NEGLIGABLE EFFECT).
C
      ITHRES= 0
      AX    = ALPHA*EIN(1)
      IF(AX.GE.1000000.0) RETURN
C
      IF(EIN(1).GE.ETHRES) THEN
                           ITHRES = 1
                           V      = SQRT(AX)-ATOP
                           ENEXT  = V*V/ALPHA
                           IF(ENEXT.LT.ETHRES.OR.V.LE.0.0) ENEXT = EMIN
                           EHOT(1) = ENEXT
                           SCOLD(1)= 0.0
C
                           ELSE
                           EHOT(1) = EIN(1)
                           SCOLD(1)= SIN(1)
                           IFILL   = 2
                           ENDIF
C
      EKM1    = EHOT(1)
      SKM1    = SCOLD(1)
CMOD  EIN(1)  = SQRT(ALPHA*EHOT(1))
      XSAVE   = SQRT(ALPHA*EHOT(1))
      ENEXT   = DEMAX*EKM1
C
C     LOOP OF ASCENDING ENERGY INCREMENT
C
      DO 100 K=LOAD1,LOAD2
      KK      = K
      IF(IFILL.GT.NFILL)      GO TO 110
C-----ENERGY SPACING IS O.K. ACCEPT NEXT TABULATED POINT.
      IF(EIN(IFILL).LE.ENEXT) THEN
                              EHOT(K) = EIN(IFILL)
                              SCOLD(K)= SIN(IFILL)
                              IFILL   = IFILL + 1
C-----INTERPOLATE TO INSERT POINT AT MAXIMUM ALLOWABLE ENERGY SPACING.
                              ELSE
                              EHOT(K) = ENEXT
                   SCOLD(K)=TERP(ENEXT,EIN(IFILL),EKM1,SIN(IFILL),SKM1)
                              ENDIF
C
      EKM1  = EHOT(K)
      SKM1  = SCOLD(K)
      ENEXT = DEMAX*EKM1
      IF(EKM1.GT.EMAX)  GO TO 110
  100 CONTINUE
      WRITE(NSYSO,109) NPMAX
      WRITE(NPRT ,109) NPMAX
      STOP 998
C
  109 FORMAT(1H ,' ERROR STOP AT SUBROUTINE(UMCFIL) BECAUSE DIMENION SIZ
     +E IS OVERFLOWED ]]]'/1H ,' RESERVED SIZE IS ',I5,' WORDS .')
C
  110 LOAD2 = KK-1
      HOT2  = LOAD2
      COLD2 = LOAD2
      N2TOT = LOAD2
C-----TURN OFF UNRESOLVED REGION.
      IF(UREACT.GT.0) THEN
                      IF(EHOT(1).GE.EUHIGH) THEN
                                            UREACT=0
                                            ENDIF
CMOD                  IF(EIN(HOT2).LE.EULOW) THEN
                      IF(EHOT(HOT2).LE.EULOW) THEN
                                             UREACT=0
                                             ENDIF
                      ENDIF
C
CM    EIN(1)= XSAVE
C
CM    DO 120     K =   2 , HOT2
CM    EIN(K)       = SQRT(ALPHA*EHOT(K))
CM    DX           = EIN(K)-EIN(K-1)
CM    SIN(K-1)     = 0.0
CM    IF(DX.GT.0.0)
CM   +SIN(K-1)     = (SCOLD(K)-SCOLD(K-1))/(DX*(EIN(K)+EIN(K-1)))
CM120 CONTINUE
CM    SIN(HOT2)    = 0.0
C
      CALL  CLEA(EIN   ,NPMAX  ,0.0)
      CALL  CLEA(SIN   ,NPMAX  ,0.0)
C
      EIN(1)       = XSAVE
C
      DO 115     K =   2 , HOT2
      EIN(K)       = SQRT(ALPHA*EHOT(K))
  115 CONTINUE
C
      DZERO        = 0.0000D+0
      CALL DCLEA(DBLWRK ,NPMAX , DZERO )
C
      DO 116     K =   1 , HOT2
      DBLWRK(K)    =  EIN(K)
  116 CONTINUE
C
      DO 120     K =   2 , HOT2
      DDX          = ( DBLWRK(K)-DBLWRK(K-1) )*( DBLWRK(K)+DBLWRK(K-1) )
      IF(DDX.GT.DZERO) SIN(K-1) = (SCOLD(K)-SCOLD(K-1))/DDX
  120 CONTINUE
C
      IF(UREACT.GT.0) THEN
             IF(EHOT(1).GE.EULOW) THEN
                      UNRES1 = 0
                      ELSE
                      ELIMIT = EULOW*1.000001
                      DO 200 I = 1 ,HOT2
                      IF(EHOT(I).GE.ELIMIT) GO TO 210
  200                 CONTINUE
                      WRITE(NSYSO,6022) MATNO,MT,NP
                      WRITE(NSYSO,6019) HOT1 ,HOT2 ,EULOW ,EUHIGH
                      WRITE(NSYSO,6025) ELIMIT,EHOT(1),EHOT(HOT2)
                      STOP 997
  210                 CONTINUE
                      UNRES1 = I
                      IF(EHOT(UNRES1-1).GE.EULOW) UNRES1 = UNRES1-1
                      IF(EHOT(UNRES1-1).GE.EULOW) UNRES1 = UNRES1-1
                                  ENDIF
C
             IF(EHOT(HOT2).LE.EUHIGH) THEN
                                      UNRES2 = HOT2 + 1
                      ELSE
                      ELIMIT   = EUHIGH*1.000001
                      DO 300 I = HOT2 , 1 , -1
                      IF(EHOT(I).LE.ELIMIT) GO TO 310
  300                 CONTINUE
                      STOP 996
  310                 CONTINUE
                      UNRES2   = I
                      ENDIF
C
*               WRITE(NSYSO,6019) HOT1 ,HOT2 ,EULOW ,EUHIGH
*               WRITE(NSYSO,6020) UNRES1,UNRES2,EHOT(1),EHOT(HOT2)
*               WRITE(NSYSO,6021) (EHOT(I),I=UNRES1-1,UNRES2+1)
                ENDIF
C
 6019 FORMAT(1H ,' ## HOT1   HOT2   EULOW   EUHIGH     ##',2I6,1P2E12.5)
 6020 FORMAT(1H ,' ## UNRES1 UNRES2 EHOT(1) EHOT(HOT2) ##',2I6,1P2E12.5)
 6021 FORMAT(1H ,' ## EHOT(UNR) ## ',1P10E11.4)
 6022 FORMAT(1H ,' ## MATNO MT NP ##',3I6)
 6023 FORMAT(1H ,' ## EHOT #',1P10E11.4)
 6024 FORMAT(1H ,' ## SHOT #',1P10E11.4)
 6025 FORMAT(1H ,' ## ELIMIT EHOT(1) EHOT(HOT2) #',1P4E11.4)
C
      CALL CLEA(SHOT,NPMAX,0.0)
C
      CALL     UMCBRN(MT,NPMAX,EIN,SCOLD,SIN,SHOT,UREACT,UNRES1,UNRES2,
     +                OVPI,OVPI2,ATOP )
C
      IF(ITHRES.GT.0) THEN
                      ITHRES=0
                      IF(EHOT(1).GT.1.1*EMIN) SHOT(1)=0.0
                      ENDIF
C
      NP        = HOT2
      DO 400  I = 1 , HOT2
      EIN(I)    = EHOT(I)
      SIN(I)    = SHOT(I)
  400 CONTINUE
C
      ISW       =  100
      IF(ISW.GT.HOT2) ISW = HOT2
CDEL  WRITE(NSYSO,6022) MATNO,MT,NP
CDEL  WRITE(NSYSO,6023) (EIN(I),I=1,ISW)
CDEL  WRITE(NSYSO,6024) (SIN(I),I=1,ISW)
C
      RETURN
      END
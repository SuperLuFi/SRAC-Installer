       SUBROUTINE  BURNRM ( IBC3   , NMAT   , MXNUC , NTNUC  , NTDEPZ ,
     1                      MATDPL , NISO   , VOLM  , FLUXM1 ,
     2                      IPBURN , PHIP   , POWMAT, DNOLD  ,
     3                      SSC1G  , SSF1G  , SSA1G , S2N1G  , SSP1G  ,
     4                      SSC1B  , SSF1B  , SSA1B , S2N1B  , SSP1B  ,
     5                      POWERL , FLXNRM , EVTOJ , REACEV , COEFLX ,
     6                      NTFISS , ABOGA  , AMASS , WTHVYM , J79    ,
     7                      NOUT2  , IBZONE , IBEDIT )
C
C   1. SET ONE-GROUP MICROSCOPIC X-SECTION FOR BURNCL ROUTINE
C      & SET HEAVY METAL WEIGHT
C   2. SET ABSOLUTE FLUX LEVEL AND MATERIAL-WISE POWER
C
       INTEGER*4    MATDPL(NMAT),NISO(NMAT)
       REAL*4       VOLM(NMAT),FLUXM1(NMAT),PHIP(NTDEPZ)
       INTEGER*4    IPBURN(MXNUC,NTDEPZ),IBZONE(NTDEPZ)
       REAL*4       DNOLD(MXNUC,NTDEPZ)
       REAL*4       SSC1G(MXNUC,NMAT) ,SSF1G(MXNUC,NMAT)
       REAL*4       SSA1G(MXNUC,NMAT) ,S2N1G(MXNUC,NMAT)
       REAL*4       SSC1B(NTNUC,NTDEPZ),SSF1B(NTNUC,NTDEPZ)
       REAL*4       SSA1B(NTNUC,NTDEPZ),S2N1B(NTNUC,NTDEPZ)
       REAL*4       SSP1G(MXNUC,NMAT) ,SSP1B(NTNUC,NTDEPZ)
       REAL*4       REACEV(2,MXNUC)    ,AMASS(MXNUC)
       REAL*4       WTHVYM(NMAT),POWMAT(NMAT)
C
C ***  SET ONE-GROUP MICROSCOPIC X-SECTION FOR BURNCL ROUTINE
C      & SET HEAVY METAL WEIGHT
C
       COEFLX  = 1.00000
C
       LENG  = NTNUC*NTDEPZ
       CALL  CLEA ( SSC1B  , LENG  , 0.0 )
       CALL  CLEA ( SSF1B  , LENG  , 0.0 )
       CALL  CLEA ( SSA1B  , LENG  , 0.0 )
       CALL  CLEA ( S2N1B  , LENG  , 0.0 )
       CALL  CLEA ( SSP1B  , LENG  , 0.0 )
       CALL  CLEA ( WTHVYM , NMAT  , 0.0 )
       CALL  CLEA ( POWMAT , NMAT  , 0.0 )
       CALL  CLEA ( PHIP   , NTDEPZ, 0.0 )
C
       DO 200  M   = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(MPOS   .LE.0) GO TO 200
       IF(NISO(M).LE.0) GO TO 200
       WTSAVE      = 0.0
       DO 150 IPOS = 1 , NTNUC
       I           = IPBURN(IPOS,MPOS)
       IF(I.GT.0) THEN
                  SSC1B(IPOS,MPOS) = SSC1G(I,M)
                  SSF1B(IPOS,MPOS) = SSF1G(I,M)
                  SSA1B(IPOS,MPOS) = SSA1G(I,M)
                  S2N1B(IPOS,MPOS) = S2N1G(I,M)
                  SSP1B(IPOS,MPOS) = SSP1G(I,M)
                  IF(IPOS.LE.NTFISS) THEN
                      WTSAVE  = WTSAVE + AMASS(IPOS)*DNOLD(IPOS,MPOS)
                      ENDIF
                  ENDIF
  150  CONTINUE
       SAVE      = WTSAVE/ABOGA
       WTHVYM(M) = SAVE*1.000E-6
       IF(IBEDIT.GT.2) WRITE(NOUT2,*) ' M MPOS  WT(GRAM) : ',M,MPOS,SAVE
  200  CONTINUE
C
       IF(POWERL.LE.0.0) THEN
                         FLXNRM = 0.000
                         RETURN
                         ENDIF
C
C ***  CALCULATE POWER AND SET FACTOR FOR GETTING ABSOLUTE FLUX LEVEL
C
       ACOEF       = EVTOJ
       POWCAL      = 0.0
       SUMFLX      = 0.0
       SUMVOL      = 0.0
       DO 400  M   = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(MPOS   .LE.0) GO TO 400
       IF(NISO(M).LE.0) GO TO 400
       TEMP        = 0.0
       DO 350 I    = 1 , NTNUC
       SAVE        = SSC1B(I,MPOS)*REACEV(2,I)+SSF1B(I,MPOS)*REACEV(1,I)
       TEMP        = TEMP + SAVE*DNOLD(I,MPOS)
  350  CONTINUE
       POWMAT(M)   = TEMP*FLUXM1(M)*ACOEF
       PHIP(MPOS)  = FLUXM1(M)
       IF(IBZONE(MPOS).LE.0) GO TO 400
       SUMFLX      = SUMFLX    + FLUXM1(M)*VOLM(M)
       POWCAL      = POWCAL    + POWMAT(M)*VOLM(M)
       SUMVOL      = SUMVOL    + VOLM(M)
  400  CONTINUE
C
       AVFLX    = SUMFLX/SUMVOL
       IF(IBEDIT.GT.2)
     1 WRITE(NOUT2,*) ' POWERL POWCAL AVFLX : ',POWERL,POWCAL,AVFLX
C
C  --- CASE FOR ONLY ABSORBER BURNUP
C
       IF(POWCAL.LE.0.0) THEN
                     IF(IBC3.EQ.3) THEN
                                   POWERL   = 0.0
                                   COEFLX   = FLXNRM/AVFLX
                                   DO 500 M = 1 , NTDEPZ
                                   PHIP(M)  = PHIP(M)*COEFLX
  500                              CONTINUE
                                   DO 510 M = 1 , NMAT
                                   POWMAT(M)= POWMAT(M)*COEFLX
  510                              CONTINUE
                                   RETURN
C
                                   ELSE
      WRITE(NOUT2,*) ' ** FATAL INPUT ERROR IN BURNUP ROUTINE ]] '
      WRITE(NOUT2,*) ' ** ZERO POWER WAS CALCULATED  ]]] '
      WRITE(NOUT2,*) ' ** PLEASE RERUN USING IBC3=3 & FLUX LEVEL INPUT.'
                                   STOP 999
                                   ENDIF
                         ENDIF
C
C  --- CASE FOR CONSTANT FLUX BURNUP & J79 > 1
C
C
       IF(IBC3.EQ.3.AND.J79.GT.1) THEN
                     COEFLX   = FLXNRM/AVFLX
                     DO 650 M = 1 , NTDEPZ
                     PHIP(M)  = PHIP(M)*COEFLX
  650                CONTINUE
C  --- CASE FOR NORMAL CASE
                     ELSE
                     COEFLX   = POWERL / POWCAL
                     DO 660 M = 1 , NTDEPZ
                     PHIP(M)  = PHIP(M)*COEFLX
  660                CONTINUE
                     FLXNRM   = COEFLX * AVFLX
                     ENDIF
C
       POWER0      = POWERL
       POWERL      = 0.0
       DO 700  M   = 1 , NMAT
       MPOS        = MATDPL(M)
       POWMAT(M)   = 0.0
       IF(MPOS.LE.0) GO TO 700
       TEMP        = 0.0
       DO 690 I    = 1 , NTNUC
       SAVE        = SSC1B(I,MPOS)*REACEV(2,I)+SSF1B(I,MPOS)*REACEV(1,I)
       TEMP        = TEMP + SAVE*DNOLD(I,MPOS)
  690  CONTINUE
       POWMAT(M)   = TEMP*PHIP(MPOS)*ACOEF
       IF(IBZONE(MPOS).LE.0) GO TO 700
       POWERL      = POWERL + POWMAT(M)*VOLM(M)
  700  CONTINUE
C
       IF(IBEDIT.LE.2) RETURN
C
       WRITE(NOUT2,*) ' ** POWERL POWCAL  AVFLX FLXNRM COEFLX  ** ',
     *                 POWER0,POWERL,AVFLX,FLXNRM,COEFLX
       WRITE(NOUT2,*) ' ** POWMAT : ',POWMAT
C
       RETURN
       END

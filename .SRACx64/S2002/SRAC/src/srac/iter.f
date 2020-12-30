      SUBROUTINE ITER (NRMAX,NGMAX,RR,RE,P,FLUX,Q,H,
     &                 S,LOC,LSS,LGV,XEC,DR,FR,MMR,AMAT,FLUXZ,VOLR,
     &                 FLXNEW,REBQ,REBA,REBF,REBR,REBS,REBWRK)
C *********************************************************************
C                           ITER
C     CALCULATION OF THERMAL FLUX FINE STRUCTURE BY OVER-RELAXATION
C *********************************************************************
C
      COMMON /DEPLET/ AKEFF (50)
C
      DIMENSION        RR(*),RE(*),LOC(NGMAX,*),FR(*),VOLR(*),
     &                 XEC(1000),P(NRMAX,NRMAX,*),
     &                 FLUX(NRMAX,NGMAX),Q(NRMAX,NGMAX),
     &                 H(NRMAX,NGMAX),S(NRMAX,NGMAX),
     &                 LSS(NGMAX,*),LGV(NGMAX,*),
     &                 DR(NRMAX,NGMAX),MMR(*),
     &                 FLUXZ(NRMAX,NGMAX),FLXNEW(NRMAX,NGMAX)
      DIMENSION        AMAT(NRMAX,1)
C
      DIMENSION        REBQ(NGMAX),REBA(NGMAX),REBF(NGMAX),REBR(NGMAX),
     @                 REBS(NGMAX,NGMAX),REBWRK(NRMAX,NGMAX)
C
      CHARACTER*4      TYPE(3,2)
C
      COMMON / PIJ2C / DUMY(9),
     &                 IPIJ,ID11,ID12,ID13,ID14,
     &                 NGLAST,NGUP,ITAPE,NGKMAX,
     &                 IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &                 EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR,NO1(9),
     &                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     &                 LCVOLR,LCVOLX,LCVOLM,NO2,AA(950)
      COMMON / MAINC / DUM(63),NOUT1,NOUT2,I66(11),ITYPE,I78(20)
     &                 ,IRANG,I99,I100,CASENM(2),TITLE(18),DMY(880)
C
      EQUIVALENCE     (DUM(40),IFIXS)
      EQUIVALENCE     (DUM(20),IBURN)
C
      DATA  TYPE/'EIGE','NVAL','UE  ','FIXE','D SO','URCE'/
C
C     Q(NR,NG)  DISTRIBUTED SOURCE
C     S(NR,NG)  PSEUDO THERMAL DISTRIBUTED SOURCE
C     H(NR,NG)  BIRTH RATE DISTRIBUTION
C     FLUX      NEUTRON FLUX MULTIPLIED REGION VOLUME AND ENERGY WIDTH
C     DR(NR,NG) LEAKAGE RATE AFTER COLLISION
C     FLUXZ(NR,NG) UNCOLLIDE NEUTRON FLUX
C
C        ITYPE   E - RANGE   NG1    NGF
C       ********************************
C          0     0 - FAST    NGMAX  NONE
C          0     2 - ALL     NGUP-1 NGUP
C          1     2 - ALL     NGUP-1 NGUP
C          1     0 - FAST    NGMAX  NONE
C          1     1 - THERMAL NONE   1
C       ********************************
C
C       IERR = 0/1 CONVERGED/ NOT IN THERMAL ITERATION
C
C *** START OF PROCESS
C
      NG1       = NGMAX
      NGF       = 1
      NRMAX2    = NRMAX*NRMAX
      NGMAX2    = NGMAX*NGMAX
      IERR      = 0
      ROLD      = 1.0
C
*     WRITE(6,*)  ' * VOLR * ',(VOLR(I),I=1,NRMAX)
C
      IF(IRANG.EQ.2) THEN
                     NG1   = NGUP-1
                     NGF   = NGUP
                     ENDIF
C
      FNORM   = 0.0
      FNORMO  = 0.0
      RENORM  = 0.0
      RNEW    = 0.0
      SOLD   = 0.0
      ITTOT  = 0
      KJ     = 1
C
C *** FISSION RATE NORMALIZATION IN EIGENVALUE PROBLEM ***
C
      IF(ITYPE.EQ.0) THEN
                     FISS  = 0.0
                     DO 100 NR  = 1,NRMAX
                     FISS       = FISS + FR(NR)
  100                CONTINUE
                     DO 110 NR  = 1,NRMAX
                     FR(NR)     = FR(NR)/FISS
  110                CONTINUE
C *** FIXED SOURCE NORMALIZATION ***
                     ELSE
                     A  = 0.
                     IF(IFIXS.EQ.2) THEN
                                    DO 120 NR = 1,NRMAX
                                    NM        = MMR(NR)
                                    DO 120 NG = 1,NGMAX
                                    K         = LOC(NG,NM) + 5
                                    A         = A + XEC(K)*FLUXZ(NR,NG)
  120                               CONTINUE
                                    DO 130    NG = 1,NGMAX
                                    DO 130    NR = 1,NRMAX
                                    FLUX (NR,NG) = FLUX (NR,NG)/A
                                    FLUXZ(NR,NG) = FLUXZ(NR,NG)/A
  130                               CONTINUE
C
                                    ELSE
                                    DO 140 NG = 1,NGMAX
                                    DO 140 NR = 1,NRMAX
  140                               A         = A + Q(NR,NG)
                                    ENDIF
C
                     CALL  CLEA( REBQ , NGMAX , 0.0 )
                     RATIO     = 1.0
                     IF(A.NE.0.0) RATIO = 1.0 / A
                     DO 150 NG = 1,NGMAX
                     DO 150 NR = 1,NRMAX
                     Q(NR,NG)  = Q(NR,NG)*RATIO
                     REBQ(NG)  = REBQ(NG) + Q(NR,NG)
  150                CONTINUE
                     IF(NGKMAX.GT.0) THEN
                                     DO 160 NR = 1,NRMAX
                                     FR(NR)    = FR(NR)*RATIO
  160                                CONTINUE
                                     ENDIF
                     SOLD  = A
                     SNORM = 1.0
                     ENDIF
C
  180 FORMAT(1H0,9X,'   ITERATION POWER-SCALING   RESIDUE CRITERON'/)
C
C --- OUTER ITERATION
C
      RSAVE = RELCA
C
      IF(NGKMAX.NE.0) WRITE(NOUT2,180)
C
      DO  600 ITO = 1,ITMOUT
      IF(ITAPE.EQ.2) REWIND 21
C ******************* CASE FOR FIXED SOURCRE PROBLEM
      IF(ITYPE.EQ.1) THEN
C *********************************  CASE FOR THERMAL ENERGY ONLY
                     IF(IRANG.EQ.1)  THEN
                                     DO 190 NG = 1,NGMAX
                                     DO 190 NR = 1,NRMAX
                                     S(NR,NG)  = Q(NR,NG)
  190                                CONTINUE
                                     GO TO  330
C *********************************  CASE FOR FAST OR ALL ENERGY
                                     ELSE
                                     DO 200 NG = 1,NGMAX
                                     DO 200 NR = 1,NRMAX
                                     H(NR,NG)  = Q(NR,NG)
  200                                CONTINUE
                                     ENDIF
C ****************** CASE FOR EIGENVALUE PROBLEM
                     ELSE
                     DO 210 NG = 1,NGMAX
                     DO 210 NR = 1,NRMAX
                     H(NR,NG)  = 0.0
  210                CONTINUE
                     ENDIF
C **** ADDITION OF FAST FISSION SOURCE
      IF(NGKMAX.GT.0) THEN
                      DO 220 NR = 1,NRMAX
                      NM        = MMR(NR)
                      DO 220 NG = 1,NGKMAX
                      K         = LOC(NG,NM) + 6
                      H(NR,NG)  = H(NR,NG)   + XEC(K)*FR(NR)
  220                 CONTINUE
                      ENDIF
C
C === LOOP FOR ENERGY GROUP
C
      DO 300 NG = 1,NG1
      IF(ITAPE.EQ.1) KJ=NG
      IF(ITAPE.EQ.2) READ(21)((P(NR,NJ,KJ),NR=1,NRMAX),NJ=1,NRMAX)
      CALL CLEA ( FLUX(1,NG) , NRMAX  , 0.0 )
      CALL CLEA ( AMAT       , NRMAX2 , 0.0 )
C
C *** GROUP FLUX BY MATRIX INVERSION ***
C
      DO 240 NR = 1,NRMAX
      NM        = MMR(NR)
      L         = LOC(NG,NM) + 10
C === POSITION OF SELF-SCATTERING CROSS SECTION
      DO 230 NJ   = 1,NRMAX
      FLUX(NR,NG) =  FLUX(NR,NG) + P(NJ,NR,KJ)*H(NJ,NG)
      AMAT(NJ,NR) = -XEC(L)*P(NR,NJ,KJ)
  230 CONTINUE
      AMAT(NR,NR) =  AMAT(NR,NR)   +   1.0
  240 CONTINUE
C === MATRIX INVERSION
      TR          =  0.0
      CALL MATINV(AMAT , NRMAX , FLUX(1,NG) , 1 , TR , NRMAX )
*     IF(NG.EQ.1) WRITE(6,*) ' ** ITO  TR FNORM ** ',ITO,TR,FNORM
C
      IF(NG.EQ.NGMAX) GO TO 260
C *** SETTING SLOWING DOWN SOURCE
      DO 250 NR = 1,NRMAX
      NM        = MMR(NR)
C *** AVOID TO MULTIPLY SELF SCATTERING TERM ***
      K1        = LOC(NG,NM)  + 11
      K2        = LGV(NG,NM)  - 2 + K1
      NGD       = NG
      DO 250 KD = K1 , K2
      NGD       = NGD + 1
      H(NR,NGD) = H(NR,NGD) + XEC(KD)*FLUX(NR,NG)
  250 CONTINUE
C
  260 CONTINUE
      IF(IFIXS.EQ.2) THEN
                     DO 270  NR  = 1,NRMAX
                     FLUX(NR,NG) = FLUX(NR,NG) + FLUXZ(NR,NG)
  270                CONTINUE
                     ENDIF
  300 CONTINUE
C     END FAST ENERGY LOOP
      IF(IRANG.EQ.0) GO TO 550
C
C *** S(NR,NG) : PSEUDO SOURCE INTO THERMAL ENERGY RANGE
C
      A         =  0.0
      CALL  CLEA( REBQ , NGMAX , 0.0 )
      DO 310 NG = NGF,NGMAX
      DO 310 NR = 1,NRMAX
      S(NR,NG)  = H(NR,NG)
      A         = A + S(NR,NG)
      REBQ(NG)  = REBQ(NG) + S(NR,NG)
  310 CONTINUE
      RATIO     = 1.0
      IF(A.NE.0.0)  RATIO = 1.0 / A
      DO 320   NG = NGF,NGMAX
      REBQ(NG)    = REBQ(NG)*RATIO
      DO 320   NR = 1,NRMAX
      S(NR,NG)    = S(NR,NG)*RATIO
      FLUX(NR,NG) = FLUX(NR,NG)*RATIO
  320 CONTINUE
      SNORM     = A
C
C *** THERMAL ENERGY RANGE
C
  330 ITCNT      = -1
      IERR       =  1
C
C --- INNER ITERATION ---
C
      DO 500 ITI = 1,ITMINN
      ITCNT      = ITCNT + 1
      ITTOT      = ITTOT + 1
CADD
      IF(ITI.LE.20) THEN
                    RELCA = 0.500000
                    ELSE
                    RELCA = RSAVE
                    ENDIF
C---- BIRTH RATE ---
      A          = 0.0
      DO 340 NG  = NGF,NGMAX
      DO 340 NR  = 1,NRMAX
      H(NR,NG)   = S(NR,NG)
  340 CONTINUE
      DO 370 NR  = 1,NRMAX
      NM         = MMR(NR)
      DO 360 NG  = NGF,NGMAX
      K          = LOC(NG,NM)
      A          = A + XEC(K+9)*FLUX(NR,NG)
C === ABSORPTION FOR RENORMALIZATION
      K1         = K + 10
      K2         = LGV(NG,NM) -1 + K1
      NGD        = NG - LSS(NG,NM)
      DO 350 KD  = K1 , K2
      NGD        = NGD + 1
      IF(NGD.LT.1) GO TO 350
      H(NR,NGD)  = H(NR,NGD) + XEC(KD)*FLUX(NR,NG)
  350 CONTINUE
  360 CONTINUE
  370 CONTINUE
C === LEAKAGE RATE INTO  A FOR RENORMALIZATION
      IF(IFIXS.EQ.2) THEN
                     DO 380 NG = NGF,NGMAX
                     DO 380 NR =   1,NRMAX
                     A         = A + DR(NR,NG)*H(NR,NG)
 380                 CONTINUE
                     ENDIF
C
C === COLLISION RATE CALCULATION
C
      IF(ITAPE.EQ.2) THEN
                     REWIND 21
                     IF(IRANG.EQ.2 )  THEN
                                      DO 390  NG=1,NG1
                                      READ(21)
  390                                 CONTINUE
                                      ENDIF
                     ENDIF
C
      CALL   CLEA( FLXNEW , NRMAX*NGMAX , 0.0 )
      CALL   CLEA( REBWRK , NRMAX*NGMAX , 0.0 )
C
      IF(ITAPE.EQ.2) THEN
                     DO 395     NG = NGF,NGMAX
                     READ(21) ((P(NR,NJ,KJ),NR=1,NRMAX),NJ=1,NRMAX)
                     DO 395     NR = 1,NRMAX
                     DO 395     NJ = 1,NRMAX
                     FLXNEW(NR,NG) = FLXNEW(NR,NG)+P(NJ,NR,KJ)*H(NJ,NG)
  395                CONTINUE
C
                     ELSE
                     DO 400     NG = NGF,NGMAX
                     DO 400     NR = 1,NRMAX
                     DO 400     NJ = 1,NRMAX
                     FLXNEW(NR,NG) = FLXNEW(NR,NG)+P(NJ,NR,NG)*H(NJ,NG)
  400                CONTINUE
                     ENDIF
C **** CHECK NEGATIVE FLUX
         DO 405     NG = NGF,NGMAX
         DO 405     NR = 1,NRMAX
CMOD     IF(FLXNEW(NR,NG).LT.1.00E-50)  FLXNEW(NR,NG) = 1.0000E-50
         IF(FLXNEW(NR,NG).LT.1.00E-30)  FLXNEW(NR,NG) = 1.0000E-30
  405    CONTINUE
C
      CALL    CLEA ( REBA , NGMAX , 0.0 )
      CALL    CLEA ( REBF , NGMAX , 0.0 )
      CALL    CLEA ( REBR , NGMAX , 0.0 )
      CALL    CLEA ( REBS , NGMAX2, 0.0 )
      LENG      =  (NGMAX-NGF+1)*NRMAX
      CALL    CLEA ( H(1,NGF) , LENG  , 0.0 )
C
      DO 420 NR  = 1,NRMAX
      NM         = MMR(NR)
      DO 420 NG  = NGF,NGMAX
      K          = LOC(NG,NM)
      REBA(NG)   = REBA(NG) +  XEC(K+9)*FLXNEW(NR,NG)
      K1         = K + 10
      K2         = LGV(NG,NM) -1 + K1
      H(NR,NG)   = H(NR,NG) + S(NR,NG)
      NGD        = NG - LSS(NG,NM)
      DO 410 KD  = K1 , K2
      NGD        = NGD + 1
      IF(NGD.LT. 1) GO TO 410
      REBS(NG,NGD)  = REBS(NG,NGD) + XEC(KD)*FLXNEW(NR,NG)
      H   (NR,NGD)  = H   (NR,NGD) + XEC(KD)*FLXNEW(NR,NG)
  410 CONTINUE
  420 CONTINUE
C === LEAKAGE RATE INTO  A FOR RENORMALIZATION
      IF(IFIXS.EQ.2) THEN
                     DO 425 NG = NGF,NGMAX
                     DO 425 NR =   1,NRMAX
                     REBA(NG)  = REBA(NG) + DR(NR,NG)*H(NR,NG)
 425                 CONTINUE
                     ENDIF
C
      ICHECK    =  ITO + ITCNT
      CALL  FUNMOD(NGMAX ,NRMAX ,FLXNEW,REBS  ,REBA  ,REBQ  ,REBR  ,
     @             REBF  ,NOUT1 ,NGF   ,NGMAX ,ICHECK )
C
      A         = 0.0
      DO 430 NG = NGF , NGMAX
      A         =  A  +  REBA(NG)
  430 CONTINUE
*     WRITE(6,*) ' ** ITO ITI A(430) ** ',ITO,ITI,A
C
      RENORM    =  1.0 / A
C
      DO 435 NG = NGF,NGMAX
      DO 435 NR = 1,NRMAX
      FLXNEW(NR,NG)= FLXNEW(NR,NG)*RENORM
      H(NR,NG)     = FLXNEW(NR,NG) - FLUX(NR,NG)
  435 CONTINUE
C
      TR         = 0.0
      DO 440 NR  = 1,NRMAX
      NM         = MMR(NR)
      VOLUME     = VOLR(NR)
      RVOL       = 1.0 / VOLUME
      DO 440 NG  = NGF,NGMAX
      K          = LOC(NG,NM)
      TR         = TR +   XEC(K+9)*H(NR,NG)*H(NR,NG)
      REBWRK(NR,NG) = XEC(K+9)*H     (NR,NG)*RVOL
      FLXNEW(NR,NG) = XEC(K+9)*FLXNEW(NR,NG)*RVOL
  440 CONTINUE
C
      DO 445 NG  = NGF,NGMAX
      DO 445 NR  =   1,NRMAX
      FLXNEW(NR,NG) = ABS(FLXNEW(NR,NG))
      REBWRK(NR,NG) = ABS(REBWRK(NR,NG))
  445 CONTINUE
C
      FCHAN      = 0.0
      DO 470 NG  = NGF,NGMAX
      ARATE      = 0.0
      DO 450 NR  =   1,NRMAX
      IF(FLXNEW(NR,NG).GT.ARATE)  ARATE = FLXNEW(NR,NG)
  450 CONTINUE
      IF(ARATE.LE.0.0) GO TO 470
      DO 460 NR  =   1,NRMAX
      SAVE       =   REBWRK(NR,NG)/ARATE
      IF(SAVE.GT.FCHAN) FCHAN = SAVE
  460 CONTINUE
  470 CONTINUE
C --- CHECK INNER ITERATION CONVERGENCE
      RNEW       = SQRT(TR)
      EPS        = EPSI
      IF(ITO.LE.3 .AND. ITMOUT.NE.1) EPS= 0.05
*     WRITE(6,*) ' ** ITO ITI RNEW FCHAN EPS ** ',
*    @                ITO,ITI,RNEW,FCHAN,EPS
C
      IF(RNEW.LE.EPS.AND.FCHAN.LE.EPS) THEN
                      IF(ITO.GT.3.OR.ITMOUT.EQ.1) IERR = 0
                      GO TO 510
                      ENDIF
C --- MODIFICATION BY S.O.R.
      IF(ITCNT.GT.0) THEN
                     RATIOR = 0.0
                     IF(ROLD.GT.0.0)  RATIOR = RNEW/ROLD
                     IF(ABS(RATIOR-1.0000).LT.EPSI) GO TO 479
                     ENDIF
C
      CALL RELAX(ITCNT,RNEW,OVER,RENORM)
C
  479 CONTINUE
      ROLD  = RNEW
C
*     WRITE(6,*) ' ITO ITI RNEW OVER RENORM * ',ITO,ITI,RNEW,OVER,RENORM
      IF(ITCNT.EQ.0) OVER = 1.000
C
      DO   480 NG = NGF,NGMAX
      DO   480 NR = 1,NRMAX
  480 FLUX(NR,NG) = FLUX(NR,NG) + OVER*H(NR,NG)
  500 CONTINUE
C
C === END OF THERMAL ITERATION
C
  510 IF(IRANG.EQ.2) THEN
C     SCALING BY  THERMAL SOURCE
      DO 520   NG = NGF,NGMAX
      DO 520   NR =   1,NRMAX
      FLUX(NR,NG) = FLUX(NR,NG)*SNORM
      IF(IFIXS.EQ.2) FLUX(NR,NG) = FLUX(NR,NG) + FLUXZ(NR,NG)
  520 CONTINUE
                     ENDIF
C
      IF(IRANG.EQ.1) THEN
C     SCALING BY FIXED THERMAL SOURCE
      DO 530   NG = NGF,NGMAX
      DO 530   NR =   1,NRMAX
      FLUX(NR,NG) = FLUX(NR,NG)*SOLD
  530 CONTINUE
                     ENDIF
C
      IF(IRANG.EQ.1) GO TO  600
C *** FISSION RATE INTEGRATION ***
  550 FISS      = 0.0
      DO 560 NR = 1,NRMAX
      NM        = MMR(NR)
      RR(NR)    = 0.0
      DO 560 NG = 1,NGMAX
      L         = LOC(NG,NM) + 4
      POW       = XEC(L)*FLUX(NR,NG)
      RR(NR)    = RR(NR) + POW
      FISS      = FISS   + POW
  560 CONTINUE
      FNORM     = FISS
      IF(ITYPE.EQ.  1)  FISS = 1.0
      IF(FISS .LE.0.0)  FISS = 1.0
C****  CHECK POWER DISTRIBUTION  *****
      SEMAX       = -1.000E+30
      SEMIN       = +1.000E+30
      CALL CLEA( REBWRK(1,1) , NRMAX , 0.0 )
C
      DO 565  NNR = 1,NRMAX
      IF(RR(NNR).LE.0.0) GO TO 565
      SAVE        =  FR(NNR) / RR(NNR)
      REBWRK(NNR,1) = SAVE
      IF(SAVE.GT.SEMAX)  SEMAX = SAVE
      IF(SAVE.LT.SEMIN)  SEMIN = SAVE
  565 CONTINUE
C****  SUCCESIVE OVER RELAXATION *****
      SOVER       = 1.0
      IF(ITO.GT.  2)  SOVER = 0.5000
      IF(ITO.GT. 20)  SOVER = 1.2000
      IF(ITO.GT.100)  SOVER = 0.5000
C
      TR        = 0.0
      DO 570 NR = 1,NRMAX
      POW       = RR(NR)/FISS - FR(NR)
      TR        = TR     + POW*POW
      FR(NR)    = FR(NR) + SOVER*POW
  570 CONTINUE
      TR        = SQRT(TR)
      WRITE(NOUT2,630) ITO,FNORM,TR,EPSO
*     WRITE(NOUT1,*) ' * ITO FNORM TR EPSO * ',ITO,FNORM,TR,EPSO
CMOD  IF(IERR.EQ.   1) GO TO  600
      IF(IERR.EQ.   1) THEN
                       FNORMO = FNORM
                       GO TO  600
                       ENDIF
C
CM    IF(TR  .LT.EPSO) GO TO  610
      IF(ITYPE.EQ.1.AND.FNORM.EQ.0.0) GO TO 610
C
      IF( ITO .LE. 3)  THEN
                       FNORMO = FNORM
                       GO TO 600
                       ENDIF
C
      SAVE  = ABS( FNORMO-FNORM ) / FNORMO
      CSAVE =  SEMIN
      IF(SEMIN.LE.0.0) CSAVE = 1.0
      SCHAN = ABS(SEMAX-SEMIN)/CSAVE
C
      IF(SAVE.LT.EPSO.AND.TR.LT.EPSO) THEN
                                      IF(SCHAN.LT.EPSO) GO TO 610
*                         WRITE(6,*)  ' * ITO,SEMAX,SEMIN,SCHAN * ',
*    @                                    ITO,SEMAX,SEMIN,SCHAN
*                         WRITE(6,*)  ' * FR-OLD/FR-NEW  * ',
*    @                                   (REBWRK(I,1),I=1,NRMAX)
                                      ENDIF
C
      IF(ITYPE.EQ.1.AND.ITO.GT.15) THEN
                                   IF(SCHAN.LT.EPSO) GO TO 610
*                      WRITE(6,*)  ' * ITO,SEMAX,SEMIN,SCHAN * ',
*    @                                 ITO,SEMAX,SEMIN,SCHAN
*                      WRITE(6,*)  ' * FR-OLD/FR-NEW  * ',
*    @                                 (REBWRK(I,1),I=1,NRMAX)
                                   ENDIF
      FNORMO = FNORM
CADD
      IF(ITMOUT.GT.1.AND.ITO.EQ.ITMOUT) IERR = 2
  600 CONTINUE
C
C *** MESSAGE PRINT ***
C
  610 IF(IERR.EQ.1) WRITE(NOUT1,640)
      IF(IERR.EQ.2) WRITE(NOUT1,650)
      WRITE(NOUT2,620)  TYPE(1,ITYPE+1),TYPE(2,ITYPE+1),TYPE(3,ITYPE+1),
     &                  ITTOT,SOLD,SNORM,RENORM,RNEW,FNORM,TR
C
      IF(ITYPE.EQ.0.AND.IBURN.GT.0) THEN
                                AKEFF(I78(2)+1) = FNORM
CDEL                            AKINF(I78(2)+1) = FNORM
                                ENDIF
C
       RETURN
C
  620 FORMAT(///10X,'=== ITERATION END IN PIJF STEP ==='/
     & 10X,'=== ',3A4 ,' TYPE PROBLEM ==='/
     & 10X,'TOTAL THERMAL INNER ITERATION COUNT  ',I12/
     & 10X,'INITIAL SOURCE NORMALIZATION FACTOR  ',E12.5/
     & 10X,'THERMAL SOURCE NORMALIZATION FACTOR  ',E12.5/
     & 10X,'RENORMALIZATION FACTOR (INNER)       ',E12.5/
     & 10X,'RESIDUE IN FINAL INNER ITERATION     ',E12.5/
     & 10X,'FINAL FISSION SOURCE NORMALIZATION   ',E12.5/
     & 10X,'RESIDUE IN FINAL FISSION RATE        ',E12.5)
  630 FORMAT(10X,I5,8X,3E12.5)
  640 FORMAT(' ** CAUTION ** THERMAL INNER ITERATION NOT CONVERGED **')
  650 FORMAT(' ** CAUTION ** OUTER ITERATION NOT CONVERGED **')
C
       END

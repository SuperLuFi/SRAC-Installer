      SUBROUTINE MIXX1(ICODE, NRR, NXR, NG, NTL, MATNM, MAR, IXR, NM,
     *                 IJG, IASIZ, IDSIZ, FLUX, FLUX1,
     *                 AA, II, AA1, II1, AAN, IIN, SIGACT, SIGF,XNSIGF,
     *                 SIGT, SIGT1, D1, D2, SIGA  , WT,
     *                 WT1, UPSCAT, UPSCA1, SIGN2T, XX, POW ,
CMOD *                 XI, BVSIGF, XD, DD, SIGS, SIGS1, SIGN2N ,LDMAX )
     *                 XI, BVSIGF, XD, DD, SIGS, SIGS1, SIGN2N ,LDMAX ,
     *                     BLSIGF  )
C
C     SUBPROGRAM TO PRODUCE THE AVERAGE X-SECTION CALLED FROM MIXX
C
      CHARACTER*4      CASENM,TITLE,FILENM,ICF,NUMB
C
      COMMON /MAINC /  IOPT(38),ISCT,DUM1(3),IP1C,IFF1,
     *                 DUM2(10),NEF,NET,
     *                 NERF,NERT,NMAT,DUM3(4),NOUT1,
     *                 NOUT2,DUM4(10),NFOUT,DUM5(21),
     *                 IRANG,ICF,I100,CASENM(2),TITLE(18)
C
      COMMON /PDSPDS/  BUF(540),IFLSW,FILENM(3)
      COMMON /TMPSET/  STND(35),NUMB(61),NTDUMY
C
      CHARACTER*4      MATNM(2,*), NODE(2) , RANGE(3)
C
      DIMENSION FLUX(IJG),FLUX1(IJG),AAN(IASIZ), AA1(IASIZ), AA(IASIZ),
     *  SIGACT(NG), SIGF(NG), XNSIGF(NG), SIGT(NG), SIGT1(NG), D1(NG),
     *  D2(NG), SIGA(NG), WT(NG), UPSCAT(NG), UPSCA1(NG), SIGN2T(NG),
     *  XX(NG), POW(NM), XI(NG,NM), BVSIGF(15,NG), XD(15,NG,NM),
     *  DD(IDSIZ), WT1(NG),
     *  SIGS(NG,LDMAX), SIGN2N(NG,LDMAX), SIGS1(NG,LDMAX),
     *  MAR(1), IXR(1), II(1), II1(1), IIN(1)
C
      DIMENSION  BLSIGF(15,NG)
C
      DIMENSION  REAC(9)
C
      DATA  RANGE /'FAST','THER','ALL '/
C
C     START OF PROCESS
C
      LDM      = NG
      IF(IRANG.EQ.0) LDM = LDM + NET
*     WRITE(6,*)  ' ** LDMAX LDM IRANG NET ** ',LDMAX,LDM,IRANG,NET
C === READ IN FLUXES
      IFLSW    = 1
      FILENM(1)= 'FLUX'
      FILENM(2)= '    '
      NODE(1)  = CASENM(1)
      NODE(2)  = 'X00X'
      NODE(2) (4:4) = ICF (4:4)
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
      CALL READ(NODE,FLUX,IJG)
C === READ IN CURRENTS IF ANISN IS CALLED AT PREVIOUS STEP
      IF(ISCT.GT.0.AND.ICODE.EQ.2) THEN
                                   IF(ICF.EQ.'0002') THEN
                                                     NODE(2)(4:4)='3'
                                                     ELSE
                                                     NODE(2)(4:4)='1'
                                                     ENDIF
                                    CALL READ(NODE,FLUX1,IJG)
                                    ELSE
                                    DO 30 I = 1 ,IJG
                                    FLUX1(I) = FLUX(I)
   30                               CONTINUE
                                    ENDIF
C
   41 FORMAT(1H0,9X,'X-REGION ',I2,'***')
CKS55 FORMAT(10X,A,20I4)
   55 FORMAT(10X,A,20I4:/(34X,20I4)) 
C
C === LOOP OF XREGION NUMBER
C
      DO 1000 IX = 1,NXR
      FILENM(1)  = 'MACR'
      FILENM(2)  = 'O   '
      IF(ICF.EQ.'0002') FILENM(2) = 'OWRK'
      WRITE(NOUT2,41) IX
      IM         = 0
C === CLEAR ARRAY
      CALL CLEA ( SIGACT, 14*NG    , 0.0 )
      CALL CLEA ( SIGS  , NG*LDMAX , 0.0 )
      CALL CLEA ( SIGN2N, NG*LDMAX , 0.0 )
      CALL CLEA ( SIGS1 , NG*LDMAX , 0.0 )
      CALL CLEA (  REAC ,   9      , 0.0 )
      CALL CLEA ( BVSIGF, 15*NG    , 0.0 )
      CALL CLEA ( BLSIGF, 15*NG    , 0.0 )
      CALL CLEA ( SIGN2T,    NG    , 0.0 )
      CALL CLEA ( XI    , NM*NG    , 0.0 )
      CALL CLEA ( XX    ,    NG    , 0.0 )
      CALL CLEA ( POW   ,    NM    , 0.0 )
      CALL CLEA ( XD    , NM*NG*15 , 0.0 )
C === FIND R-REGIONS(ZONE) AND MATERIAL
      MATNP      = 0
      N2N        = 0
      NDELAY     = 0
C     NP1  NUMBER OF MATERIALS WHICH HAVE P1 COMPONENTS
      NP1        = 0
      WRITE(NOUT2,55) 'SIGNED MAT ID BY R-REG =',(MAR(IR),IR=1,NRR)
C
      DO 300 IR = 1,NRR
      IF(IXR(IR).NE.IX)  GO TO 300
      MATN      = IABS(MAR(IR))
      IF (MATN.EQ.MATNP) GO TO 100
      IP1       = 0
      IF (MAR(IR).LE.0) IP1 = 1
      MATNP     = MATN
      LTH       = 0
      LTH1      = 0
      NODE(1)   = MATNM(1,MATN)
      NODE(2)   = MATNM(2,MATN)
C     'F'  'T'  'A'
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      NODE(2) (4:4) = ICF            (4:4)
      CALL SEARCH(NODE,LTH,ISW)
C
      ISWTR         = 0
      IF (ISW.EQ.1)  THEN
CMOD                 WRITE(NOUT1,6010) NODE(1),NODE,FILENM(1),FILENM(2)
CMOD                 STOP
                     JSW   = 0
                     NODE(2) (4:4) = '4'
                     CALL SEARCH(NODE,LTH,JSW)
                     IF(JSW.EQ.1) THEN
                     WRITE(NOUT1,6010) NODE(1),NODE,FILENM(1),FILENM(2)
                                  STOP
                                  ENDIF
                     ISWTR = 1
                     ENDIF
C
 6010 FORMAT(' *** ERROR STOP IN MIXX-STEP , MEMBER ',A4,'FXXX NOR '
     * ,2A4,' NOT FOUND IN ',2A4,' FILE')
C
      CALL READ(NODE,AA,LTH)
C
      IF(ICF.EQ.'0002') THEN
                        NODE(2)(4:4)='3'
                        ELSE
                        NODE(2)(4:4)='1'
                        ENDIF
       CALL SEARCH(NODE,LTH1,ISW)
       IF(ISW.EQ.0)     THEN
                        CALL READ(NODE,AA1,LTH1)
                        IP1 = 1
                        NP1 = NP1+1
                        ELSE
                        IP1 = 0
                        ENDIF
C ==  N2N DATA
      IF(ICF.EQ.'0000') THEN
                        NODE(2) (4:4) =  'N'
                        ELSE
                        NODE(2) (4:4) =  'M'
                        ENDIF
      CALL SEARCH(NODE,LTHN,ISW)
      IFN2N = 0
      IF (ISW.EQ.1) GO TO 86
      N2N   = N2N+1
      IFN2N = 1
      CALL READ(NODE,AAN,LTHN)
C ==  DELAYED NEUTRON DATA
   86 IF(ICF.EQ.'0000') THEN
                        NODE(2) (4:4) =  'Z'
                        ELSE
                        NODE(2) (4:4) =  'Y'
                        ENDIF
      CALL SEARCH(NODE,LTHD,ISW)
      IDELAY = 0
      IF (ISW.EQ.1) GO TO 90
      NFAMLY = 6
CMOD  IF (LTHD.EQ.NG*30) NFAMLY = 15
      IF (LTHD.EQ.NG*45) NFAMLY = 15
      NDELAY = 1
      IDELAY = 1
      CALL READ(NODE,DD,LTHD)
C
   90 IM     = IM+1
      POW(IM)= 0.0
C === ACCUMULATION
  100 LOC    = 0
      LOC1   = 0
      LOCN   = 0
C===== LOOP OF ENERGY GROUP
      DO  200 N = 1,NG
      IFAD      = NRR*(N-1) + IR
      FF        = FLUX(IFAD)
      WT(N)     = WT(N)     + FF
      FF1       = FLUX1(IFAD)
      WT1(N)    = WT1(N)  +  FF1
C
      SIGACT(N) = SIGACT(N) + AA(LOC+3)*FF
      SIGF  (N) = SIGF  (N) + AA(LOC+4)*FF
      XNSIGF(N) = XNSIGF(N) + AA(LOC+5)*FF
      SIGT  (N) = SIGT  (N) + AA(LOC+6)*FF
      POW  (IM) = POW(IM)   + AA(LOC+5)*FF
      XI (N,IM) = AA(LOC+7)
      SIGA  (N) = SIGA  (N) + AA(LOC+10)*FF
      IF (AA(LOC+8).NE.0.0) D1   (N) = D1(N)    + FF/AA(LOC+8)
      IF (AA(LOC+9).NE.0.0) D2   (N) = D2(N)    + FF/AA(LOC+9)
      IF (IP1.EQ.1)         SIGT1(N) = SIGT1(N )+ FF1*AA1(LOC1+6)
C
C ==  DELAYED NEUTRON DATA
C
      IF (IDELAY.EQ.1)  THEN
                        LCC          = (N-1) * NFAMLY
                        DO 120   IG  = 1,NFAMLY
                        LCC          = LCC + 1
                        LCC2         = LCC + NG*NFAMLY*2
                        BVSIGF(IG,N) = BVSIGF(IG,N)+DD(LCC)*FF
                        XD(IG,N,IM)  = DD(LCC+NG*NFAMLY)
                        BLSIGF(IG,N) = BLSIGF(IG,N)+DD(LCC2)*FF
  120                   CONTINUE
                        ENDIF
C     P0 SCATTERING DATA
      N1        = N   - II(LOC+1) + 1
      N2        = N   + II(LOC+2) - II(LOC+1)
      LOC       = LOC + 10
      DO 150 ND = N1,N2
      LOC       = LOC + 1
      IF(ND.LT.1) GO TO 145
      SIGS(N,ND)=SIGS(N,ND) + AA(LOC)*FF
      GO TO 150
  145 UPSCAT(N) =UPSCAT(N)  + AA(LOC)*FF
  150 CONTINUE
C     P1 SCATTERING DATA
      IF(IP1.EQ.1)  THEN
                    N1         = N    - II1(LOC1+1) + 1
                    N2         = N    + II1(LOC1+2) - II1(LOC1+1)
                    LOC1       = LOC1 + 10
*                   WRITE(6,*) ' ** N N1 N2 (P1)** ',N,N1,N2
                    DO 160 ND  = N1,N2
                    LOC1       = LOC1 + 1
                    IF(ND.LT.1) GO TO 155
                    SIGS1(N,ND)= SIGS1(N,ND) + AA1(LOC1)*FF1
                    GO TO 160
  155               UPSCA1(N)  = UPSCA1(N)   + AA1(LOC1)*FF1
  160               CONTINUE
                    ENDIF
C === N2N DATA
      IF(IRANG.NE.1.AND.IFN2N.NE.0.AND.LOCN.LT.LTHN)
     1                     THEN
                           N1        =  N   - IIN(LOCN+1) + 1
                           N2        =  N   + IIN(LOCN+2) - IIN(LOCN+1)
*                          WRITE(6,*) ' ** N N1 N2 (N2N)** ',N,N1,N2
                           SIGN2T(N) = SIGN2T(N) + AAN(LOCN+6)*FF
                           LOCN      = LOCN + 10
                           DO 195 ND = N1,N2
                           LOCN      = LOCN+1
                           SIGN2N(N,ND)=SIGN2N(N,ND)+AAN(LOCN)*FF
  195                      CONTINUE
                           ENDIF
  200 CONTINUE
C
  300 CONTINUE
      POWER     = 0.0
      DO 400 IN = 1,IM
      DO 350 N  = 1,NG
  350 XX(N)     = XX(N) + XI(N,IN)*POW(IN)
  400 POWER     = POWER + POW(IN)
C ==  DELAYED FISSION SPECTRUM
      IF (NDELAY.EQ.1)  THEN
                        DO 405   N  = 1,NG
                        DO 405   IG = 1,NFAMLY
                        XD(IG,N,1 ) = XD(IG,N,1) * POW(1)
  405                   CONTINUE
                        IF (IM.LE.1) GO TO 450
                        DO 410   IN = 2,IM
                        DO 410   N  = 1,NG
                        DO 410   IG = 1,NFAMLY
                        XD(IG,N,1)  = XD(IG,N,1) + XD(IG,N,IN)*POW(IN)
  410                   CONTINUE
                        ENDIF
C
  450 CONTINUE
CADD
      RPOWER   = 0.000
      IF(POWER.GT.0.0)  RPOWER = 1.00000/POWER
CEND
      DO 500 N = 1,NG
      REAC(1)  = REAC(1) + SIGACT(N)
      REAC(2)  = REAC(2) + SIGF  (N)
      REAC(3)  = REAC(3) + XNSIGF(N)
      REAC(7)  = REAC(7) + SIGA   (N)
      REAC(8)  = REAC(8) + UPSCAT(N)
      REAC(9)  = REAC(9) + WT    (N)
      SIGACT(N)= SIGACT(N)/WT(N)
      SIGF  (N)= SIGF  (N)/WT(N)
      XNSIGF(N)= XNSIGF(N)/WT(N)
      SIGT  (N)= SIGT  (N)/WT(N)
      UPSCAT(N)= UPSCAT(N)/WT(N)
      IF(N2N  .NE.0  )   SIGN2T(N) = SIGN2T(N)/WT(N)
      IF(D1(N).NE.0.0)   D1(N)     =     WT(N)/D1(N)
      IF(D2(N).NE.0.0)   D2(N)     =     WT(N)/D2(N)
      REAC(4)  = REAC(4) + WT(N)/SIGT(N)
      REAC(5)  = REAC(5) + D1(N)*WT(N)
      REAC(6)  = REAC(6) + D2(N)*WT(N)
      SIGA  (N)= SIGA  (N)/WT(N)
CMOD  IF(POWER.NE.0.0) XX(N)=XX(N)/POWER
                       XX(N)=XX(N)*RPOWER
      IF(NP1.GT.0) THEN
                   SIGT1 (N) = SIGT1 (N)/WT1(N)
                   UPSCA1(N) = UPSCA1(N)/WT1(N)
                   IF(ISWTR.EQ.1) THEN
                                  SIGT(N) = SIGT(N)-0.33333333*SIGT1(N)
                                  ENDIF
                   ENDIF
C === SCATTERING DATA
      DO 460 ND=1,LDM
      IF(ND.GT.NG)  REAC(8)     = REAC(8)+SIGS(N,ND)
C ===            P0   DATA
                    SIGS(N,ND)  = SIGS(N,ND)/WT(N)
C ===            P1   DATA
      IF(NP1.GT.0)  SIGS1(N,ND) = SIGS1(N,ND)/WT1(N)
C ===            N2N  DATA
      IF(N2N.NE.0)  SIGN2N(N,ND)= SIGN2N(N,ND)/WT(N)
  460 CONTINUE
C ===
      IF(NP1.GT.0.AND.ISWTR.EQ.1) THEN
               SIGS(N,N) = SIGS(N,N) - 0.33333333*SIGT1(N)
               ENDIF
C ===       DELAYED NEUTRON DATA
      IF (NDELAY.NE.0) THEN
                       DO 480   IG  = 1,NFAMLY
                       BVSIGF(IG,N) = BVSIGF(IG,N)/WT(N)
                       BLSIGF(IG,N) = BLSIGF(IG,N)/WT(N)
CMOD  IF(POWER.NE.0.0) XD(IG,N,1)   = XD(IG,N,1)  /POWER
                       XD(IG,N,1)   = XD(IG,N,1) * RPOWER
  480                  CONTINUE
                       ENDIF
  500 CONTINUE
C === READ IN BENOIST MODEL DIFFUSION COEFFICIENTS ===
      IF(ICODE.NE.1)    GO TO 502
C     WRITE(6,*) ' IOPT(17)=',IOPT(17)
      IF(IOPT(17).LT.2) GO TO 502
      REWIND NFOUT
C     SKIP FLUX
      READ(NFOUT)
C     WRITE(6,*) ' FLUX READ'
      READ(NFOUT) (D1(N),N=1,NTL)
C     WRITE(6,*) ' D1   READ'
      IF(IOPT(17).EQ.3) READ(NFOUT) (D2(N),N=1,NTL)
C === PRINT OUT
  502 CONTINUE
      DO 505 I = 1,8
  505 REAC(I)  = REAC(I)/REAC(9)
      REAC(4)  = 1.0 /REAC(4)
      WRITE(NOUT2,506) (REAC(I),I=1,9)
      IF(ICODE.EQ.1.AND.IOPT(17).GE.2) WRITE(NOUT2,507)
C
  506 FORMAT(10X,'ONE GROUP CONSTANTS'/         10X,
     * 30H* ACTIVATION CROSS SECTION    ,E12.5/  10X,
     * 30H* FISSION    CROSS SECTION    ,E12.5/  10X,
     * 30H* NU*FISSION CROSS SECTION    ,E12.5/  10X,
     * 30H* TOTAL      CROSS SECTION    ,E12.5/  10X,
     * 30H* DIFFUSION  COEFFICIENT 1    ,E12.5/  10X,
     * 30H* DIFFUSION  COEFFICIENT 2    ,E12.5/  10X,
     * 30H* ABSORPTION CROSS SECTION    ,E12.5/  10X,
     * 30H* SCATTEROUT CROSS SECTION    ,E12.5/  10X,
     * 30H* INTEGRATED FLUX-X-REGION    ,E12.5)
  507 FORMAT(10X,'*** COMMENT *** DIFFUSION COEF. (BENOIST) NOT YET ',
     +'INCLUDED IN THE ABOVE LIST')
C === STORE X-SECTION INTO PACKED FORM ===
      CALL CLEA( AA  , IASIZ , 0.0 )
      CALL CLEA( AAN , IASIZ , 0.0 )
      IF(NP1.GT.0) CALL CLEA( AA1 , IASIZ , 0.0 )
C
      SUMKAI    = 0.0
      DO 501  N = 1 , NG
      SUMKAI    = SUMKAI + XX(N)
  501 CONTINUE
      FACTXI    = 0.00
      IF(SUMKAI.GT.0.0)  FACTXI = 1.0000000 / SUMKAI
      DO 503  N = 1 , NG
      XX(N)     = XX(N)*FACTXI
  503 CONTINUE
CDEL  IF(FACTXI.GT.0.0) THEN
CDEL         WRITE(6,*) ' ** NODE SUMKAI (MIXX1) : ',NODE,SUMKAI
CDEL         WRITE(6,*) ' ** KAI : ',(XX(N),N=1,NG)
CDEL         ENDIF
C
      LOC       = 0
      LOC1      = 0
      LOCN      = 0
      DO 600  N = 1,NG
      AA(LOC+3) = SIGACT(N)
      AA(LOC+4) = SIGF (N)
      AA(LOC+5) = XNSIGF(N)
      AA(LOC+6) = SIGT (N)
      AA(LOC+7) = XX  (N)
      AA(LOC+8) = D1  (N)
      AA(LOC+9) = D2  (N)
      AA(LOC+10)= SIGA (N)
      IF(N2N.NE.0) AAN(LOCN+6)=SIGN2T(N)
      IF(NP1.GT.0)  THEN
                    AA1(LOC1+ 3) = 0.0
                    AA1(LOC1+ 4) = 0.0
                    AA1(LOC1+ 5) = 0.0
                    AA1(LOC1+ 6) = SIGT1(N)
                    AA1(LOC1+ 7) = 0.0
                    AA1(LOC1+ 8) = 0.0
                    AA1(LOC1+ 9) = 0.0
                    AA1(LOC1+10) = 0.0
                    ENDIF
C === FIND SCATTERING VECTOR LENGTH  OF P0  MATRIX
      IFUP      = 0
      IF(UPSCAT(N).NE.0.0) GO TO 520
      DO 510 L  = 1,N
      IF(SIGS(N,L).NE.0.0) GO TO 530
  510 CONTINUE
      L         = N
      GO TO 530
  520 AA(LOC+11)= UPSCAT(N)
      IFUP      = 1
      L         = 1
  530 II(LOC+1) = N - L + 1 + IFUP
      DO 540 LD = N,LDM
      LA        = LDM - LD + N
      IF(SIGS(N,LA).NE.0.) GO TO 550
  540 CONTINUE
  550 II(LOC+2) = LA-L+1+IFUP
C === STORE VECTOR ===
      LOC       = LOC + 10 + IFUP
      DO 560 LD = L,LA
      LOC       = LOC + 1
      AA(LOC)   = SIGS(N,LD)
  560 CONTINUE
C === FIND SCATTERING VECTOR LENGTH  OF  P1 MATRIX
      IFUP1     = 0
      IF(NP1.GT.0) THEN
                   IF(UPSCA1(N).NE.0.) GO TO 620
                   DO 610     L = 1,N
                   IF(SIGS1(N,L).NE.0.) GO TO 630
  610              CONTINUE
                   L            = N
                   GO TO 630
  620              AA1(LOC1+11) = UPSCA1(N)
                   IFUP1        = 1
                   L            = 1
  630              II1(LOC1+1)  =  N-L+1+IFUP1
                   DO 640    LD = N,LDM
                   LA1          = LDM-LD+N
                   IF(SIGS1(N,LA1).NE.0.) GO TO 650
  640              CONTINUE
  650              II1(LOC1+2) = LA1-L+1+IFUP1
C === STORE VECTOR ===
                   LOC1        = LOC1+10+IFUP1
                   DO 660   LD = L,LA1
                   LOC1        = LOC1+1
                   AA1(LOC1)   = SIGS1(N,LD)
  660              CONTINUE
                   ENDIF
C === FIND N2N VECTOR LENGTH
               IF(N2N.NE.0) THEN
                            L           = N
                            IIN(LOCN+1) = 1
                            DO 570   LD = N,LDM
                            LA          = LDM-LD+N
                            IF(SIGN2N(N,LA).NE.0.) GO TO 580
  570                       CONTINUE
CMOD                        GO TO 600
                            LA          = N
                            SIGN2N(N,N) = 0.0
                            AAN(LOCN+6) = 0.0
CEND
  580                       IIN(LOCN+2) = LA - L + 1
C === STORE VECTOR ===
                            LOCN        = LOCN + 10
                            DO 590   LD = L,LA
                            LOCN        = LOCN + 1
                            AAN(LOCN)   = SIGN2N(N,LD)
  590                       CONTINUE
                            ENDIF
  600 CONTINUE
      LTH  = LOC
      LTH1 = LOC1
      LTHN = LOCN
C ===
      NODE(1)=CASENM(1)
      NODE(2)='X0XX'
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
      NODE(2) (3:3) = NUMB(IX) (4:4)
      NODE(2) (4:4) = ICF      (4:4)
      CALL OVRWRT(NODE,AA,LTH)
C
      IF(NP1.GT.0) THEN
                   IF(ICF.EQ.'0002') THEN
                                     NODE(2)(4:4)='3'
                                     ELSE
                                     NODE(2)(4:4)='1'
                                     ENDIF
                   CALL OVRWRT(NODE,AA1,LTH1)
                   ENDIF
C
C === N2N DATA
C
      IF(N2N.NE.0) THEN
                   IF(ICF.EQ.'0002') THEN
                                     NODE(2) (4:4) = 'M'
                                     ELSE
                                     NODE(2) (4:4) = 'N'
CDEL                                 CALL PACK(NODE(2),4,INCHR)
                                     ENDIF
                   CALL OVRWRT(NODE,AAN,LTHN)
                   ENDIF
C
C === DELAYED NEUTRON DATA
C
      IF (NDELAY.EQ.0) GO TO 720
C
      CALL CLEA ( DD , IDSIZ , 0.0 )
      LCC       = 0
C
      DO 710  N = 1,NG
      DO 700 IG = 1,NFAMLY
      LCC       = LCC+1
      DD(LCC)   = BVSIGF(IG,N)
      DD(LCC+NG*NFAMLY) = XD(IG,N,1)
      DD(LCC+NG*NFAMLY*2) = BLSIGF(IG,N)
  700 CONTINUE
  710 CONTINUE
      IF(ICF.EQ.'0000') THEN
                        NODE(2) (4:4) = 'Z'
                        ELSE
                        NODE(2) (4:4) = 'Y'
                        ENDIF
CM    CALL OVRWRT(NODE,DD,NG*NFAMLY*2)
      CALL OVRWRT(NODE,DD,NG*NFAMLY*3)
C
  720 CONTINUE
      NODE(2) (4:4) = ICF(4:4)
      FILENM(1)     = 'FLUX'
      FILENM(2)     = '    '
      CALL OVRWRT(NODE,WT,NG)
      IF(ICODE.NE.2 .OR. ISCT.LE.0) GO TO 1000
CMOD  IF(IP1C.EQ.0)                 GO TO 1000
C === OUTPUT CEALL AVERAGED CURRENT INTO FLUX FILE
C === ONLY AFTER ANSIN SN-PL CALCULATION  (L>0)
      IF(ICF.EQ.'0002') THEN
                        NODE(2)(4:4)='3'
                        ELSE
                        NODE(2)(4:4)='1'
                        ENDIF
      CALL OVRWRT(NODE,WT1,NG)
 1000 CONTINUE
C
C     END OF PROCESS
C
      RETURN
      END

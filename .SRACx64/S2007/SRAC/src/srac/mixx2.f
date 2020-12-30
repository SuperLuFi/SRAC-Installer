      SUBROUTINE MIXX2(ICODE, NRR, NXR, NG, NTL, MATNM, MAR, IXR, NM,
     *                 IJG, IASIZ, IDSIZ, FLUX, FLUX1,
     *                 AA, II, AA1, II1, AAN, IIN, SIGACT, SIGF,XNSIGF,
     *                 SIGT, SIGT1, D1, D2, SIGA, WT,
     *                 WT1, UPSCAT, UPSCA1, SIGN2T, XX,
     *                 POW, XI, BVSIGF, XD, DD, SIGS, SIGS1, SIGN2N)
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
     *  MAR(*), IXR(*), SIGS(NG,*), SIGN2N(NG,*), SIGS1(NG,*),
     *  II(*), II1(*), IIN(*)
C
      DIMENSION  REAC(9)
C
      DATA  RANGE /'FAST','THER','ALL '/
C
C     START OF PROCESS
C
      IF(ICF.EQ.'0000') RETURN
C
      LDM      = NG
      IF(IRANG.EQ.0) LDM = LDM + NET
C === READ IN FLUXES
      IFLSW    = 1
      FILENM(1)= 'FLUX'
      FILENM(2)= '    '
      NODE(1)  = CASENM(1)
      NODE(2)  = 'X00X'
      NODE(2) (4:4) = '2'
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
      CALL READ(NODE,FLUX,IJG)
C
   41 FORMAT(1H0,9X,'X-REGION ',I2,'***')
   55 FORMAT(10X,A,20I4)
 6010 FORMAT(' *** ERROR STOP IN MIXX-STEP , MEMBER ',A4,'FXXX NOR '
     * ,2A4,' NOT FOUND IN ',2A4,' FILE')
C
C === LOOP OF XREGION NUMBER
C
      DO 1000 IX = 1,NXR
      FILENM(1)  = 'MACR'
      FILENM(2)  = 'OWRK'
CDEL  WRITE(NOUT2,41) IX
      IM         = 0
C === CLEAR ARRAY
      CALL CLEA ( SIGACT, 14*NG  , 0.0 )
      CALL CLEA ( SIGS  , NG*LDM , 0.0 )
      CALL CLEA (  REAC ,   9    , 0.0 )
      CALL CLEA ( XI    , NM*NG    , 0.0 )
      CALL CLEA ( XX    ,    NG    , 0.0 )
      CALL CLEA ( POW   ,    NM    , 0.0 )
CDEL  CALL CLEA ( XD    , NM*NG*15 , 0.0 )
C === FIND R-REGIONS(ZONE) AND MATERIAL
      MATNP      = 0
C
      DO 300 IR = 1,NRR
      IF(IXR(IR).NE.IX)  GO TO 300
      MATN      = IABS(MAR(IR))
      IF (MATN.EQ.MATNP) GO TO 100
      MATNP     = MATN
      LTH       = 0
      NODE(1)   = MATNM(1,MATN)
      NODE(2)   = MATNM(2,MATN)
C     'F'  'T'  'A'
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      NODE(2) (4:4) = '4'
      CALL SEARCH(NODE,LTH,ISW)
C
      IF (ISW.EQ.1)  THEN
                     WRITE(NOUT1,6010) NODE(1),NODE,FILENM(1),FILENM(2)
                     STOP
                     ENDIF
C
      CALL READ(NODE,AA,LTH)
      IM     = IM+1
      POW(IM)= 0.0
C === ACCUMULATION
  100 LOC    = 0
C===== LOOP OF ENERGY GROUP
      DO  200 N = 1,NG
      IFAD      = NRR*(N-1) + IR
      FF        = FLUX(IFAD)
      WT(N)     = WT(N)     + FF
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
  200 CONTINUE
  300 CONTINUE
C
C
      POWER     = 0.0
      DO 400 IN = 1,IM
      DO 350 N  = 1,NG
  350 XX(N)     = XX(N) + XI(N,IN)*POW(IN)
  400 POWER     = POWER + POW(IN)
CADD
      RPOWER   = 0.000
      IF(POWER.GT.0.0)  RPOWER = 1.00000/POWER
CEND
C
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
      IF(D1(N).NE.0.0)   D1(N)     =     WT(N)/D1(N)
      IF(D2(N).NE.0.0)   D2(N)     =     WT(N)/D2(N)
      REAC(4)  = REAC(4) + WT(N)/SIGT(N)
      REAC(5)  = REAC(5) + D1(N)*WT(N)
      REAC(6)  = REAC(6) + D2(N)*WT(N)
      SIGA  (N)= SIGA  (N)/WT(N)
CMOD  IF(POWER.NE.0.0) XX(N)=XX(N)/POWER
                       XX(N)=XX(N)*RPOWER
C === SCATTERING DATA
      DO 460 ND=1,LDM
      IF(ND.GT.NG)  REAC(8)     = REAC(8)+SIGS(N,ND)
                    SIGS(N,ND)  = SIGS(N,ND)/WT(N)
  460 CONTINUE
  500 CONTINUE
C === READ IN BENOIST MODEL DIFFUSION COEFFICIENTS ===
CTFREE
CDEL
C     IF(ICODE.NE.1)    GO TO 502
C     WRITE(6,*) ' IOPT(17)=',IOPT(17)
C     IF(IOPT(17).LT.2) GO TO 502
C     REWIND NFOUT
C     SKIP FLUX
C     READ(NFOUT)
C     WRITE(6,*) ' FLUX READ'
C     READ(NFOUT) (D1(N),N=1,NTL)
C     WRITE(6,*) ' D1   READ'
C     IF(IOPT(17).EQ.3) READ(NFOUT) (D2(N),N=1,NTL)
CEND
CTFREE
C === PRINT OUT
  502 CONTINUE
      DO 505 I = 1,8
  505 REAC(I)  = REAC(I)/REAC(9)
      REAC(4)  = 1.0/REAC(4)
CDEL  WRITE(NOUT2,506) (REAC(I),I=1,9)
CDEL  IF(ICODE.EQ.1.AND.IOPT(17).GE.2) WRITE(NOUT2,507)
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
CM    IF(FACTXI.GT.0.0) THEN
CM           WRITE(6,*) ' ** NODE SUMKAI (MIXX2) : ',NODE,SUMKAI
CM           WRITE(6,*) ' ** KAI : ',(XX(N),N=1,NG)
CM           ENDIF
C
      LOC       = 0
      DO 600  N = 1,NG
      AA(LOC+3) = SIGACT(N)
      AA(LOC+4) = SIGF (N)
      AA(LOC+5) = XNSIGF(N)
      AA(LOC+6) = SIGT (N)
      AA(LOC+7) = XX  (N)
      AA(LOC+8) = D1  (N)
      AA(LOC+9) = D2  (N)
      AA(LOC+10)= SIGA (N)
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
  600 CONTINUE
      LTH  = LOC
C ===
      NODE(1)= CASENM(1)
      NODE(2)='X0XX'
      NODE(2) (1:1) = RANGE(IRANG+1) (1:1)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
      NODE(2) (3:3) = NUMB(IX) (4:4)
      NODE(2) (4:4) = '4'
      CALL OVRWRT(NODE,AA,LTH)
 1000 CONTINUE
C
C     END OF PROCESS
C
      RETURN
      END

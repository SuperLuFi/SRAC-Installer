C***********************************************************************
C                             MIXX
C***********************************************************************
      SUBROUTINE MIXX(ICODE,NRR,NXR,NG,NTL,MATNM,MAR,IXR)
C
C     SUBPROGRAM TO PRODUCE THE AVERAGE X-SECTION CONTROL
C
CM    CHARACTER*4      CASENM,TITLE
C
      COMMON /MAINC/ IOPT(38),ISCT,DUM1(3),IP1C,DUM2(11),NEF,NET
     *  ,NERF,NERT,NMAT,DUM3(4),NOUT1,NOUT2,DUM4(10),NFOUT,DUM5(19)
     *  ,MEMORY,IOPEN,IRANG,ICF,I100,CASENM(2),TITLE(18)
C
      COMMON /WORK/ AA(60000)
C
      DIMENSION        MAR(NRR),IXR(NRR)
      CHARACTER*4      RANGE(3),MATNM(2,NMAT)
C
      DATA             RANGE/'FAST','THER','ALL '/
C
*     WRITE(6,*) ' **TEST PRINT IN MIXX**'
*     WRITE(6,*) ' ICODE=',ICODE
*     WRITE(6,*) ' NRR  =',NRR
*     WRITE(6,*) ' NXR  =',NXR
*     WRITE(6,*) ' NG   =',NG
*     WRITE(6,*) ' NTL  =',NTL
*     WRITE(6,*) ' ISCT =',ISCT
*     WRITE(6,*) ' IRANG=',IRANG
*     WRITE(6,*) ' NMAT =',NMAT
*     WRITE(6,*) ' MTNAME =',(MATNM(1,N),MATNM(2,N),N=1,NMAT)
*     WRITE(6,*) ' MAR  =',(MAR(N),N=1,NRR)
*     WRITE(6,*) ' IXR  =',(IXR(N),N=1,NRR)
C
      WRITE(NOUT2,10) RANGE(IRANG+1),CASENM,TITLE
      IF (NXR.LE.0) THEN
                    WRITE(NOUT2,2000) NXR
                    RETURN
                    ENDIF
C
      IJG      = NRR*NG
      LDMAX    = NG
      IF(IRANG.EQ.0) LDMAX=NG+NET
C
C     NM MAX SEARCH
C
      NM       = 0
      DO 30 IX = 1,NXR
      MATNP    = 0
      INM      = 0
      DO 20 IR = 1,NRR
      IF (IXR(IR).NE.IX) GO TO 20
      MATN     = IABS(MAR(IR))
      IF (MATN.EQ.MATNP) GO TO 20
      MATNP    = MATN
      INM      = INM + 1
   20 CONTINUE
      IF (NM.LT.INM) NM = INM
   30 CONTINUE
C
*     WRITE(6,*) ' IJG   = ',IJG,' NM    = ',NM,
*    +           ' LDMAX = ',LDMAX
C
      ISCT1    = 1
C
CMOD  IASIZ = 7500
      IASIZ = 8500
CMOD  IDSIZ = NG*15*2
      IDSIZ = NG*15*3
      KFLUX = 1
      KFLUX1= KFLUX + IJG
      KAA   = KFLUX1+ IJG  *ISCT1
      KAAN  = KAA   + IASIZ
      KAA1  = KAAN  + IASIZ
      KSIGAC= KAA1  + IASIZ*ISCT1
      KSIGF = KSIGAC+ NG
      KXNSIG= KSIGF + NG
      KSIGT = KXNSIG+ NG
      KSIGT1= KSIGT + NG
      KD1   = KSIGT1+ NG*ISCT1
      KD2   = KD1   + NG
      KSIGA = KD2   + NG
      KWT   = KSIGA + NG
      KWT1  = KWT   + NG
      KUPSCA= KWT1  + NG*ISCT1
      KUPSC1= KUPSCA+ NG
      KSIG2T= KUPSC1+ NG*ISCT1
      KXX   = KSIG2T+ NG
      KPOW  = KXX   + NG
      KXI   = KPOW  + NM
      KBVSIG= KXI   + NG*NM
      KXD   = KBVSIG+ NG*15
      KDD   = KXD   + NG*15*NM
      KSIGS = KDD   + IDSIZ
      KSIGS1= KSIGS + NG*LDMAX
      KSIG2N= KSIGS1+ NG*LDMAX*ISCT1
      KBLSIG= KSIG2N+ NG*LDMAX
CMOD  LAST  = KSIG2N+ NG*LDMAX - 1
      LAST  = KBLSIG+ NG*15    - 1
C
      CALL  CLEA( AA , LAST , 0.0 )
C
      IF (LAST.GT.MEMORY) THEN
                          WRITE (NOUT1,2010) MEMORY,LAST
                          STOP
                          ENDIF
C
      CALL MIXX1(ICODE,NRR ,NXR  ,NG   ,NTL ,MATNM, MAR, IXR,
     *           NM   ,IJG ,IASIZ,IDSIZ,AA(KFLUX) , AA(KFLUX1),
     *           AA(KAA),AA(KAA),AA(KAA1),AA(KAA1),AA(KAAN),AA(KAAN),
     *    AA(KSIGAC),AA(KSIGF),AA(KXNSIG),AA(KSIGT),AA(KSIGT1),AA(KD1),
     *       AA(KD2),AA(KSIGA),AA(KWT),AA(KWT1),AA(KUPSCA),AA(KUPSC1),
     *       AA(KSIG2T),AA(KXX),AA(KPOW),AA(KXI),AA(KBVSIG),AA(KXD),
     *       AA(KDD),AA(KSIGS),AA(KSIGS1),AA(KSIG2N),LDMAX ,AA(KBLSIG))
C
      CALL  CLEA( AA , LAST , 0.0 )
C
      CALL MIXX2(ICODE,NRR ,NXR  ,NG   ,NTL ,MATNM, MAR, IXR,
     *           NM   ,IJG ,IASIZ,IDSIZ,AA(KFLUX) , AA(KFLUX1),
     *           AA(KAA),AA(KAA),AA(KAA1),AA(KAA1),AA(KAAN),AA(KAAN),
     *    AA(KSIGAC),AA(KSIGF),AA(KXNSIG),AA(KSIGT),AA(KSIGT1),AA(KD1),
     *       AA(KD2),AA(KSIGA),AA(KWT),AA(KWT1),AA(KUPSCA),AA(KUPSC1),
     *       AA(KSIG2T),AA(KXX),AA(KPOW),AA(KXI),AA(KBVSIG),AA(KXD),
     *       AA(KDD),AA(KSIGS),AA(KSIGS1),AA(KSIG2N)   )
C *** HIGH ORDER PL X-SECTION (PL ORDER>1)
      IF(ISCT.GT.1) THEN
C
      CALL  CLEA( AA , LAST , 0.0 )
      CALL MIXX3(ICODE,NRR ,NXR  ,NG   ,NTL ,MATNM, MAR, IXR,
     *           NM   ,IJG ,IASIZ,IDSIZ,AA(KFLUX) , AA(KFLUX1),
     *           AA(KAA),AA(KAA),AA(KAA1),AA(KAA1),AA(KAAN),AA(KAAN),
     *    AA(KSIGAC),AA(KSIGF),AA(KXNSIG),AA(KSIGT),AA(KSIGT1),AA(KD1),
     *       AA(KD2),AA(KSIGA),AA(KWT),AA(KWT1),AA(KUPSCA),AA(KUPSC1),
     *       AA(KSIG2T),AA(KXX),AA(KPOW),AA(KXI),AA(KBVSIG),AA(KXD),
     *       AA(KDD),AA(KSIGS),AA(KSIGS1),AA(KSIG2N)   )
                    ENDIF
C
C *** PSEUDO 1 X-REGION EDIT
C
      IF(NXR.GT.1) THEN
C *** NM MAX SEARCH
      NM       = 0
      MATNP    = 0
      DO 120 IR = 1,NRR
      IF (IXR(IR).EQ.0)  GO TO 120
      MATN     = IABS(MAR(IR))
      IF (MATN.EQ.MATNP) GO TO 120
      MATNP    = MATN
      NM       = NM + 1
  120 CONTINUE
C
      KXI   = KPOW  + NM
      KBVSIG= KXI   + NG*NM
      KXD   = KBVSIG+ NG*15
      KDD   = KXD   + NG*15*NM
      KSIGS = KDD   + IDSIZ
      KSIGS1= KSIGS + NG*LDMAX
      KSIG2N= KSIGS1+ NG*LDMAX*ISCT1
      KBLSIG= KSIG2N+ NG*LDMAX
CMOD  LAST  = KSIG2N+ NG*LDMAX - 1
      LAST  = KBLSIG+ NG*15    - 1
C
      IF (LAST.GT.MEMORY) THEN
                          WRITE (NOUT1,2010) MEMORY,LAST
                          STOP
                          ENDIF
C
      CALL  CLEA( AA , LAST , 0.0 )
C
      CALL MIX11(ICODE,NRR ,  1  ,NG   ,NTL ,MATNM, MAR, IXR,
     *           NM   ,IJG ,IASIZ,IDSIZ,AA(KFLUX) , AA(KFLUX1),
     *           AA(KAA),AA(KAA),AA(KAA1),AA(KAA1),AA(KAAN),AA(KAAN),
     *    AA(KSIGAC),AA(KSIGF),AA(KXNSIG),AA(KSIGT),AA(KSIGT1),AA(KD1),
     *       AA(KD2),AA(KSIGA),AA(KWT),AA(KWT1),AA(KUPSCA),AA(KUPSC1),
     *       AA(KSIG2T),AA(KXX),AA(KPOW),AA(KXI),AA(KBVSIG),AA(KXD),
     *       AA(KDD),AA(KSIGS),AA(KSIGS1),AA(KSIG2N),LDMAX )
                   ENDIF
C
      RETURN
C
   10 FORMAT(///9X,'*** MIX-X-SECTION STEP IN *** ',A4,
     * ' ENERGY RANGE'/10X,'*** ',2A4,'***',18A4,'***')
 2000 FORMAT('0',10X,'NUMBER OF X-REGION IS LESS THAN OR EQUAL TO',
     *       ' ZERO. WE CANNOT PROCESS MIX-X-SECTION. NXR =',I4   )
 2010 FORMAT(' *** WORK AREA ALLOCATION ERROR IN MIXX STEP , RESET '
     * ,'WORK AREA'/' ALLOCATED MEMORY =',I10,' USED MEMORY =',I10)
C
      END

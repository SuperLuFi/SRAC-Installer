CDEL   @OPTIONS XOPT(NOPREEX)
CDEL     1993/10/28/TSUCHI
CDEL     1993/11/04/KANKEO
      SUBROUTINE GAM(NM,MATNM,MATD,VM,NISO)
C
C     SUBPROGRAM TO PRODUCE THE TRANSPORT X-SECTION
C     BY P1 OR B1 APPROXIMATION
C
      CHARACTER*4      CASENM,TITLE,NUMB,ICF,FILENM
C
      COMMON /MAINC / IOPT(54),
     *                NEF,NET,NERF,NERT,NMAT,DUM2(4),
     *                NOUT1,NOUT2,DUM3(6),
     *                NEF3,DUM4(25),I098,ICF,I100,CASENM(2),TITLE(18),
     *                DUM5(880)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      COMMON /WORK  / SIGS(107,107),
     *                AA(7500),SIGACT(107),SIGF(107),XNSIGF(107),
     *                SIGT(107),D1(107),D2(107),SIGA(107),UPSCAT(107),
     *                XX(107),POW(100),XI(107,100)
C
      CHARACTER*4     MATNM(2,NMAT)
      CHARACTER*4     RANGE(3),NODE(2),NODE2(2)
C
      DIMENSION       MATD(NM),NISO(1),VM(NM)
      DIMENSION       A0(7500),I0(7500),II(7500),F0(107),F1(107)
      DIMENSION       E(108),W(107)
      DIMENSION       LENG(8,300),ICARR(300),NGFT(2)
      EQUIVALENCE    (AA(1),A0(1),II(1),I0(1)),(DUM4(4),NFOUT)
C
      DATA            RANGE  /'FAST','THER','ALL '/
C
C     LENG    1  2  3  4  5  6  7  8
C     E RANG  F  F  F  T  T  T  A  A
C     IPL     TR 0  1  TR 0  1  TR TR
C     FINE    F  F  F  F  F  F  F  C
C
C     START OF PROCESS
C
      NMAT1   = NMAT + 1
      IF(NMAT1.GT.300) GO TO 1200
      CALL ICLEA( LENG , 8*NMAT1 , 0 )
C     READ IN ENERGY BOUNDARIES AND WEIGHTS
      NODE(1) = 'CONT'
      NODE(2) = 'A002'
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
C === CHECK  IF 'CONTA002' EXISTS IN MACROWRK FILE
      CALL SEARCH( NODE , LTH , ISW1  )
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      CALL SEARCH( NODE , LTH , ISW )
C === 'CONTA002' EXISTS IN MACROWRK , THEN GO TO 5
      IF(ISW.EQ.0) THEN
                   CALL READ(NODE,I0(2001),LTH)
                   GO TO 8
                   ENDIF
C === READ IN 'CONTF002'
      NODE(2) (1:1) = 'F'
      CALL SEARCH( NODE , LTH , ISW )
      IF(ISW.EQ.1) GO TO 900
C
      CALL READ  ( NODE , I0       , LTH )
      IF(IOPT(4).EQ.0) GO TO 6
C === READ IN 'CONTT002'
      NODE(2) (1:1) = 'T'
      LTH           = 2*(NET+1)
      CALL READ  ( NODE , I0(1001) , LTH )
C
      I0(2001) = NEF  + NET
      LOC1     = 1
      LOC2     = NEF  + 1
      LOC3     = 2001
      LOC4     = 2001 + I0(2001)
      DO 2   I = 1,NEF
      LOC1     = LOC1 + 1
      LOC2     = LOC2 + 1
      LOC3     = LOC3 + 1
      LOC4     = LOC4 + 1
      A0(LOC3) = A0(LOC1)
      A0(LOC4) = A0(LOC2)
    2 CONTINUE
C
      IF(NET.EQ.0) GO TO 4
      LOC1     = 1001
      LOC2     = 1001 + NET
      DO 3   I = 1,NET
      LOC1     = LOC1 + 1
      LOC2     = LOC2 + 1
      LOC3     = LOC3 + 1
      LOC4     = LOC4 + 1
      A0(LOC3) = A0(LOC1)
      A0(LOC4) = A0(LOC2)
    3 CONTINUE
C
    4 LOC4     = LOC4 + 1
      LOC2     = LOC2 + 1
      A0(LOC4) = A0(LOC2)
      LTH      = I0(2001)*2 + 2
      IF(NET.EQ.0) GO TO 6
C
      NODE(2) (1:1) = 'A'
      CALL WRITE(NODE,I0(2001),LTH)
C
    8 IF(ISW1.NE.0) THEN
                    FILENM(1) = 'FLUX'
                    FILENM(2) = '    '
                    CALL WRITE(NODE,I0(2001),LTH)
                    FILENM(1) = 'MACR'
                    FILENM(2) = 'OWRK'
                    ENDIF
C
    6 NGR     = LTH/2 -    1
      ISHIFT  = NGR   + 2001
      DO 7 NG = 1,NGR
      W(NG)   = A0(NG+2001)
      E(NG)   = A0(ISHIFT+NG)
    7 CONTINUE
      E(NGR+1)= A0(ISHIFT+NGR+1)
C
      CALL ICLEA (ICARR,300,0)
      IFLAG   = 0
C     CLASSIFICATION
      DO 10 MAT=1,NMAT
C     NISO(MAT)=0 MEANS ALREADY MADE IN THE PREVIOUS CASE OR WILL BE
C                       MADE AFTER THE CELL CALCULATION
      IF(NISO(MAT).EQ.0) ICARR(MAT)=-1
   10 CONTINUE
C
      IF(NM.EQ.0)       GO TO 20
      IF(IOPT(16).EQ.0) GO TO 20
C
      VTOT       = 0.0
      DO 15 MNO  = 1,NM
      MAT        = IABS(MATD(MNO))
      IF(ICARR(MAT).EQ.-1) IFLAG=1
      ICARR(MAT) = 1
      VTOT       = VTOT+VM(MNO)
   15 CONTINUE
   20 CONTINUE
C
C === LOOP OF MATERIALS
C
      MTYPE    = 0
C
      DO 25 MAT = 1,NMAT
      IF(ICARR(MAT).EQ.-1) GO TO 25
C
      NODE(1)   = MATNM(1,MAT)
      NODE(2)   = MATNM(2,MAT)
C === TEST FAST CROSS SECTION
      NODE(2) (1:1) = 'F'
      NODE(2) (4:4) = '2'
      CALL SEARCH(NODE,LENG(1,MAT),ISW)
      NODE(2) (4:4) = '4'
      CALL SEARCH(NODE,LENG(2,MAT),ISW)
      NODE(2) (4:4) = '3'
      CALL SEARCH(NODE,LENG(3,MAT),ISW)
C === TEST THERMAL CROSS SECTION
      NODE(2) (1:1) = 'T'
      NODE(2) (4:4) = '2'
      CALL SEARCH(NODE,LENG(4,MAT),ISW)
      NODE(2) (4:4) = '4'
      CALL SEARCH(NODE,LENG(5,MAT),ISW)
      NODE(2) (4:4) = '3'
      CALL SEARCH(NODE,LENG(6,MAT),ISW)
C === TEST WHOLE ENERGY CROSS SECTION
      NODE(2) (1:1) = 'A'
      NODE(2) (4:4) = '2'
      CALL SEARCH(NODE,LENG(7,MAT),ISW)
      NODE(2)(4:4)  = '0'
      CALL SEARCH(NODE,LENG(8,MAT),ISW)
C === NONE OF TR,P0  SKIP
      NODE(2) (1:1) = 'F'
      NODE(2) (4:4) = '4'
      IF(LENG(1,MAT)+LENG(2,MAT)+LENG(7,MAT).EQ.0)         GO TO 1100
C ===  IF TR EXISTS COMPLETE SET ASSUMED
      IF(LENG(1,MAT)+LENG(7,MAT).GT.0)                     GO TO   25
      IF(IOPT(16).GT.0.AND.ICARR(MAT).EQ.1.AND.IFLAG.EQ.0) GO TO   25
      IF(IOPT(16).EQ.3) THEN
                        FILENM(1) = 'FLUX'
                        FILENM(2) = '    '
                        NODE(2)(4:4) = '2'
                        CALL SEARCH(NODE,NWORD,ISW)
                        IF(ISW.EQ.1) THEN
                               WRITE(NOUT1,23) NODE,FILENM(1),FILENM(2)
                                     GO TO 25
                                     ENDIF
                        CALL READ(NODE,F0,NWORD)
                        NODE(2)(4:4) = '3'
                        CALL SEARCH(NODE,NWORD,ISW)
                        IF(ISW.EQ.1) THEN
                               WRITE(NOUT1,23) NODE,FILENM(1),FILENM(2)
                                     GO TO 25
                                     ENDIF
                        CALL READ(NODE,F1,NWORD)
                        WRITE(NOUT1,26) NODE,NODE
                        ENDIF
C
      CALL P1B1(MATNM(1,MAT),F0,F1,LENG(1,MAT),E,W,MTYPE)
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
   25 CONTINUE
C
      IF(NM.EQ.0)       RETURN
      IF(IFLAG.EQ.1)    RETURN
      IF(IOPT(16).EQ.0) RETURN
      IF(IOPT(16).EQ.3) RETURN
      IF(NM.GT.100)     GO TO 1300
C
C=====TRANSPORT CORRECTED BY MATERIAL IF IOPT(16)=0
C=====IFLAG=1 MEANS THAT ANY OF MIXTURE HAS NO CONSTITUENT NUCLIDE
C     MIXING BY VOLUME WEIGHT
C
      IFLSW     = 1
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
C
C === CALCULATES CALL AVERAGE MACROSCOPIC X-SECTION
C
      DO 620 IRANG = 1,2
      NG           = IOPT(IRANG+54)
      NGFT(IRANG)  = NG
      DO 620 IPL   = 1,2
C === CLEAR ARRAY
      CALL CLEA ( SIGACT ,  963 , 0.0 )
      CALL CLEA ( POW    ,  100 , 0.0 )
      CALL CLEA ( XI     ,10700 , 0.0 )
      CALL CLEA ( SIGS   ,11449 , 0.0 )
      ICOUNT       = 0
      DO 300   IM  = 1,NM
      MATN         = IABS(MATD(IM))
      LTH          = 0
      NODE(1)      = MATNM(1,MATN)
      NODE(2)      = MATNM(2,MATN)
      NODE(2) (1:1)= RANGE(IRANG) (1:1)
      NODE(2) (4:4)= NUMB(5-IPL)  (4:4)
      IPP          = 3*IRANG + IPL - 2
C
      IF(LENG(IPP,MATN).GT.0)     GO TO   60
      IF(IPL.EQ.2)                GO TO  300
      NODE(2) (4:4)= '2'
      IPP          =  3*IRANG - 2
      IF(LENG(IPP,MATN).GT.0)     GO TO   60
      GO TO 1100
C
   60 LTH           = LENG(IPP,MATN)
      CALL READ(NODE,AA,LTH)
      IF(NODE(2)(4:4).NE.'2') ICOUNT=ICOUNT+1
      FF            = VM(IM)
      POW(IM)       = 0.0
      K             = 0
      DO 200 N  = 1,NG
      SIGACT(N) = SIGACT(N) + AA(K+3)*FF
      SIGF  (N) = SIGF  (N) + AA(K+4)*FF
      XNSIGF(N) = XNSIGF(N) + AA(K+5)*FF
      SIGT  (N) = SIGT  (N) + AA(K+6)*FF
      POW  (IM) = POW(IM)   + AA(K+5)*FF
      XI (N,IM) = AA(K+7)
      IF (AA(K+8).NE.0.0)
     *D1    (N) = D1(N)     + FF/AA(K+8)
      IF (AA(K+9).NE.0.0)
     *D2    (N) = D2(N)     + FF/AA(K+9)
      SIGA  (N) = SIGA  (N) + AA(K+10)*FF
      N1        = N - II(K+1) + 1
      N2        = N + II(K+2) - II(K+1)
      K         = K + 10
      DO 190 ND = N1,N2
      K         = K +  1
      IF(ND.LT.1) GO TO 185
      SIGS(N,ND)= SIGS(N,ND) + AA(K)*FF
      GO TO 190
  185 UPSCAT(N) = UPSCAT(N)  + AA(K)*FF
  190 CONTINUE
  200 CONTINUE
  300 CONTINUE
C
      IF(ICOUNT.EQ.0)   GO TO 620
C
      POWER     = 0.0
      DO 400 IN = 1,NM
      DO 350 N  = 1,NG
  350 XX(N)     = XX(N) + XI(N,IN)*POW(IN)
  400 POWER     = POWER + POW(IN)
CADD
      RPOWER    = 1.000
      IF(POWER.GT.0.0)  RPOWER = 1.0000/POWER
CEND
      DO 500 N  = 1,NG
      SIGACT(N) = SIGACT(N)/VTOT
      SIGF  (N) = SIGF  (N)/VTOT
      XNSIGF(N) = XNSIGF(N)/VTOT
      SIGT  (N) = SIGT  (N)/VTOT
      UPSCAT(N) = UPSCAT(N)/VTOT
      IF(D1(N).NE.0)
     *D1(N)     =   VTOT/D1(N)
      IF(D2(N).NE.0)
     *D2(N)     =   VTOT/D2(N)
      SIGA  (N) = SIGA  (N)/VTOT
CDEL  IF(POWER.NE.0.)
CDEL *XX(N)     = XX(N)/POWER
      XX(N)     = XX(N)*RPOWER
      DO 490 ND = 1,107
      SIGS(N,ND)= SIGS(N,ND)/VTOT
  490 CONTINUE
  500 CONTINUE
C === STORE X-SECTION INTO PACKED FORM ===
      K        = 0
      DO 600 N = 1,NG
      AA(K+3)  = SIGACT(N)
      AA(K+4)  = SIGF  (N)
      AA(K+5)  = XNSIGF(N)
      AA(K+6)  = SIGT  (N)
      AA(K+7)  = XX    (N)
      AA(K+8)  = D1    (N)
      AA(K+9)  = D2    (N)
      AA(K+10) = SIGA  (N)
C === FIND VECTOR LENGTH
      IFUP     = 0
      IF(UPSCAT(N).NE.0.) GO TO 525
      DO 520 L = 1,N
      IF(SIGS(N,L).NE.0.) GO TO 530
  520 CONTINUE
      L        = N
      GO TO 530
  525 AA(K+11) = UPSCAT(N)
      IFUP     = 1
      L        = 1
  530 II(K+1)  = N-L+1+IFUP
      DO 540 LD= N,107
      LA       = 107 - LD + N
      IF(SIGS(N,LA).NE.0.) GO TO 550
  540 CONTINUE
  550 II(K+2)  =  LA - L + 1 + IFUP
C === STORE VECTOR ===
      K        =  K + 10 + IFUP
      DO 580 LD= L,LA
      K        = K + 1
      AA(K)    = SIGS(N,LD)
  580 CONTINUE
  600 CONTINUE
C ===
      LTH      = K
      NODE(1)  = CASENM(1)
      NODE(2)  = 'X01X'
      NODE(2) (1:1) = RANGE(IRANG) (1:1)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
                        NODE(2) (4:4) = NUMB(5-IPL   ) (4:4)
      CALL WRITE ( NODE , AA , LTH )
C     NMAT1 POSITION FOR CASE AVERAGE
      LENG(3*IRANG+IPL-2,NMAT1) = LTH
  620 CONTINUE
C
C     CALCULATE NEUTRON SPECTRUM & TRANPORT CORRECTED X-SECTION
C
      MTYPE     = 1
      NODE(1)   = CASENM(1)
      NODE(2)   = 'F01X'
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
      CALL P1B1(NODE,F0,F1,LENG(1,NMAT1),E,W,MTYPE)
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
C
      DO 800 IM = 1,NM
      MATN      = IABS(MATD(IM))
      CALL CLEA (SIGT,107,0.0)
      NODE(1)   = MATNM(1,MATN)
      NODE(2)   = MATNM(2,MATN)
      NODE(2) (4:4 ) = '3'
      NGFST     = 0
C
      DO 650 IRANG = 1,2
      NG           = NGFT(IRANG)
C ==  READ P1 COMP. OF MATN
      NODE(2) (1:1) = RANGE(IRANG) (1:1)
      LTH           = LENG(3*IRANG,MATN)
      IF(LTH.EQ.0) GO TO 645
      CALL READ(NODE,AA,LTH)
      K        = 0
      DO 640 N = 1,NG
      N1       = N - II(K+1) + 1
      N2       = N + II(K+2) - II(K+1)
      K        = K + 10
      DO 630 ND= N1,N2
      K        = K + 1
      NDD      = ND
CTSUCHIHASHI---- 11/30/1989 --------------------------------------------
CDEL  IF(IRANG.EQ.2) NDD = N
CDEL  IF(ND.GT.NG)   GO TO 630
CEND--------------------------------------------------------------------
      SIGT(NDD+NGFST) = SIGT(NDD+NGFST) + AA(K)*F1(N+NGFST)*0.3333333333
  630 CONTINUE
  640 CONTINUE
  645 IF(IOPT(4).EQ.0) GO TO 660
      NGFST    = NGFT(IRANG)
  650 CONTINUE
C
  660 NGFST        = 0
      DO 750 IRANG = 1,2
C === IF TR COMP EXISTS
      IF(LENG(3*IRANG-2,MATN).GT.0) GO TO 740
      NODE(2) (1:1) = RANGE(IRANG) (1:1)
      NODE(2) (4:4) = '4'
C === IF NO P1 COMP. EXISTS
      IF(LENG(3*IRANG,MATN).EQ.0)   GO TO 730
C === READ P0 COMPONENT OF MATN
      LTH           = LENG(3*IRANG-1,MATN)
      CALL READ(NODE,AA,LTH)
      NG            = NGFT(IRANG)
      DO 670  N     = 1,NG
      SIGT(N+NGFST) = SIGT(N+NGFST)/F1(N+NGFST)
  670 CONTINUE
      K             = 0
      DO 700  N     = 1,NG
      AA(K+6)       = AA(K+6)-SIGT(N+NGFST)
      AA(K+8)       = 0.333333/AA(K+6)
      K1            = K + II(K+1) + 10
      AA(K1)        = AA(K1) - SIGT(N+NGFST)
      K             = K + II(K+2) + 10
  700 CONTINUE
C ==  WRITE TRANSPORT CORRECTED PO COMPONENT
      NODE(2) (4:4) =  '2'
      CALL WRITE(NODE,AA,LTH)
      LENG(3*IRANG-2,MATN) = LTH
      GO TO 740
C
  730 IF(LENG(3*IRANG-2,MATN).GT.0) GO TO 740
C=====TRANSPORT CORRECTED P0 ALREADY EXISTS
C=====RENAME CONSISTENT P0 INTO TRANSPORT CORRECTED P0
      NODE2(1) = NODE(1)
      NODE2(2) = NODE(2)
      NODE2(2) (4:4) =  '2'
CKSK  CALL RENAME(NODE,NODE2)
CKSK  RENAME IS BUILTIN FUNCTION IN G77
      CALL RINAME(NODE,NODE2)
      LENG(3*IRANG-2,MATN) = LENG(3*IRANG-1,MATN)
      LENG(3*IRANG-1,MATN) = 0
  740 IF(IOPT(4) .EQ. 0) GO TO 800
      NGFST = NGFT(IRANG)
  750 CONTINUE
  800 CONTINUE
C
C     END OF PROCESS
C
      RETURN
C
  900 WRITE(NOUT1,901)
      RETURN
C
 1100 WRITE(NOUT1,1110) NODE
      STOP
C
 1200 WRITE(NOUT1,1210) NMAT1
      STOP
C
 1300 WRITE(NOUT1,1310) NM
      STOP
C
   23 FORMAT(' *** MEMBER ',2A4,' NOT FOUND IN ',2A4,' FILE *** PROCESS
     @CONTINUED AS IF IC16=0')
   26 FORMAT(' *** MEMBERS ',A4,A3,'4 AND ',2A4,' IN FLUX  FILE WILL BE
     @ USED TO REDUCE TRANSPORT X-SECTIONS')
  901 FORMAT(1H0,10X,'*** GAM STEP SKIPPED '
     *  ,'BECAUSE OF NO COMPOSED MATERIAL')
 1110 FORMAT(1H0,10X,'MEMBER **',2A4,'** NOT FOUND IN MACROWRK FILE'
     *          ,' : STOP IN GAM STEP')
 1210 FORMAT(1H0,10X,' WORK DIMENSION  IS OVERFLOW (NMAT1=',I3,') '
     *          ,' : STOP IN GAM STEP BECAUSE OF PROGRAMMING ERROR ]]')
 1310 FORMAT(1H0,10X,' WORK DIMENSION  IS OVERFLOW (NM=',I3,') '
     *          ,' : STOP IN GAM STEP BECAUSE OF PROGRAMMING ERROR ]]')
C
      END

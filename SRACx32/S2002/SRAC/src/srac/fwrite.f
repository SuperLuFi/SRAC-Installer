      SUBROUTINE FWRITE(IOPT,NTL,NFL,NR,NRR,IRR,VOL)
C***********************************************************************
C                              FWRITE
C***********************************************************************
C     SUBPROGRAM READ FLUXES FROM F33 OR CALC V(I)*W(G) THEN STORE THEM
C      INTO FLUX FILE BY R-REGION (ZONE) BY E-GROUP
C***********************************************************************
C     AUG.16,1993 MODIFIED TO OVERWRITE MEMBER 'CASE'XVOL (FIND 'AKIE')
C***********************************************************************
C
C
C     RANGE ='FAST','THERMAL' AND 'ALL'
C
C     CASE1  TRANSPORT          NTL =NG      NFL =NG
C     CASE2  TRANSPORT + CALC   NTL =NGN     NFL =NG
C     CASE3  CALC               NTL =0       NFL =NG
C     INPUT OPTION
C     IOPT=1  PIJ  'FAST'  BY R-REGION 'THERMAL' BY T-REGION
C     IOPT=2  ANISN        BY MESH
C     IOPT=3  TWOTRAN      BY MESH
C     IOPT=4  TUD          BY MESH
C
C     FINAL OUT INTO FLUX FILE  BY R-REGION(ZONE) MULTIPLED BY VOLUME
C              FLUX AND VOL
C     MODIFIED TO WRITE CASESVOL T-REGION VOLUMES
C
      CHARACTER*4     FILENM,NODE,RANGE,NUMB,ICF,CASENM
C
      COMMON /MAINC/  IDUM(52)
     &               ,DUM1(4),NERF,NERT,NMAT,DUM2(4),NOUT1,NOUT2,DUM3(6)
     &               ,NEF3,DUM4(28),CASENM(2),DUM5(398)
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /WORK/   FLUXI(50000),FLUXO(50000),VOLR(500),WT(200)
C
      DIMENSION       IRR(NR),VOL(NR),RANGE(3),NODE(2)
C
      EQUIVALENCE    (DUM4(26),IRANG),(NFOUT,DUM4(4))
      EQUIVALENCE    (IBNSTP,DUM4(7)),(DUM4(27),ICF)
C
      DATA RANGE/'FAST','THER','ALL '/
C
C === READ FIXED SPECTRUM IN THE LIBRARY
C
      IF(IOPT.EQ.4  )   GO TO 900
C
      CALL CLEA(  VOLR , 500 , 0.0 )
      CALL CLEA(  WT   , 200 , 0.0 )
      NG        = MAX0(NTL,NFL)
      IF(NFL .EQ.NTL)   GO TO 100
C
      IFLSW     = 1
      FILENM(1) = 'MACR'
      FILENM(2) = 'O   '
      IF(ICF.EQ.'0002') FILENM(2) = 'OWRK'
      NODE(1)   = 'CONT'
      NODE(2)   = 'X00X'
      CALL PACK (NODE(2),4,ICF)
      CALL PACKX(NODE(2),1,RANGE(IRANG+1),1)
      NGL       = 0
      CALL READ (NODE,NGL      ,1    )
      CALL READ (NODE,WT       ,NGL+1)
C
*     WRITE(6,*) ' ** NGL WT ** ',NGL,(WT(I),I=2,NGL+1)
C
C === VOLR VOLUME OF R-REGIONS (ZONE)
C
  100 CONTINUE
      LENG     = NR*NG
      IF(LENG.GT.50000) GO TO 901
C
      CALL CLEA ( FLUXI , LENG  , 0.0 )
      CALL CLEA ( FLUXO , LENG  , 0.0 )
C
      DO 120 I = 1,NR
      J        = IRR(I)
      VOLR(J)  = VOLR(J) + VOL(I)
  120 CONTINUE
C
  130 KD       = 0
      IF(NTL .EQ. 0) GO TO 210
C
C === READ IN FLUXES FROM F33
C
      IJG      = NR*NTL
      REWIND  NFOUT
      READ(NFOUT)  (FLUXI(I),I=1,IJG)
C
      K         = 0
      DO 200  N = 1,NTL
      DO 200  I = 1,NR
      J         = IRR(I)
      K         = K + 1
      KD        = NRR*(N-1) + J
      VOLL      = 1.00
      IF(IOPT.NE.1) VOLL = VOL(I)
      FLUXO(KD) = FLUXO(KD) + FLUXI(K)*VOLL
  200 CONTINUE
      KD        = NTL*NRR
C
C === STORE ASYMPTOTIC FLUX ===
C
  210 IF(NFL.EQ.NTL) GO TO 400
      NFF       = NTL + 1
      DO  300 N = NFF,NFL
      DO  300 J =   1,NRR
      KD        = KD  + 1
      FLUXO(KD) = VOLR(J)*WT(N+1)
  300 CONTINUE
  400 CONTINUE
C
C === STORE FLUXES INTO DATAPOOL ===
C
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
      NODE(1)   = CASENM(1)
      NODE(2)   = 'X00X'
      CALL PACK(NODE(2),4,ICF)
      IF(IBNSTP.GT.0)    CALL PACK (NODE(2),2,NUMB(IBNSTP))
      CALL PACKX(NODE(2),1,RANGE(IRANG+1),1)
      CALL OVRWRT(NODE,FLUXO,KD)
C
      NODE(2)   = 'XVOL'
      CALL PACKX(NODE(2),1,RANGE(IRANG+1),1)
C(AUG.16,1993) AKIE(BEGIN)
C     CALL SEARCH(NODE,LTH,ISW)
C     IF(ISW.EQ.0)  GO TO 900
C     CALL WRITE(NODE,VOLR,NRR)
      CALL OVRWRT(NODE,VOLR,NRR)
C(AUG.16,'93) AKIE(END)
C
      IF(NR.EQ.NRR) GO TO 900
      IF(IOPT.NE.1) GO TO 900
C
      NODE(2)(1:1) = 'S'
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.0 ) GO TO 900
      CALL WRITE(NODE,VOL,NR)
C
C *** END OF PROCESS
C
  900 RETURN
C
C *** ERROR MESSAGE
C
  901 WRITE(NOUT1,902) NR,NG
      WRITE(NOUT2,902) NR,NG
      STOP
C
  902 FORMAT(1H ,' *NO OF WORK DIMENSION FOR FLUX EXCEEDS 50000 WORDS ]*
     @ '    /1H ,' * NO OF T OR R-REIGON IS ',I4,' AND ',
     @ ' NO OF ENERGY GROUPS IS ',I3,' * '/)
C
      END

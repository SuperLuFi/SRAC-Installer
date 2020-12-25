C **********************************************************************
                       SUBROUTINE PDSFLX
     1          (FLUX,VOL,KZBM,FLUXM,NM,IT,JT)
C ********************* 1987/5/20 ***FROM ITFLUX ***********************
      COMMON /MAINC/ KKKK(200)
      COMMON /TW1C/ TWRN(2000)
      COMMON /WORK/ A(132)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      DIMENSION IDCASE(2),FLUX(NM,IT,JT),NODE(2),LETR(3),
     1          NUMB(3),VOL(IT,JT),KZBM(1),FLUXM(1),IB(11)
      EQUIVALENCE(KKKK(98),IRANG),(KKKK(99),ICF),(KKKK(101),IDCASE)
     1          ,(KKKK(65),NOUT2)
      EQUIVALENCE (TWRN(164),ITFLUX),(TWRN(209),IVMESH),(TWRN(6),IGM),
     1            (TWRN(68),ITJT),(TWRN(210),IZMESH),(TWRN(170),NMPW),
     2            (TWRN(58),IMJM)
      CHARACTER *4 NUMB,LETR,IB,FILENM,NODE,IDCASE,ICF,NZERO
      DATA NUMB/'0002','0003','0004'/,NZERO/'0000'/
      DATA LETR/'FAST','THER','ALL '/
      DATA IB/'   0','   1','   2','   3','   4','   5','   6',
     1        '   7','   8','   9','   A'/
      IFLSW=1
      FILENM(1)='FLUX'
      FILENM(2)='    '
      FILENM(3)='    '
      NODE(1)=IDCASE(1)
      NODE(2)='X00X'
C
      I79=KKKK(79)
      J79=I79+1
C
      CALL PACKX(NODE(2),1,LETR(IRANG+1),1)
      CALL PACK(NODE(2),2,IB(J79))
C
      IF(NM.LE.3) GO TO 10
      WRITE(NOUT2,600) NM
      WRITE(6,600) NM
  600 FORMAT(' *** NM=',I2,2X,'---TOO LARGE---(IN PDSFLX)')
      STOP
   10 REWIND IVMESH
      REWIND IZMESH
      REWIND ITFLUX
C
      READ(IVMESH) ((VOL(I,J),I=1,IT),J=1,JT)
      READ(IZMESH) (KZBM(I),I=1,ITJT)
C
      NMGT=NM*IGM*IMJM
      CALL CLEARW(0.0,FLUXM,NMGT)
      NMG=IMJM*IGM
      DO 30 IG=1,IGM
      IG1=IG-1
C     DO 30 N=1,NM
      N=1
      N1=N-1
      READ(ITFLUX) ((FLUX(N,I,J),I=1,IT),J=1,JT)
      DO 20 J=1,JT
      JJ=(J-1)*IT
      DO 20 I=1,IT
      K=KZBM(JJ+I)
      KK=N1*NMG+IG1*IMJM+K
   20 FLUXM(KK)=FLUXM(KK)+FLUX(N,I,J)*VOL(I,J)
   30 CONTINUE
C
C =============== WRITE FLUX*VOLUME BY MATERIAL ON PDS FILE
C
      IF(ICF.NE.NZERO) GO TO 35
      CALL PACKX(NODE(2),4,NZERO,4)
      CALL OVRWRT(NODE,FLUXM(1),IMJM*IGM)
      RETURN
   35 DO 70 N=1,NM
      CALL PACKX(NODE(2),4,NUMB(N),4)
      GO TO (40,50,60) , N
   40 CALL OVRWRT(NODE,FLUXM(1),IMJM*IGM)
      GO TO 70
   50 IF(ICF.NE.NUMB(1)) GO TO 70
      CALL OVRWRT(NODE,FLUXM(IGM*IMJM+1),IMJM*IGM)
      GO TO 70
   60 IF(ICF.NE.NUMB(1)) GO TO 70
      CALL OVRWRT(NODE,FLUXM(2*IGM*IMJM+1),IMJM*IGM)
   70 CONTINUE
C
      RETURN
      END

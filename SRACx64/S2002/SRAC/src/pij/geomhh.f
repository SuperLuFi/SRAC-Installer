C     GEOMETRY #14 HEXAGONAL SHELLS IDIVP=0,1,2
C              PERMITS PIN ON OUTER BOUNDARY
      SUBROUTINE GEOMHH (RX,RPP,RDP,NPIN,THETA,IM,IP,D,NTR,NAMEP)
      COMMON / PIJ1C / NX,DUM1,NTPIN,NAPIN,DUM2(4),NDPIN,IDIVP,BETM,
     1    NX1,DUM3(4),IXP,IYP,IZP,NDPIN1,DUM4(2),LL,DUM5(5),P,RHO,BETA,
     1    EDIN,EDOUT,SINB,COSB,NZP
      COMMON /PIJ2C/ IGT,NZ,IDUM1(8),NTTAB
      EQUIVALENCE (IDUM1(7),IEDPIJ)
      COMMON /MAINC/ DUMMY1(63),NOUT1,NOUT2
      DIMENSION D(1),IM(1),IP(1),RX(1),RPP(NTPIN),RDP(1),THETA(NTPIN)
     * ,NPIN(1),NTR(NAPIN),NAMEP(2,NDPIN,NAPIN)
C
C
      DIMENSION SD(6)
      DATA C0/1.732050/
      LL=0
      IEXIT=0
      COSC=C0*COSB
      SINC=C0*SINB
      DO 99 ND=1,NX
      N=NX+2-ND
      C3 = 2. *RX(N)
C
      SD(1)=(-RHO*(SINC+COSB)+C3)/(SINB-COSC)
      SD(2)=( RHO*(SINC-COSB)-C3)/(SINB+COSC)
      SD(3)=(-RHO*COSB-RX(N))/SINB
      SD(4)=(RHO*(SINC+COSB)+C3)/(COSC-SINB)
      SD(5)=(RHO*(SINC-COSB)+C3)/(COSC+SINB)
      SD(6)=(RX(N)-RHO*COSB)/SINB
C
C
   30 DIN=AMIN1(SD(4),SD(5),SD(6))
      DOUT=SD(1)
      DO 35 I=1,3
      IF(SD(I)-DOUT) 35,34,34
   34 IJ=I
      DOUT=SD(I)
   35 CONTINUE
      IF(ND.EQ.1) IEXIT=IJ
C
      IF(DIN .GT. DOUT ) GO TO 40
      IF(ND.GT.1) GO TO 100
      IF(IXP+IYP+IZP.EQ.0) GO TO 65
      WRITE(6,*) ' *** ILLEGAL PATH CROSSING THE SECONDARY CELL ***'
      WRITE(6,*) ' SD= ',(SD(I),I=1,6)
      WRITE(6,*) ' RHO=',RHO,' ANG=',BETA*57.29578,' DEGREE EXIT='
     *           ,IEXIT
      STOP
C
   40 IZ=N+NZP-1
      CALL INSERT(DIN,IZ,IZ,DOUT,D,IM,IP)
   99 CONTINUE
  100 CONTINUE
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  1LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IEXIT,'IP=',(IP(L),L=2,LL)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'        D(L)=',(D(L),L=1,LL)
*                     ENDIF
C     IF(NAPIN.EQ.0) GO TO 300
      NN=0
      DO 200 I=1,NAPIN
      IF(IDIVP.EQ.0) GO TO 101
      NTRR=NTR(I)
      IF(NTRR.LE.1) GO TO 101
           IF(NTRR.LE.NX) THEN
C
C **  SCAN THE CROSS POINTS WITH I-TH HEXAGON
C     DCP : POSITIVE
C     DCM : NEGATIVE
      NOBJ=NTRR+NZP
      LM=0
      LP=0
      DO 105 L=2,LL-1
      IF(IM(L-1).EQ.NOBJ .AND. IM(L).EQ.NOBJ-1) LM=L
      IF(IM(L-1).EQ.NOBJ-1 .AND. IM(L).EQ.NOBJ) LP=L
  105 CONTINUE
      IF(LM.GE.LP) GO TO 101
           DCP=D(LM)
           DCM=D(LP)
      GO TO 102
                          ELSE
           DCP=D(1)
           DCM=D(LL)
                          ENDIF
  101 NTRR=0
  102 CONTINUE
      JMAX=NPIN(I)
      DO 170 J=1,JMAX
      NN=NN+1
      TH=THETA(NN)
      ANG=THETA(NN)-BETA
C     IF(ABS(ANG).GT.6.29)
C    *WRITE(6,*) I,J,NN,RPP(NN),THETA(NN),RHO,BETA
C     IF(IEDPIJ.GE.3)
C    *WRITE(6,*) '   I=',I,' J=',J,' NN=',NN,' RPP=',RPP(NN),
C    *           'THETA=',THETA(NN),' RHO=',RHO,' BETA=',BETA
      DSQ=(RPP(NN)*SIN(ANG)+RHO)**2
      TM1=-RPP(NN)*COS(ANG)
      N=NDPIN1
  103 DET=DSQ-RDP(N)**2
      IF( DET.GE.0.) GO TO 170
      TM2 = SQRT(-DET)
      DSP = TM1 + TM2
      DSM = TM1 - TM2
      N=N-1
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,9010) NAMEP(1,N,I),NAMEP(2,N,I),N,I
*9010 FORMAT(1H ,13HNAMEP(1,N,I)=,I5,2X,13HNAMEP(2,N,I)=,I5
*    1     ,2X,2HN=,I5,2X,2HI=,I5)
*     WRITE(6,*) J,NN,' NTRR=',NTRR,' DCP,DCM=',DCP,DCM,
*    * ' DSP,DSM=',DSP,DSM,' SIN COS=',SIN(ANG),COS(ANG),
*    * ' DSQ,TM1=',DSQ,TM1,' RPP,THETA=',RPP(NN),THETA(NN)
*                     END IF
      IF(NTRR.EQ.0) GO TO 140
      IF(DSP.GT.DCP) GO TO 110
      IF(DCM.LT.DSM) GO TO 150
      IF(DCM.LT.DSP) GO TO 160
      GO TO 140
  110 IF(DSM.LT.DCM) GO TO 130
      IF(DSM.GT.DCP) GO TO 140
C
C                                       IDVP=2     IDVP=1
C   0    (DSP  DSM)                      2 2        1 1     140
C   1     DCP (DSP  DSM) DCM             1 1        1 1     150
C   2     DCP (DSP  DCM  DSM)            1 2        1 1     160
C   3     DCP  DCM (DSP  DSM)            2 2        1 1     140
C   4    (DSP (DCP  DCM) DSM)          2 1 1 2    1 1 1 1   130 140
C   5    (DSP  DCP  DSM) DCM             2 1        1 1     120
C   6    (DSP  DSM) DCP  DCM             2 2        1 1     140
C   A    (DSP 2  2  DSM)      140
C   B    (DSP 1  1  DSM)      150
C   C    (DSP 1  2  DSM)      160
C   D    (DCP 1  1  DCM)      130
C   E    (DSP 2  1  DSM)      120
  120 CALL INSERT(DSP,NAMEP(2,N,I),NAMEP(1,N,I),DSM,D,IM,IP)
      GO TO 165
  130 CALL INSERT(DCP,NAMEP(1,N,I),NAMEP(1,N,I),DCM,D,IM,IP)
  140 CALL INSERT(DSP,NAMEP(2,N,I),NAMEP(2,N,I),DSM,D,IM,IP)
      GO TO 165
  150 CALL INSERT(DSP,NAMEP(1,N,I),NAMEP(1,N,I),DSM,D,IM,IP)
      GO TO 165
  160 CALL INSERT(DSP,NAMEP(1,N,I),NAMEP(2,N,I),DSM,D,IM,IP)
  165 CONTINUE
      IF(N.GT.1) GO TO 103
  170 CONTINUE
  200 CONTINUE
  300 LL=LL-1
      IF(IEDPIJ.GE.3) THEN
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  3LL=',LL,'IM=',(IM(L),L=1,LL)
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  IJK=',IEXIT,'IP=',(IP(L),L=2,LL+1)
      WRITE(6,'(A,10F12.5/(11X,10F12.5))')
     &'      D(L)=',(D(L),L=1,LL+1)
                      ENDIF
      DO 310 L=1,LL
      D(L)=D(L)-D(L+1)
  310 CONTINUE
      IF(IEDPIJ.GE.3) THEN
      WRITE(6,'(A,10F12.5/(11X,10F12.5))')
     &'      D(L)=',(D(L),L=1,LL)
                      ENDIF
      IF(LL.GT.NTTAB) GO TO 999
C
   60 CONTINUE
      GO TO (61,62,63),IEXIT
      WRITE(6,*) ' *** ILLEGAL PATH CROSSING THE SECONDARY CELL ***'
      WRITE(6,*) ' SD= ',(SD(I),I=1,6)
      WRITE(6,*) ' RHO=',RHO,' ANG=',BETA*57.29578,' DEGREE EXIT='
     *           ,IEXIT
      STOP
   61 RHO=-P*(SINC+COSB)+RHO
      IXP=IXP+1
      GO TO 65
   62 RHO=-P*(SINC-COSB)+RHO
      IYP=IYP+1
      GO TO 65
   63 RHO=RHO+2.*P*COSB
      IZP=IZP+1
C
   65 CONTINUE
      RETURN
  999 WRITE(NOUT1,990) LL,NTTAB
  990 FORMAT(' *** PATH TABLE LENGTH EXCEEDS NTTAB ',2I10)
      STOP
      END

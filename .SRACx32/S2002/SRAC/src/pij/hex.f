      SUBROUTINE HEX (RX,D,IM,IP)
      COMMON / PIJ1C / NX,DUM(15),IXP,IYP,IZP,I20,I21,I22,LL,
     1                 DUM24(4),IC,P,RO,ALP,I32,I33,C1,C2,LC
      COMMON /MAINC / DUMMY1(63),NOUT1,NOUT2,DUMMY2(435)
C
      DIMENSION D(1),IM(1),IP(1),RX(1)
C
      DIMENSION SD(6)
      DATA C0/1.732050/,P3/1.047198/,P6/0.523599/
      LL=0
      C3=2.*P
      CS=C0*C1
      CC=C0*C2
C
      SD(1)=(-RO*(CC+C1)+C3)/(C2-CS)
      SD(2)=( RO*(CC-C1)-C3)/(C2+CS)
      SD(3)=(-RO*C1-P)/C2
      SD(4)=(RO*(CC+C1)+C3)/(CS-C2)
      SD(5)=(RO*(CC-C1)+C3)/(CS+C2)
      SD(6)=(P-RO*C1)/C2
C
      IF(ALP.GT.0.0.AND.ALP.LE.P6) GO TO 20
      IF(ALP.GT.P6.AND.ALP.LE.P3) GO TO 30
      WRITE(NOUT2,10) ALP
CKSK
*  10 FORMAT(1H1 ///10X 11H*****  ALP= 1PE11.4, 6H ***** )
   10 FORMAT(1H1,///10X,11H*****  ALP=,1PE11.4, 6H ***** )
      STOP
C
   20 DIN=AMIN1(SD(1),SD(5),SD(6))
      DOUT=SD(2)
      DO 25 I=2,4
      IF(SD(I)-DOUT) 25,24,24
   24 IJ=I
      DOUT=SD(I)
   25 CONTINUE
      GO TO 39
C
   30 DIN=AMIN1(SD(4),SD(5),SD(6))
      DOUT=SD(1)
      DO 35 I=1,3
      IF(SD(I)-DOUT) 35,34,34
   34 IJ=I
      DOUT=SD(I)
   35 CONTINUE
C
   39 IF(DIN .LT. DOUT ) RETURN
   40 CALL INSERT(DIN,NX,NX,DOUT,D,IM,IP)
      N=NX-1
   50 DD=RX(N+1)**2-RO**2
      IF(DD.LE.0.0) GO TO 60
      SDP=SQRT(DD)
      SDM=-SDP
      CALL INSERT(SDP,N,N,SDM,D,IM,IP)
      N=N-1
      IF(N.LE.0) GO TO 60
      GO TO 50
C
   60 CONTINUE
      GO TO (61,62,63,64),IJ
   61 RO=-P*(CC+C1)+RO
      IXP=IXP+1
      GO TO 65
   62 RO=-P*(CC-C1)+RO
      IYP=IYP+1
      GO TO 65
   63 RO=RO+C3*C1
      IZP=IZP+1
      GO TO 65
   64 RO =P*(CC+C1)+RO
C
   65 CONTINUE
      RETURN
      END

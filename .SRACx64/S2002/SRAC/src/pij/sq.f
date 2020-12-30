C             SQ                  LEVEL=4        DATE=84.02.06
      SUBROUTINE SQ  (RX,D,IM,IP)
      COMMON / PIJ1C / NX,DUM(15),IXP,IYP,I19(4),
     &                 LL,DUM24(4),IC,P,RO,ALP,DIN,DOUT,C2,C1,LC
      DIMENSION D(1),IM(1),IP(1),RX(1)
C
C
      LL=0
      SD1=-(P-RO*C1)/C2
      SD2=-(P+RO*C2)/C1
      SD3=(P+RO*C1)/C2
      SD4=(P-RO*C2)/C1
      DIN=AMIN1(SD3,SD4)
      DOUT=AMAX1(SD1,SD2)
C
      IF(DIN .LT. DOUT ) RETURN
      CALL INSERT(DIN,NX,NX,DOUT,D,IM,IP)
C
      N=NX-1
   10 DD=RX(N+1)**2-RO**2
      IF(DD.LE.0.0) GO TO 20
      SDP=SQRT(DD)
      SDM=-SDP
      CALL INSERT(SDP,N,N,SDM,D,IM,IP)
      N=N-1
      IF(N.LE.0) GO TO 20
      GO TO 10
C
   20 CONTINUE
      IF(SD2.LT.SD1) GO TO 30
      RO=RO+2.*P*C2
      IXP=IXP+1
      RETURN
   30 RO=RO-2.*P*C1
      IYP=IYP+1
      RETURN
      END

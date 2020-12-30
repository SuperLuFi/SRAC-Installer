C             SQ2                 LEVEL=3        DATE=84.02.06
      SUBROUTINE SQ2 (RX,D,IM,IP)
      COMMON / PIJ1C / NX,DUM(15),IXP,IYP,I19(4),LL,I24(4),
     &                 IC,P,RO,ALP,DIN,DOUT,C2,C1,LC
      DIMENSION D(1),IM(1),IP(1),RX(1)
C
C
      DIMENSION DR(4)
      DATA PAI/3.1415926/
      LL=0
 9000 FORMAT(1H ,5X,'IC=',I5,'RO=',E11.5,(10X,'RX(I)=',E11.5))
      SD1=-(P-RO*C1)/C2
      SD2=-(P+RO*C2)/C1
      SD3=(P+RO*C1)/C2
      SD4=(P-RO*C2)/C1
      DIN=AMIN1(SD3,SD4)
      DOUT=AMAX1(SD1,SD2)
      IF( DIN .LE. DOUT ) RETURN
C
      CALL INSERT(DIN,2*NX-1,2*NX-1,DOUT,D,IM,IP)
C
      N=NX-1
   10 DD=RX(N+1)**2-RO**2
      IF(DD.LE.0.0) GO TO 20
      SDP=SQRT(DD)
      SDM=-SDP
      CALL INSERT(SDP,2*N -1,2*N -1,SDM ,D,IM,IP)
      N=N-1
      IF(N.LE.0) GO TO 20
      GO TO 10
C
   20 DO 21 I=1,4
   21 DR(I)=RO*TAN(PAI*FLOAT(9-2*I)/8.-ALP)
C     WRITE(6,6000) LL,(D(I),IP(I),IM(I),I=1,LL)
 6000 FORMAT(1H ,10X,'LL=',I5,(/10X,'D(I)=',E12.5,2X,
     &        'IP(I)=',I5,2X,'IM(I)=',I5))
C
 9010 FORMAT(1H ,5X,'N=',I5,2X,'IC=',I5)
      IF(ALP.LT.0.375*PAI.AND.ALP.GT.0.125*PAI) GO TO 25
      CALL DIVIDE(DR(1),DR(2),1,D,IM,IP)
   24 CALL DIVIDE(DR(3),DR(4),1,D,IM,IP)
      GO TO 26
   25 CALL DIVIDE(1.E10*DR(2),DR(2),1,D,IM,IP)
      CALL DIVIDE(DR(1),1.E10*DR(1),1,D,IM,IP)
      GO TO 24
   26 CONTINUE
      IF(SD2.LT.SD1) GO TO 30
      RO=RO+2.*P*C2
      IXP=IXP+1
      RETURN
   30 RO=RO-2.*P*C1
      IYP=IYP+1
      RETURN
      END

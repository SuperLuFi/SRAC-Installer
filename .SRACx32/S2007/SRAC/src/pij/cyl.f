      SUBROUTINE CYL (RX,D,IM,IP)
      COMMON / PIJ1C / NX,DUMM(21),LL,DUM2(6),RO
      DIMENSION D(1),IM(1),IP(1),RX(1)
C
C
      LL=0
      N=NX
C
   10 DD=RX(N+1)**2-RO**2
      IF(DD.LE.0.0) GO TO 20
      SDP=SQRT(DD)
      SDM=-SDP
      CALL INSERT(SDP,N,N,SDM,D,IM,IP)
      N=N-1
      IF(N.LE.0) GO TO 20
      GO TO 10
   20 CONTINUE
      RETURN
      END

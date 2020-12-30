C             SLAB                LEVEL=4        DATE=84.02.07
      SUBROUTINE SLAB(RX,D,IM,IP)
      COMMON / PIJ1C / NX,DUM(15),IXP,I18(5),LL,I24(10),SINB,COSB,LC
      DIMENSION D(1),IM(1),IP(1),RX(1)
      LL=NX+1
      IF(COSB  .LT.0.) GO TO 2
      DO 1 N=1,LL
      J=LL+1-N
      D(N) = RX(J)
      IM(N)= J-1
    1 CONTINUE
      GO TO   4
    2 DO 3 N=1,LL
      D(N)=-RX(N)
      IM(N)= N
    3 CONTINUE
    4 IXP=IXP+1
      RETURN
      END

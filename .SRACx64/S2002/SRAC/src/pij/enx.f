      FUNCTION  ENX(M,X)
C     CALCULATION OF EIN(X) FUNCTION
      EX = EXP(-X)
      MM1=M-1
      XM=FLOAT(MM1)
      IF(X.GT.0.)GO TO 1
      ENX=1./XM
      RETURN
    1 IF(X.GE.1.)GO TO 2
      E1=-(ALOG(X)+(((((( -2.8E-5*X+2.31E-4)*X-1.667E-3)*X+1.0417E-2)*X
     &   -5.55556E-2)*X+0.25)*X-1.0)*X+0.5772157)
      IF(M.EQ.1) GO TO 3
      SUM=0.
      XKAI=1./XM
      DO 10 K=1,MM1
      SUM=SUM+XKAI
      XM=XM-1.0
      IF(XM.EQ.0.0)XM=1.0
   10 XKAI=-XKAI*X/XM
      ENX=  EX   *SUM +XKAI*E1
      RETURN
    3 ENX=E1
      RETURN
    2 AX=(((X+8.5733287)*X+18.059017)*X+8.6347609)*X+0.26777373
      BX=(((X+9.5733223)*X+25.632956)*X+21.099653)*X+3.9584969
      IF(M.EQ.1) GO TO 6
      SUM=0.
      XKAI=1./XM
      DO 20 K=1,MM1
      SUM=SUM+XKAI
      XM=XM-1.0
      IF(K.EQ.MM1)GO TO 21
   20 XKAI=-XKAI*X/XM
   21 ENX=   EX  *(SUM-XKAI*AX/BX)
      RETURN
    6 ENX=   EX  *AX/BX/X
      RETURN
      END

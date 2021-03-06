C             PCMBAL              LEVEL=1        DATE=81.11.14
      SUBROUTINE PCMBAL (SRC,FRL,FLR,FUD,FDU,ABS,IM,JM,IP,JP,IG)
C
C     PRINTS COARSE MESH BALANCE TABLES
C     IG IS GROUP INDICATOR ( 0/GROUP )
C
      COMMON /TW1C/ DD(1),LIM1,IAA(210)
      DIMENSION D(212)
      EQUIVALENCE (D(1),DD(1))
      EQUIVALENCE (D(157),NOUT)
C
      DIMENSION SRC(IM,JM),FRL(IP,JM),FLR(IP,JM),FUD(IM,JP),FDU(IM,JP),
     &ABS(IM,JM)
C
      IF (IG.NE.0) WRITE (NOUT,110)IG
      IF (IG.EQ.0) WRITE (NOUT,120)
      WRITE (NOUT,130)
      DO 100 J=1,JM
      DO 100 I=1,IM
      JA=(J-1)*IM+I
      WRITE (NOUT,140) JA,SRC(I,J),FRL(I,J),FLR(I+1,J),FUD(I,J),
     &FDU(I,J+1),ABS(I,J),FLR(I,J),FRL(I+1,J),FDU(I,J),FUD(I,J+1)
  100 CONTINUE
      RETURN
C
C
  110 FORMAT (1H0///'0COARSE MESH BALANCE TABLE FOR GROUP ',I3///)
  120 FORMAT (1H0///'0COARSE MESH BALANCE TABLE FOR ALL GROUPS'///)
  130 FORMAT ('0ZONE',2X,'SOURCE',6X,'LEFT',7X,'RIGHT',5X,'BOTTOM',7X,
     &'TOP',5X,'ABSORPTION',3X,'LEFT',7X,'RIGHT',6X,'BOTTOM',6X,'TOP'/
     &8X,4('INFLOW',5X),11X,4('OUTFLOW',4X)/)
  140 FORMAT (1H ,I5,1P10E11.4)
      END

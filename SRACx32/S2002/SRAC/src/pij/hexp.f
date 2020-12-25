C             HEXP                LEVEL=1        DATE=84.03.06
      SUBROUTINE    HEXP(X0,Y0,H,IPEN)
C****
C       HEXAGON
C****
C****  H HALF OF SIDE-TO-SIDE SPACING
C****  R LENGTH OF HEXAGON
C****
C****
      DIMENSION     X(6),Y(6)
      CALL NEWPEN(IPEN)
C     H=0.8660254*R
      R=H/0.8660254
      R05=0.5*R
      X(1)=R
      X(2)=R05
      X(3)=-R05
      Y(1)=0.
      Y(2)=H
      Y(3)=H
      DO 100 I=1,3
      X(I+3)=-X(I)
      Y(I+3)=-Y(I)
  100 CONTINUE
      X1=X0+X(1)
      Y1=Y0+Y(1)
      CALL PLOT(X1,Y1,3)
      DO 110 I=2,6
      X2=X0+X(I)
      Y2=Y0+Y(I)
C     IF(IPEN.EQ.0) CALL DASHP(X2,Y2,0.2)
      CALL PLOT (X2,Y2,2)
  110 CONTINUE
C     IF(IPEN.EQ.0) CALL DASHP(X1,Y1,0.2)
      CALL PLOT (X1,Y1,2)
      RETURN
      END

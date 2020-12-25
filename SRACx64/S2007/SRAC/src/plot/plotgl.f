C*************************************************************************
C*    DRAW LINE WITH SELECTIVE LINE TYPE.                                *
C*                                                                       *
C*        XX,YY .... THE COORDINATION OF THE DISTINATIONE.               *
C*        NN ....... =0 : POSITIONING.                                   *
C*                   1<=NN<=12 : LINE TYPE NUMBER.                       *
C*                   NN=999 : CONTINUE FROM THE OLD STATUS.              *
C*                   -1>=NN>=-12 : DUMMY MOVING(WITH PEN UP).            *
C*                   NN=1000 RETURN THE CURRENT POSITION(XX,YY).         *
C*                   NN=1001 RETURN THE DRAWN LENGTH FROM POSITIONNING   *
C*                                IN XX.                                 *
C*************************************************************************
      SUBROUTINE PLOTGL( XX, YY, NN )
C      
      DIMENSION ACLN(12,12), NCLN(12), PTRN(12)
C     
      DATA NCLN/ 1, 2, 2, 4, 6, 8, 4, 6, 8, 8, 2, 6 /
      DATA ISW, AM, AS, AS1, AL, LMAX / 0, 0.2, 1.5, 1.2, 4.0, 12 /
      DATA XOLD, YOLD, JSX, RLENG / 0.0, 0.0, 1, 1.E7 /
      DATA EPS/ 0.01 /
C      
C     ***  INTERNAL FUNCTION ALENGT(X1,Y1,X2,Y2)  ***
C      
      ALENGT( V1, W1, V2, W2 ) = SQRT( ( V2 - V1 )**2 + ( W2 - W1 )**2 )
C
      IF( NN .EQ. 999 ) THEN
         CALL PLOT( XOLD, YOLD, 3 )
         GO TO 30
      ENDIF
      IF(ISW.NE.0)GO TO 20
C      
C     ***  INITIALIZE  ***
C      
 10   CONTINUE
C
      CALL SEEHFC(F1)
      ACLN(1,1)=1.E7
      ACLN(1,2)=AS/F1
      ACLN(2,2)=AS1/F1
      ACLN(1,3)=AL/F1
      ACLN(2,3)=AS/F1
      ACLN(1,4)=AL/F1
      ACLN(2,4)=AS/F1
      ACLN(3,4)=AS1/F1
      ACLN(4,4)=AS/F1
      ACLN(1,5)=AL/F1
      ACLN(2,5)=AS1/F1
      ACLN(3,5)=AS/F1
      ACLN(4,5)=AS1/F1
      ACLN(5,5)=AS/F1
      ACLN(6,5)=AS1/F1
      ACLN(1,6)=AL/F1
      ACLN(2,6)=AS1/F1
      ACLN(3,6)=AS/F1
      ACLN(4,6)=AS1/F1
      ACLN(5,6)=AS/F1
      ACLN(6,6)=AS1/F1
      ACLN(7,6)=AS/F1
      ACLN(8,6)=AS1/F1
      ACLN(1,7)=AL/F1
      ACLN(2,7)=AS1/F1
      ACLN(3,7)=-AM/F1
      ACLN(4,7)=AS1/F1
      ACLN(1,8)=AL/F1
      ACLN(2,8)=AS1/F1
      ACLN(3,8)=-AM/F1
      ACLN(4,8)=AS1/F1
      ACLN(5,8)=-AM/F1
      ACLN(6,8)=AS1/F1
      ACLN(1,9)=AL/F1
      ACLN(2,9)=AS1/F1
      ACLN(3,9)=-AM/F1
      ACLN(4,9)=AS1/F1
      ACLN(5,9)=-AM/F1
      ACLN(6,9)=AS1/F1
      ACLN(7,9)=-AM/F1
      ACLN(8,9)=AS1/F1
      ACLN(1,10)=AL/F1
      ACLN(2,10)=AS1/F1
      ACLN(3,10)=AS/F1
      ACLN(4,10)=AS1/F1
      ACLN(5,10)=-AM/F1
      ACLN(6,10)=AS1/F1
      ACLN(7,10)=AS/F1
      ACLN(8,10)=AS1/F1
      ACLN(1,11)=-AM/F1
      ACLN(2,11)=AS1/F1
      ACLN(1,12)=-AM/F1
      ACLN(2,12)=AS1/F1
      ACLN(3,12)=-AM/F1
      ACLN(4,12)=AS1/F1
      ACLN(5,12)=-AM/F1
      ACLN(6,12)=2.*AS1/F1
      IF(ISW.EQ.-1)GO TO 1100
      ISW=1
   20 CONTINUE
      N=IABS(NN)
      K=2
      IF(NN.LE.-1)K=3
   30 CONTINUE
      X=XX
      Y=YY
      IF(N.EQ.1)GO TO 800
      IF(N.EQ.1000)GO TO 910
      IF(N.EQ.1001)GO TO 920
      IF(N.EQ.0)GO TO 900
      N=MOD(N,LMAX)
      IF(N.EQ.0)N=LMAX
C
      ALENG=ALENGT(X,Y,XOLD,YOLD)
      IF(ABS(ALENG).LT.EPS)GO TO 1000
      DX=X-XOLD
      DY=Y-YOLD
      ASINL=DY/ALENG
      ACOSL=DX/ALENG
      NC1=NCLN(N)
      XT=XOLD
      YT=YOLD
      SMA=0.
  100 AX=ACLN(JSX,N)
C
      IPEN=K
      IF(MOD(JSX,2).EQ.0)IPEN=3
      IF(AX.LT.0)GO TO 200
      IF(RLENG.LT.AX)AX=RLENG
      IF(SMA+AX .GE. ALENG) GO TO 300
C     ON THE WAY OF THE LINE
  150 XT=AX*ACOSL+XT
      YT=AX*ASINL+YT
      CALL PLOT(XT,YT,IPEN)
      JSX=JSX+1
      IF(JSX.GT.NC1)JSX=1
      RLENG=1.E7
      SMA=SMA+AX
      GO TO 100
C     POINT DRAW
  200 AX=-AX
      CALL PLOT(XT+AX,YT,K)
      CALL PLOT(XT+AX,YT+AX,K)
      CALL PLOT(XT,YT+AX,K)
      CALL PLOT(XT,YT,K)
      JSX=JSX+1
      IF(JSX.GT.NC1)JSX=1
      RLENG=1.E7
CC    SMA=SMA+AX
CC    IF (SMA.GE.ALENG)GO TO 950
      GO TO 100
C     LAST PROCESS TO (X,Y)
  300 RLENG=ABS((SMA+AX)-ALENG)
      CALL PLOT(X,Y,IPEN)
      GO TO 950
C     N=0 POSITIONNING
  900 SUML=0.
      CALL PLOT(X,Y,3)
      IPEN=2
      XOLD=X
      YOLD=Y
      ICON=0
      JSX=1
      RLENG=1. E 7
C
      GO TO 1000
C     WHERE(X,Y)
  910 XX=XOLD
      YY=YOLD
      GO TO 1000
C     HOW LENGTH FROM POSITIONNING
  920 XX=SUML
      GO TO 1000
C      REAL LINE
  800 SUML=SUML+ALENGT(X,Y,XOLD,YOLD)
      XOLD=X
      YOLD=Y
      CALL PLOT(XOLD,YOLD,K)
      GO TO 1000
C     NORMAL END PROCESS
  950 CONTINUE
      XOLD=X
      YOLD=Y
      SUML=SUML+ALENG
C      RETURN
 1000 RETURN
C
      ENTRY CHGLPT(PTRN,N1,N2)
C
C     PTRN ...... ARRAY DATA FOR THE NEW LINE PATERN.
C     N1 ........ SIZE OF PRTN ARRAY(MUST BE 1<=N1<=12).
C     N2 ........ NEW LINE TYPE NUMBER(MUST BE 12>=N2>=1).
C
      IF(ISW.NE.0)GO TO 1100
      ISW=-1
      GO TO 10
 1100 CONTINUE
      DO 1200 I=1,N1
      ACLN(I,N2)=PTRN(I)/F1
 1200 CONTINUE
      NCLN(N2)=N1
      RETURN
      END

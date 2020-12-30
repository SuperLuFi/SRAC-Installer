C***********************************************************************
      SUBROUTINE     GRIDRT     (X,Y,R,T,MODE)
C***********************************************************************
C                      CALLED BY GRVOL
C     CONVERT POSITION EXPRESSED BY CARTESIAN OR TRIAGONAL ORDINATES
C     INTO THAT BY R-THETA COORDINATES EXCEPT X-Y
C     THETA VALUE IS RETURNED IN ( 0 TO 2*PI RADIAN)
C***********************************************************************
                DATA PI/3.1415926/
C               MODE =1  X-Y        ORDINATES NO CHANGE
C                    =2  TRIAGONAL  ORDINATES TO X-Y
C                    =3  R-THETA    ORDINATES NO CHANGE
C                    =4  H-S        ORDINATES TO R-THETA
C                    =5  TRIAGONAL  ORDINATES  TO R-THETA
                    IF(MODE.EQ.1)
     *                  THEN
                        R=X
                        T=Y
                       RETURN
                        ENDIF
                    IF(MODE.EQ.2)
     *                  THEN
                     R=X-Y*0.5
                     T=Y*0.8660254
                      RETURN
                        ENDIF
                     IF(MODE.EQ.3)
     *                  THEN
                        RR=X
                        TT=Y
                      GO TO 10
                        ENDIF
                     IF(MODE.EQ.4)
     *                   THEN
                RR=X*SQRT(0.75+(Y-0.5)**2)
                TT=ATAN((Y-0.5)/0.8660254) + PI/6.
                        GO TO 10
                         ENDIF
                    IF(MODE.EQ.5)
     *                  THEN
                     XX=X-Y*0.5
                     YY=Y*0.8660254
                        ENDIF
    5           RR=SQRT(XX**2+YY**2)
                IF(YY.EQ.0.) GO TO 20
                IF(XX.EQ.0.) GO TO 30
                             TT=ATAN2(YY,XX)
   10           IF(TT.LT.0.) TT=TT+2.*PI
                        GO TO 50
   20                        TT=0.
                IF(XX.LT.0.) TT=-PI
                        GO TO 50
   30                        TT=PI/2.
                IF(YY.LT.0.) TT=1.5*PI
   50                     R=RR
                          T=TT
                IF(R.LT.1.E-4) T=0.
  100                    RETURN
                          END

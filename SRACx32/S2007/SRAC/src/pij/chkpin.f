C             CHKPIN              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       CHKPIN    : COUNTER PIN RODS ON ANNULAR CYLINDER THAT IS       *
C                             DIVIDED TWO ANGLE(THS,THE)               *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.02.04                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        CYLNUM                                                        *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          NPIN   :  NUMBER OF PIN RODS ON ANNULAR                     *
C          THETA  :  TABLE OF ANGLE OF PIN POSITION VECTOR             *
C          THS    :  START ANGLE                                       *
C          THE    :  END ANGLE                                         *
C                                                                      *
C    -- OUTPUT --                                                      *
C                                                                      *
C       (ARGUMENT)                                                     *
C          NN     :  NUMBER OF PIN RODS IN THS THROUGH THE             *
C          SETA   :  NUMBER OM PIN RODS IN DIVIDE ANGLE                *
C                                                                      *
C***********************************************************************
C
C
C
      SUBROUTINE  CHKPIN (NPIN,THETA,THS,THE,NN,SETA)
      DIMENSION  THETA(50),SETA(40)
      DATA   PAI / 3.14159265 /,   EPS / 0.03 /
C
C
C
      NN = 0
      IF ( NPIN .EQ. 0 )   GO TO 15
      DO  10  I = 1,NPIN
         IF ( THETA(I) .LT. THS-EPS  .OR.
     @        THETA(I) .GT. THE+EPS        )  GO TO 5
            NN = NN+1
            SETA(NN) = THETA(I)
            GO TO 10
    5    IF ( THETA(I)-2*PAI .LT. THS-EPS  .OR.
     @        THETA(I)-2*PAI .GT. THE+EPS )  GO TO 10
            NN = NN+1
            SETA(NN) = THETA(I)-2*PAI
   10    CONTINUE
C
C
C
C
C
   15 IF ( NN .EQ. 0 )   GO TO 50
      DO  30  I = 1,NN
         SMIN = SETA(I)
         IMIN = I
         DO  20  J = I,NN
            IF ( SETA(J) .GE. SMIN )  GO TO 20
               SMIN = SETA(J)
               IMIN = J
   20       CONTINUE
         SETA(IMIN) = SETA(I)
   30    SETA( I )  = SMIN
C
   50 RETURN
      END

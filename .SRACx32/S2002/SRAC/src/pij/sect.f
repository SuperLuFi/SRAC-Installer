C             SECT                               DATE=94.06.18
      SUBROUTINE SECT(NY,N,D,X,BETA,TY)
C     AZIMUTHAL POSIOTION N OF A POINT ON THE LINE DEFINED BY X,BETA
C     DISTANCE D FROM THE PIVOT IS DECIDED BY THIS ROUTINE
C     ARGUMENTS::::
C       NY: INPUT : NUMBER OF AZIMUTHAL DIVISION
C       N : OUTPUT: AZIMUTHAL POSITION
C       D : INPUT : SIGNED DISTANCE FROM PIVOT
C       X : INPUT : SIGNED DISTANCE BETWEEN PIVOT AND (0,0)
C       BETA: INPUT : ANGLE OF NUMERICAL INTEGRATION
C       TY : ANGLES IN RADIAN FOR AZIMUTHAL DIVISION
C ANG: TEMP : ANGLE
      DIMENSION TY(NY)
      DATA PI,PI2/  3.141593, 6.283185/
      ANG=BETA+ATAN( X/D)
      IF(D.GT.0.) ANG=ANG+PI
      IF(ANG.LT.0.)ANG=ANG+PI2
      IF(ANG.GT.PI2)ANG=ANG-PI2
C
      IF(TY(1).GT.0.) THEN
      DO 10 I=1,NY
            N=I
      IF(ANG.LT.TY(I)) GOTO 30
   10 CONTINUE
C     ANGLE REMAINING IS BETWEEN TY(NY) AND 2*PI
            N=1
                      ELSE
C TY(1)=0.
      DO 20 I=1,NY-1
             N=I
      IF(ANG.LT.TY(I+1)) GOTO 30
   20 CONTINUE
C     ANGLE REMAINING IS BETWEEN TY(NY) AND 2*PI=TY(NY+1)
             N=NY
                      ENDIF
   30   RETURN
      END

C             STORAF              LEVEL=1        DATE=81.11.14
      SUBROUTINE STORAF (AF,Q,IQ,J,IP,LT)
C
C     STORE ANGULAR FLUX IN ECS
C
      DIMENSION AF(1), Q(1)
C
      IF (IQ.EQ.0) GO TO 110
C
C     REMOVE BOUNDARY SOURCE FROM ANGULAR FLUX
C
      DO 100 I=1,LT
  100 AF(I)=AF(I)-Q(I)
C
C     STORE
C
  110 CALL RITE (0,IP+(J-1)*LT,AF,LT,1)
      IF(IQ.EQ.0) GO TO 130
C
C     RESTORE BOUNDARY SOURCE FOR CALCULATION
C
      DO 120 I=1,LT
  120 AF(I)=AF(I)+Q(I)
  130 CONTINUE
      RETURN
      END

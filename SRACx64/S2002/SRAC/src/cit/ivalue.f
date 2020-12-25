      SUBROUTINE IVALUE( A , LENG , B )
C***********************************************************************
C     IVALUE IS ORIGINALY A SERVICE ROUTINE IN FACOM
C     THIS ROUTINE IS A DUMMY PROGRAM TO EXPORT SRAC & COREBN
C     DO 100 I=1,LENG
C       A(I)=B
C 100 CONTINUE
C     THIS SUBROUTINE IS REPLACED BY CLEA ROUTINE, WHICH HAS
C     A SAME FUNCTION (CLEA MAY BE VECTORIZED).
C***********************************************************************
      DIMENSION A(1)
C
      CALL CLEA (A,LENG,B)
      RETURN
      END

C             WOT8                LEVEL=4        DATE=84.10.12
      SUBROUTINE WOT8(L1,M1,L2,M2,L3,M3,L4,M4,L5,M5,L6,M6,L7,M7,L8,M8,
     &         NOU)
C   *** WOT8  WILL WRITE UP TO EIGHT ONE-DIMENSIONAL ARRAYS OF VARYING
C       LENGTHS IN COLUMNS ACROSS A PAGE. L1,L2,ETC. ARE THE POSITIONS
C       IN COMMON OF THE BASE ADDRESSES OF THE ARRAYS TO BE WRITTEN.
C       IF ANY LN IS ZERO THE N-TH COLUMN WILL BE BLANK.
      COMMON /SKPBUF/ LOCATN,JUMP,LFAST,LSLOW,JMP1,JMP2,JMP3
      COMMON/SN1C/  D(1)
      CHARACTER * 13 Z(8)
      DIMENSION L(8),M(8),LD(1)
      EQUIVALENCE (D(1),LD(1))
      DATA K/100000/
      L(1)=L1
      L(2)=L2
      L(3)=L3
      L(4)=L4
      L(5)=L5
      L(6)=L6
      L(7)=L7
      L(8)=L8
      M(1)=M1
      M(2)=M2
      M(3)=M3
      M(4)=M4
      M(5)=M5
      M(6)=M6
      M(7)=M7
      M(8)=M8
      N=0
      DO 1 I=1,8
      LL=L(I)
      IF(LD(LL).EQ.0) M(I)=0
    5 IF(LL.EQ.0)M(I)=0
    1 N=MAX0(N,M(I))
      DO 2 I=1,N
      DO 3 J=1,8
      IF(M(J).GE.I) GO TO 4
C     ENCODE(13,100,Z(1,J))
      WRITE(Z(J),100)
      GO TO 3
    4 LL=L(J)
      LL=LD(LL) + I - 1
      IF(K.GT.IABS(LD(LL))) GO TO 31
C     ENCODE(13,110,Z(1,J)) D(LL)
      WRITE(Z(J),110) D(LL)
      GO TO 3
   31 CONTINUE
C     ENCODE(13,120,Z(1,J)) LD(LL)
      WRITE(Z(J),120) LD(LL)
    3 CONTINUE
    2 WRITE(NOU,10) I,Z
      RETURN
   10 FORMAT(I6,8A13)
  100 FORMAT('             ')
  110 FORMAT(1PE13.5)
  120 FORMAT(I8,'     ')
      END

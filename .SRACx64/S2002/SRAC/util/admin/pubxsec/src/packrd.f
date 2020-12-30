C***********************************************************************
      SUBROUTINE PACKRD( LENG, A4CHR, CWORK )
C***********************************************************************
C     READ CHARACTER VARIABLE(A4) WRITTEN BY 'PACKWT' FROM 1D-ARRAY
C     DEPENDING ON 64-BIT(IBIT64=1) OR 32-BIT(IBIT64=0) MACHINES.
C
C I   LENG  : DATA LENGTH TO BE READ
C O   A4CHR : CHARACTER(A4) DATA ARRAY FOR OUTPUT
C I   CWORK : 1-D ARRAY IN SINGLE PRECISION INCLUDING A4 CHARACTER DATA
C
C NOTE : DO NOT CHANGE THE ORDER OF LENG, A4CHR, CWORK.
C        DO NOT ADD NEW VARIABLES AFTER CWORK.
C        BECAUSE ADDRESS OF VARIABLE AFTER CWORK MAY BE DESTROYED IN
C        SOME COMPUTERS(CRAY-64) WHEN DECLARATIONS FOR CWORK IS NOT
C        CONSISTENT.
C        WHEN 32-BIT MACHINES DISAPPEAR IN FUTURE, PACKWT/PACKRD SHOULD
C        BE MODIFIED OR REMOVED BY USING THE FOLLOWING PROGRAMING:
C
C          EX.    CHARACTER    CWORK*8, A4CHR*4
C                 EQUIVALENCE (WORK(1),CWORK(1))
C                 CWORK(I) = A4CHR(I)//'    '
C                 CALL PONPON('WRTP',SUBNAM,IPDS,MEMBER,LENG,WORK,IPRN)
C                   :
C                 CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,IPRN)
C                 A4CHR(I) = CWORK(I)(1:4)
C
C-----------------------------------------------------------------------
C
C      32-BIT
C    1   2   3   4   5   6   (A4 ADDRESS IN WORK)
C    *---*---*---*---*---*---
C    1   2   3   4   5   6   (A4 ADDRESS IN A4CHR)
C
C    64-BIT
C    1       2       3       4       5       6    (A8 ADDRESS IN WORK)
C    1   2   3   4   5   6   7   8   9   10  11   (A4 ADDRESS IN CWORK)
C    *-------*-------*-------*-------*-------*-------
C    1---    2---    3---    4---    5---    6--- (A4 DATA FOR A4CHR)
C  
C        A4         A4        REAL*8/A8
C      A4CHR(1) = CWORK(1) = WORK(1)(1:4)
C      A4CHR(2) = CWORK(3) = WORK(2)(1:4)
C      A4CHR(3) = CWORK(5) = WORK(3)(1:4)
C        :             :
C        :             :
C      A4CHR(I) = CWORK(2*I-1) = WORK(I)(1:4)
C-----------------------------------------------------------------------
C
C     << SAMPLE IN USE >>
C     CHARACTER*4 A4CHR(10), TMPC
C     DIMENSION   WORK(10)
C     WORK(1)  = 1.0
C     WORK(2)  = 2.0
C     A4CHR(1) = 'AAAA'
C     A4CHR(2) = 'BBBB'
C     CALL PACKWT(2,A4CHR,WORK(3))       !  =>   WORK(3)='AAAA'
C                                        !  =>   WORK(4)='BBBB'
C     A4CHR(3) = 'CCCC'
C     CALL PACKWT(1,A4CHR(3),WORK(5))    !  =>   WORK(5)='CCCC'
C     CALL PACKWT(1,'DDDD',WORK(6))      !  =>   WORK(6)='DDDD'
C     LENG = 6
C     PONPON( 'WRTP',SUBNAM,IPDS,MEMBER,LENG,WORK,IPRN )
C        :
C        :
C     PONPON( 'READ',SUBNAM,IPDS,MEMBER,LENG,WORK,IPRN )
C     CALL PACKRD(2,A4CHR,WORK(5))       !  =>   A4CHR(1)='CCCC'
C                                        !  =>   A4CHR(2)='DDDD'
C     CALL PACKRD(1,A4CHR(3),WORK(3))    !  =>   A4CHR(3)='AAAA'
C        :
C
C***********************************************************************
C
      CHARACTER*4  CWORK, A4CHR
      DIMENSION    CWORK(*), A4CHR(*)
C
C---- FOR AUTOMATIC CHECKING 64-BIT OR 32-BIT
      DIMENSION     ITEST(3), CTEST(3)
      EQUIVALENCE ( ITEST,CTEST )
      CHARACTER*4   CTEST
C
      ITEST(1) = 1
      ITEST(2) = 2
      CTEST(3) = 'IM64'
      IF(ITEST(2).EQ.2) THEN 
C     ( I AM 32-BIT MACHINE )
        IBIT64 = 0
      ELSE
C     ( I AM 64-BIT MACHINE )
        IBIT64 = 1
      ENDIF
      IF(IBIT64.EQ.0) GOTO 1000
C
C==== FOR 64-BIT MACHINES
C
      DO 100 I=1,LENG
        IPOS = 2*I-1
        A4CHR(I) = CWORK(IPOS)
  100 CONTINUE
      GOTO 9999
C
C==== FOR 32-BIT MACHINES
C
 1000 CONTINUE
      DO 200 I=1,LENG
        A4CHR(I) = CWORK(I)
  200 CONTINUE
C
 9999 RETURN
      END

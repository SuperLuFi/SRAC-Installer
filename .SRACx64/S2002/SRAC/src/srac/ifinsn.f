C             IFINSN              LEVEL=1        DATE=81.11.14
      SUBROUTINE IFINSN (U,E,W,N,MM)
C
C     READS SN CONSTANTS FROM STANDARD INTERFACE FILE
C
      COMMON /TW1C/ DD(1),LIM1,IAA(210)
      DIMENSION D(212)
      EQUIVALENCE (D(1),DD(1))
      EQUIVALENCE (D(157),NOUT),(D(165),ISNCON)
C
      DIMENSION U(1), E(1), W(1)
      DIMENSION ER1 ( 4 ), ER2 ( 3 ), IDENT ( 4 ), IIDENT ( 1 ),
     &ISPEC ( 4 )
C
      EQUIVALENCE (ISPEC(1),NDIM), (ISPEC(2),MM4)
      EQUIVALENCE (IDENT(4),IIDENT(1))
C
CSASA REAL*8 IDENT,ER1,ER2
      CHARACTER*8 IDENT,ER1,ER2
C
      DATA ER1/'DIMENS','IONS D','ISAGRE','E     '/
      DATA ER2/'SN ORD','ER DIS','AGREES'/
C
C
C     READ IDENTIFICATION AND SPECIFICATION RECORDS
C
      WRITE (NOUT,120)
      CALL REED (ISNCON,0,0.0,0,4)
      CALL REED (ISNCON,IDENT(4),IDENT,4,10)
      WRITE ( NOUT, 130 ) ( IDENT ( I ), I = 1, 3 ), IIDENT ( 1 )
      CALL REED (ISNCON,0,ISPEC,2,2)
      WRITE (NOUT,140)(ISPEC(I),I=1,2)
      IF ( NDIM .EQ. 2 ) GO TO 80
      CALL ERROR ( 2, ER1, 4 )
      GO TO 111
   80 CONTINUE
      IF ( MM4 .EQ. 4 * MM ) GO TO 90
      CALL ERROR ( 2, ER2, 3 )
      GO TO 111
   90 CONTINUE
      CALL REED (ISNCON,0,W,MM,2)
      CALL REED (ISNCON,1,0.0,2,7)
      CALL REED (ISNCON,MM4,U,MM,8)
      CALL REED (ISNCON,1,0.0,2,7)
      CALL REED (ISNCON,2*MM4,E,MM,8)
      DO 100 M=1,MM
      U(M)=ABS(U(M))
      E(M)=ABS(E(M))
  100 CONTINUE
      WRITE (NOUT,150)N
      DO 110 M=1,MM
      WRITE (NOUT,160)M,U(M),E(M),W(M)
  110 CONTINUE
  111 CONTINUE
      CALL REED ( ISNCON, 0, 0.0, 0, 4 )
      RETURN
C
C
  120 FORMAT('0     INTERFACE FILE SNCONS IS USED TO OBTAIN SN CONSTA',
     &'NTS')
  130 FORMAT(1H0,A6,' FILE NAME '/1X,A6,' USER IDENTIFICATION '/1X,A6
     &/1X,I6,' VERSION  ')
  140 FORMAT(1H ,I6,' NDIM     '/1X,I6,' NDIR     '//)
  150 FORMAT(////' S',I2,' CONSTANTS'//19X,'MU',13X,'ETA',12X,'WEIGHT')
  160 FORMAT(I8,4X,3E16.8)
      END

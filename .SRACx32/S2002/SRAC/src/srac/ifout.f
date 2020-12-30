C             IFOUT               LEVEL=1        DATE=81.11.14
      SUBROUTINE IFOUT
C
C     CREATES INTERFACE OUTPUT FILES
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
      EQUIVALENCE (D(105),LAST1),(A(52),LAST2),(D(114),LXFX),
     &(D(160),NDUMP2),(D(161),NEXTRA)
C
      DIMENSION ERRMG(5)
C
      EQUIVALENCE (IA(57),MM),(IA(58),NM),(IA(64),IT),(IA(65),JT),
     &(IA(77),ITP),(D(187),LW),(D(188),LCM),(D(189),LCE)
C
CSASA REAL*8 ERRMG
      CHARACTER*8 ERRMG
C
      DATA ERRMG/'STORAG','E EXCE','EDED O','N IFOU','T     '/
C
C     SAVE SN CONSTANTS
C
C
C     ASSIGN STORAGE FOR ARRAYS NEEDED TO CREATE INTERFACE OUTPUT
C
      MM4=MM*4
      LTW4=131
      LTCM4=LTW4+MM4
      LTCE4=LTCM4+MM4
      LTTLF=LTCE4+MM4
      LAFI=LTTLF+LXFX
      LAH1=LAFI+MM4*ITP
      LAH2=LAH1+ITP*MM
      LASTOT=LAH2+ITP*MM
C
C     CHECK STORAGE OVERFLOW
C
      IF ( LASTOT .GT. LAST2 ) CALL ERROR ( 3, ERRMG, 5 )
      LAFJ=LAFI
      LAV1=LAH1
      LAV2=LAV1+IT*MM
CKSK  CALL IFRITE ( A(LAFI),A(LAH1),A(LAH2),A(LAFJ),A(LAV1),A(LAV2),
CKSK &A(LTTLF),A(LTTLF),D(LW),D(LCM),D(LCE),A(LTW4),A(LTCM4),
CKSK &A(LTCE4),NM,MM,IT,ITP,JT,MM4 )
      CALL IFRITE ( AAA(LAFI),AAA(LAH1),AAA(LAH2),AAA(LAFJ),AAA(LAV1),
     &AAA(LAV2),AAA(LTTLF),AAA(LTTLF),DD(LW),DD(LCM),DD(LCE),AAA(LTW4),
     &AAA(LTCM4),AAA(LTCE4),NM,MM,IT,ITP,JT,MM4 )
C
C     RETURN COMMON BLOCK A TO CORE
C
      CALL REED (NDUMP2,1,0.0,4,7)
CKSK  CALL REED (NDUMP2,0,A,ITEMP,3)
      CALL REED (NDUMP2,0,AAA,ITEMP,3)
      CALL REED (NDUMP2,0,0.0,0,4)
      RETURN
      END
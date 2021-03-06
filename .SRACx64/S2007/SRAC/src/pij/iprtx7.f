      SUBROUTINE IPRTX7(HOL,MESHX,MESHP)
      DIMENSION MESHX(NX,1),MESHP(NDPIN,NX1,NX1)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGD,NDPIN,
     1                 IDIVP,BETM,NX1,NY1,IDUM(6),NDPIN1
      COMMON /MAINC/ DUM1(63),NOUT1,NOUT2,DUM2(35),CASENM(2),TITLE(18)
C
      CHARACTER*8 AST,BLK,HOL,SUBSTR
      CHARACTER*132 OUTSTR
      CHARACTER*3 AST3,BLK3
C
      DATA AST/'********'/,BLK/'  *     '/
      DATA  AST3/'***'/,BLK3/'  *'/
C
      I0=1
  100 I1=MIN0(NX,I0+14)
      N8=12*(I1-I0)
      LINE=60
      DO 210 J=1,NX1
      IF(LINE.LT.60) GO TO 140
      WRITE(NOUT2,8000) CASENM,TITLE,HOL
 8000 FORMAT(1H1///10X,2A4,'**',18A4,5H**** ,A8,' NUMBER ****')
 8001 FORMAT(5X,I2,15I8)
 8002 FORMAT(4X,16A8)
 8003 FORMAT(1H(,I3,1H))
 8004 FORMAT(2H <,I3,3H> *)
      WRITE(NOUT2,8002) (BLK,I=I0,I1+1)
      WRITE(NOUT2,8001) (I-1,I=I0,I1+1)
      WRITE(NOUT2,8002) (BLK,I=I0,I1+1)
      WRITE(NOUT2,8002) (BLK,I=I0,I1+1)
      LINE=6
  140 CONTINUE
      JSCAN=0
      IF(NAPIN.EQ.0) GO TO 190
      DO 180 JJ=1,NDPIN
      IF(JJ.EQ.1) THEN
      WRITE(OUTSTR(1:4),'(I3,1H )') J-1
      DO 150 N=I0,I1
  150 OUTSTR(1:8*(N-I0)+12)=OUTSTR(1:8*(N-I0)+4)//AST
      OUTSTR(1:8*(I1-I0)+15)=OUTSTR(1:8*(I1-I0)+12)//AST3
                      ELSE
      OUTSTR(1:4)='    '
      DO 160 N=I0,I1
  160 OUTSTR(1:8*(N-I0)+12)=OUTSTR(1:8*(N-I0)+4)//BLK
      OUTSTR(1:8*(I1-I0)+15)=OUTSTR(1:8*(I1-I0)+12)//BLK3
                      ENDIF
C **  SCAN IF PIN ROD LOCATED
      ISCAN=0
      DO 170 I=I0,I1+1
      IF(MESHP(JJ,I,J).NE.0) THEN
      WRITE(SUBSTR,8003) MESHP(JJ,I,J)
      OUTSTR(8*(I-I0)+5:8*(I-I0)+9)=SUBSTR(1:5)
      ISCAN=ISCAN+1
                               ENDIF
  170 CONTINUE
      IF(ISCAN.NE.0) THEN
      JSCAN=JSCAN+1
      WRITE(NOUT2,'(A)') OUTSTR(1:8*(I1-I0)+17)
      LINE=LINE+1
                     ENDIF
  180 CONTINUE
  190 IF(J.EQ.NX1) GO TO 210
C *** MODERATOR REGION
      WRITE(NOUT2,8002) (BLK,I=I0,I1+1)
      LINE=LINE+1
      WRITE(OUTSTR,8002) (BLK,I=I0,I1+1)
      DO 200 I=I0,I1
      IF(MESHX(I,J).NE.0) THEN
      WRITE(SUBSTR,8004) MESHX(I,J)
      OUTSTR(8*(I-I0)+8:8*(I-I0)+15)=SUBSTR
                          ENDIF
  200 CONTINUE
      WRITE(NOUT2,'(A)') OUTSTR(1:8*(I1-I0)+15)
      WRITE(NOUT2,8002) (BLK,I=I0,I1+1)
      LINE=LINE+2
  210 CONTINUE
      IF(I1.GE.NX) RETURN
      I0=I1+1
      GO TO 100
      END

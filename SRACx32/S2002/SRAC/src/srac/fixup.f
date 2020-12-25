C             FIXUP               LEVEL=1        DATE=81.11.14
      SUBROUTINE FIXUP (FI,FJ,FM)
C
      COMMON /WORK/AAA(1),LIM2,AI(130)
      DIMENSION AA(132)
      EQUIVALENCE (AA(1),AAA(1))
C
      EQUIVALENCE ( AA(33),Z),( AA(34),B),( AA(35),C),( AA(36),D),
     &( AA(37),T),( AA(38),S),( AA(39),CT),( AA(42),A)
      EQUIVALENCE ( AA(43),TI),( AA(44),TJ),( AA(45),TM)
C
C
C     SET TO ZERO NEGATIVE FLUX FIXUP
C
      FIP=TI
      FJP=TJ
      FMP=TM
      IF (FIP.LT.0.0) GO TO 100
      IF (FJP) 110,120,120
C
C     I FLUX NEGATIVE
C
  100 F=(Z*FI+2.*B*FJ+(C+D)*FM+S)/(2.*(B+C)+CT)
      IF (F.LT.0.0) GO TO 170
      FIP=0.0
      FJP=F+F-FJ
      IF (FJP.LT.0.0) GO TO 130
      FMP=F+F-FM
      IF (FMP.LT.0.0) GO TO 140
      GO TO 180
C
C     J FLUX NEGATIVE
C
  110 F=((A+Z)*FI+B*FJ+(C+D)*FM+S)/(2.*(A+C)+CT)
      IF (F.LT.0.0) GO TO 170
      FJP=0.0
      FIP=F+F-FI
      IF (FIP.LT.0.0) GO TO 130
      FMP=F+F-FM
      IF (FMP.LT.0.0) GO TO 150
      GO TO 180
C
C     M FLUX NEGATIVE
C
  120 F=((A+Z)*FI+2.*B*FJ+D*FM+S)/(2.*(A+B)+CT)
      IF (F.LT.0.0) GO TO 170
      FMP=0.0
      FIP=F+F-FI
      IF (FIP.LT.0.0) GO TO 140
      FJP=F+F-FJ
      IF (FJP.LT.0.0) GO TO 150
      GO TO 180
C
C     I AND J  FLUX NEGATIVE
C
  130 TC=2.*C+CT
      IF(TC.LE.0) GO TO 170
      F=(Z*FI+B*FJ+(C+D)*FM+S)/TC
      IF (F.LT.0.0) GO TO 170
      FMP=F+F-FM
      IF (FMP.LT.0.0) GO TO 160
      FIP=0.0
      FJP=0.0
      GO TO 180
C
C     I AND M  FLUX NEGATIVE
C
  140 F=(Z*FI+2.*B*FJ+D*FM+S)/(2.*B+CT)
      IF (F.LT.0.0) GO TO 170
      FJP=F+F-FJ
      IF (FJP.LT.0.0) GO TO 160
      FIP=0.0
      FMP=0.0
      GO TO 180
C
C     J AND M FLUX NEGATIVE
C
  150 TA=A+A+CT
      IF (TA.LE.0.0) GO TO 170
      F=((A+Z)*FI+B*FJ+D*FM+S)/TA
      IF (F.LT.0.0) GO TO 170
      FIP=F+F-FI
      IF (FIP.LT.0.0) GO TO 160
      FJP=0.0
      FMP=0.0
      GO TO 180
C
C     I,J, AND M FLUX NEGATIVE
C
  160 IF (CT.LE.0.0) GO TO 170
      F=(Z*FI+B*FJ+D*FM+S)/CT
      IF (F.LT.0.0) GO TO 170
      FIP=0.0
      FJP=0.0
      FMP=0.0
      GO TO 180
C
C     NO FIXUP
C
  170 FI=TI
      FJ=TJ
      FM=TM
      RETURN
C
C     FIXUP
C
  180 FI=FIP
      FJ=FJP
      FM=FMP
      T=F
      RETURN
      END

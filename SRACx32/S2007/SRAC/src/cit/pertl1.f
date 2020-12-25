C     THIS ROUTINE IS USED ONLY IN SRAC-CITATION AND NOT USED IN COREBN
C     AREA AND DELTAX,Y,Z CALCULATION FORT LEAKAGE
C
      SUBROUTINE PERTL1(X     ,XX    ,Y     ,YY    ,Z     ,
     1                  ZZ    ,PDI   ,NRGNE ,PVOL  ,
     2                  J     ,I     ,KB    ,JMAX  ,IMAX  ,
     3                  KBMAX ,JVX   ,IVX   ,KBVX  ,JVXP1 ,
     4                  IVXP1 ,KBVXP1,NUAC5 ,INRB  ,DELL  ,
     5                  DELLL ,TAL   ,DELR  ,DELRR ,TAR   ,
     6                  DELT  ,DELTT ,TAT   ,DELB  ,DELBB ,
     7                  TAB   ,DELF  ,DELFR ,TAF   ,DELK  ,
     8                  DELBK ,TABK  ,DELHT ,DELHB ,TAH   ,
     9                  LVX   ,IOUT                        )
C
      DIMENSION X(JVX),XX(JVXP1),Y(IVX),YY(IVXP1),Z(KBVX),ZZ(KBVXP1),
     1          PDI(IVX),NRGNE(JVX,IVX,KBVX),PVOL(LVX)
C
      PI = 3.141599265D0
C     2*SQRT(1/3) = 1.1547005
      SQUIRE = 1.1547005
C     ODD OR EVEN
      NOE = J-(J/2)*2
      DELL = X(J)-XX(J)
      IF (DELL.EQ.0.0) THEN
       WRITE(IOUT,1010) XX(J),J
       STOP
      ENDIF
      DELLL = 0.0
      IF (J.NE.1) DELLL = XX(J)-X(J-1)
      DELR = XX(J+1)-X(J)
      IF (DELR.EQ.0.0) THEN
       WRITE(IOUT,1010) XX(J+1),J
       STOP
      ENDIF
      DELRR = 0.0
      IF (J.NE.JMAX) DELRR = X(J+1)-XX(J+1)
      T1 = YY(I+1)-YY(I)
      DELT = Y(I)-YY(I)
      IF (DELT.EQ.0.0) THEN
       WRITE(IOUT,1010) YY(I),I
       STOP
      ENDIF
      DELTT = 0.0
      IF (I.EQ.1) GO TO 100
      DELTT = YY(I)-Y(I-1)
      IF (NUAC5.EQ.10.OR.NUAC5.EQ.14) DELTT = YY(I)-PDI(I-1)
  100 CONTINUE
      DELB = YY(I+1)-Y(I)
      IF (NUAC5.EQ.10.OR.NUAC5.EQ.14) DELB = YY(I+1)-PDI(I)
      IF (DELB.EQ.0.0) THEN
       WRITE(IOUT,1010) YY(I+1),I
       STOP
      ENDIF
      DELBB = 0.0
      IF (I.NE.IMAX) DELBB = Y(I+1)-YY(I+1)
      TB = ZZ(KB+1)-ZZ(KB)
      DELF = Z(KB)-ZZ(KB)
      IF (DELF.EQ.0.0) THEN
       WRITE(IOUT,1010) ZZ(KB),KB
       STOP
      ENDIF
      DELFR = 0.0
      IF (KB.NE.1) DELFR = ZZ(KB)-Z(KB-1)
      DELK = ZZ(KB+1)-Z(KB)
      IF (DELK.EQ.0.0) THEN
       WRITE(IOUT,1010) ZZ(KB+1),KB
       STOP
      ENDIF
      DELBK = 0.0
      IF (KB.NE.KBMAX) DELBK = Z(KB+1)-ZZ(KB+1)
  110 CONTINUE
      GO TO (111,120,130,250,250,111,120,180,190,200,210,220,230,240),
     &       NUAC5
  111 CONTINUE
      TAL = T1
      TAR = T1
      TAT = XX(J+1)-XX(J)
      TAB = TAT
      GO TO 250
  120 CONTINUE
      TAL = 2.0*PI*XX(J)*T1
      TAR = 2.0*PI*XX(J+1)*T1
      TAT = PI*(XX(J+1)**2-XX(J)**2)
      TAB = TAT
      GO TO 250
  130 CONTINUE
      TAL = 4.0*PI*XX(J)**2
      TAR = 4.0*PI*XX(J+1)**2
      TAT = 0.0
      TAB = 0.0
      GO TO 250
  180 CONTINUE
      TAL = T1
      TAR = T1
      TAT = YY(I)*(XX(J+1)-XX(J))
      TAB = YY(I+1)*(XX(J+1)-XX(J))
      DELR = DELR*Y(I)
      DELRR = DELRR*Y(I)
      DELL = DELLL*Y(I)
      DELLL = DELLL*Y(I)
      GO TO 250
  190 CONTINUE
      TAT = SQUIRE*DELT
      TAL = SQUIRE*DELL
      TD1 = X(J+1)-X(J)
      TD2 = Y(I)-Y(I-1)
      IF (I.EQ.1) TD2 = 2*Y(I)
      IF (J.EQ.JMAX) TD1 = TD2
      DELHT = 0.5*SQRT(TD1**2+TD2**2-TD1*TD2)
      DELHB = DELHT
      TAR = TAL
      TAB = TAT
      TAH = SQUIRE*DELHT
      GO TO 250
  200 CONTINUE
      TAL = 3.46410*(X(J)-XX(J))
C     2*SQRT(3) = 3.46410
      TAR = 3.46410*(XX(J+1)-X(J))
      IF (NOE.EQ.0) GO TO 201
      TAT = 3.46410*(Y(I)-YY(I))
      TAB = 0.0
      GO TO 250
  201 CONTINUE
      TAB = 3.46410*(YY(I+1)-PDI(I))
      TAT = 0.0
      GO TO 250
  210 CONTINUE
      TAL = T1*TB
      TAR = TAL
      TAT = (XX(J+1)-XX(J))*TB
      TAB = TAT
      TAF = (XX(J+1)-XX(J))*T1
      TABK = TAF
      GO TO 250
  220 CONTINUE
      T2 = XX(J+1)-XX(J)
      TAL = XX(J+1)-XX(J)
      TAL = TB*T1
      TAR = TAL
      TAT = YY(I)*T2*TB
      TAB = YY(I+1)*T2*TB
      TAF = 0.5*(YY(I+1)**2-YY(I)**2)*T2
      TABK = TAF
      DELR = DELR*Y(I)
      DELRR = DELRR*Y(I)
      DELL = DELL*Y(I)
      DELLL = DELLL*Y(I)
      GO TO 250
  230 CONTINUE
      TAT = SQUIRE*DELT*TB
      TAL = SQUIRE*DELL*TB
      TD1 = X(J+1)-X(J)
      TD2 = Y(I)-Y(I-1)
      IF (I.EQ.1) TD2 = 2*Y(I)
      IF (J.EQ.JMAX) TD1 = TD2
      DELHT = 0.5*SQRT(TD1**2+TD2**2-TD1*TD2)
      DELHB = DELHT
      TAR = TAL
      TAB = TAT
      TAH = SQUIRE*DELHT*TB
      TABK = SQUIRE*(DELT**2+DELL**2+DELHT**2)
      TAF = TABK
      GO TO 250
  240 CONTINUE
      TAL = 3.46410*(X(J)-XX(J))*TB
C     2*SQRT(3) = 3.46410
      TAR = 3.46410*(XX(J+1)-X(J))*TB
      NRN = NRGNE(J,I,KB)
      TAF = PVOL(NRN)/TB
      TABK = TAF
      IF (NOE.EQ.0) GO TO 241
      TAT = 3.46410*(Y(I)-YY(I))*TB
      TAB = 0.0
      GO TO 250
  241 CONTINUE
      TAB = 3.46410*(YY(I+1)-PDI(I))*TB
      TAT = 0.0
  250 CONTINUE
      IF (J.NE.JMAX) GO TO 254
      GO TO (254,251,252,253),INRB
  251 CONTINUE
      DELRR = X(1)
      IF (NUAC5.EQ.8.OR.NUAC5.EQ.12) DELRR = DELRR*Y(I)
      GO TO 254
  252 CONTINUE
      DELRR = YY(IVXP1)-Y(IVX)
      IF (NUAC5.EQ.10.OR.NUAC5.EQ.14) DELRR = YY(IVXP1)-PDI(IVX)
      GO TO 254
  253 CONTINUE
      DELRR = DELR
  254 CONTINUE
      IF (J.NE.1) GO TO 255
      IF (INRB.NE.2) GO TO 255
      DELLL = XX(JVXP1)-X(JVX)
      IF (NUAC5.EQ.8.OR.NUAC5.EQ.12) DELLL = DELLL*Y(I)
  255 CONTINUE
      IF (I.NE.IMAX) GO TO 260
      IF (INRB.NE.3) GO TO 260
      DELB = XX(JVXP1)-X(JVX)
      IF (NUAC5.EQ.8.OR.NUAC5.EQ.12) DELB = DELB*Y(I)
      IF (DELB.EQ.0.0) THEN
       WRITE(IOUT,1010) XX(JVXP1),JVX
       STOP
      ENDIF
  260 CONTINUE
      RETURN
 1010 FORMAT('0*** SAMPLE EDGE (',1PE12.5,') IS EQUAL TO FLUX',I5,'-TH',
     &       ' POINT'                                                  )
      END

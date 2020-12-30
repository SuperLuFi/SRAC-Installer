      SUBROUTINE INTEGP(MESH  ,MPOI  ,R     ,DN58  ,NOUT2 ,
     1                  JERR  ,IEND  ,NNAME ,VOLUME,RR    ,
     2                  VD                                 )
C
C     CALCULATE INTEGRAL PARAMETER FROM REACTION RATE
C
C      R = REACTION PER ATOM AS FISS,CAPT
C      R(1,*,**) : EPITHERMAL
C      R(2,*,**) : THERMAL
C          *     : =1 FISSION , =2 CAPTURE
C            **  : =1 U-235   , =2 U-238
C      RN = REACTION R*VOL*NUMBER DENSITY
C      FISSN, CAPTN
      DIMENSION R(2,2,2),MESH(3),DN58(2),FISS(2),CAPT(2),NNAME(2),
     *          RR(2,2,2),VD(2)
      DIMENSION RN(2,2,2),FISSN(2),CAPTN(2)
C
C
      IF(IEND.EQ.0) RETURN
      IF(JERR.NE.0) GO TO 1000
      FISS(1) = R(1,1,1) + R(2,1,1)
      FISS(2) = R(1,1,2) + R(2,1,2)
      CAPT(1) = R(1,2,1) + R(2,2,1)
      CAPT(2) = R(1,2,2) + R(2,2,2)
C     DNR  = DN58(2)/DN58(1)
CJAIS MODIFIED FOR ZERO DEVIDE     *** APRIL 11/1985 ***
C     RHO28=R(1,2,2)/R(2,2,2)
C     DEL25=R(1,1,1)/R(2,1,1)
C     IF(R(2,2,2).NE.0.0) THEN
C                         RHO28=R(1,2,2)/R(2,2,2)
C                         ELSE
C                         RHO28=1.0E+70
C                         IF(R(1,2,2).EQ.0.0) RHO28=1.0
C                         ENDIF
C     IF(R(2,1,1).NE.0.0) THEN
C                         DEL25=R(1,1,1)/R(2,1,1)
C                         ELSE
C                         DEL25=1.0E+70
C                         IF(R(1,1,1).EQ.0.0) DEL25=1.0
C                         ENDIF
CJAIS END
C     IF(FISS(1).NE.0.0) THEN
C     DEL28=FISS(2)/FISS(1)*DNR
C     CSTAR=CAPT(2)/FISS(1)*DNR
C                        ELSE
C     DEL28=0.
C     CSTAR=0.
C                        ENDIF
C
      IF (MESH(1).EQ.0) THEN
      WRITE(NOUT2,6000) MESH(1),MESH(2),MESH(3),VOLUME,MPOI
      WRITE(NOUT2,6110) NNAME(1),DN58(1),R(1,1,1),R(2,1,1),FISS(1),
     *                  R(1,2,1),R(2,2,1),CAPT(1)
      WRITE(NOUT2,6120) NNAME(2),DN58(2),R(1,1,2),R(2,1,2),FISS(2),
     *                  R(1,2,2),R(2,2,2),CAPT(2)
C     WRITE(NOUT2,6200) NNAME(2),RHO28,NNAME(1),DEL25,
C    1                  NNAME(2),NNAME(1),DEL28,NNAME(2),NNAME(1),
C    2                  CSTAR
                        GO TO 130
                        ENDIF
      VD(1) = VD(1) +         VOLUME
      VD(2) = VD(2) +         VOLUME
      DO 120 I = 1,2
      DO 110 J = 1,2
      DO 100 K = 1,2
      RR(K,J,I) = RR(K,J,I) + R(K,J,I)*VOLUME*DN58(I)
      RN(K,J,I) =             R(K,J,I)*VOLUME*DN58(I)
  100 CONTINUE
  110 CONTINUE
      FISSN(I)=               FISS(I)*VOLUME*DN58(I)
      CAPTN(I)=               CAPT(I)*VOLUME*DN58(I)
  120 CONTINUE
      WRITE(NOUT2,6000) MESH(1),MESH(2),MESH(3),VOLUME,MPOI
      WRITE(NOUT2,6110) NNAME(1),DN58(1),RN(1,1,1),RN(2,1,1),FISSN(1),
     *                  RN(1,2,1),RN(2,2,1),CAPTN(1)
      WRITE(NOUT2,6120) NNAME(2),DN58(2),RN(1,1,2),RN(2,1,2),FISSN(2),
     *                  RN(1,2,2),RN(2,2,2),CAPTN(2)
  130 CONTINUE
      GO TO 9000
 1000 CONTINUE
      WRITE(NOUT2,6000) MESH(1),MESH(2),MESH(3),VOLUME,MPOI
      GO TO (210,220,230,240,250,260),JERR
  210 WRITE(NOUT2,6300)
      GO TO 9000
  220 WRITE(NOUT2,6310)
      GO TO 9000
  230 WRITE(NOUT2,6300)
      WRITE(NOUT2,6310)
      GO TO 9000
  240 WRITE(NOUT2,6320)
      GO TO 9000
  250 WRITE(NOUT2,6330)
      GO TO 9000
  260 WRITE(NOUT2,6340)
 9000 CONTINUE
      RETURN
 6000 FORMAT(1H0/' MESH POINT (X,Y,Z) = (',I3,',',I3,',',I3,')',
     1               ' VOLUME =',1PE12.5,' (CC) MATERIAL NO. =',I3)
 6110 FORMAT(2H0 ,'NAME   DENSITY',T19,'EPITHERMAL-F',T33,'THERMAL-F',
     2       T46,'TOTAL-F',T55,'EPITHERMAL-C',T69,'THERMAL-C',T82,
     3       'TOTAL-C'//2X,A4,1P7E12.5                    )
 6120 FORMAT(2X,A4,1P7E12.5                    )
 6200 FORMAT(1H0,1X,
     1 'RATIO OF EPITHERMAL TO THERMAL ',A4,' CAPTURES  ----',
     2  1PE13.5/2X,
     3 'RATIO OF EPITHERMAL TO THERMAL ',A4,' FISSIONS  ----',
     4  1PE13.5/2X,
     5 'RATIO OF ',A4,' FISSIONS TO ',A4,' FISSIONS  ---------',
     6  1PE13.5/2X,
     7 'RATIO OF ',A4,' CAPTURES TO ',A4,' FISSIONS  ---------',
     8  1PE13.5)
 6300 FORMAT(1H0,10X,'U235 NUCLIDE OVER')
 6310 FORMAT(1H0,10X,'U238 NUCLIDE OVER')
 6320 FORMAT(1H0,10X,'MESH POINT OUT OF RANGE   SKIP THIS CASE')
 6330 FORMAT(1H0,10X,'MATERIAL POINTER EXCEED NUMBER OF MATERIALS',
     1               '   SKIP THIS CASE')
 6340 FORMAT(1H0,10X,'NUCLIDE DENSITY IS NOT GIVEN  SKIP THIS CASE')
      END

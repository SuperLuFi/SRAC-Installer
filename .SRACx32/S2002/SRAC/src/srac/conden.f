C **********************************************************************
C                        CONDEN
C **********************************************************************
      SUBROUTINE CONDEN(IC,ICODE,NXR,NECF,MATNM,NM,MATD,NISO)
C
C     SUBPROGRAM TO CALL CONDEM FOR  NXR EFFECTIVE  MACRO-X-SECTIONS
C           AND FOR MACRO-X-SECTIONS OF EACH ISOLATED COMPONENT MIXTURE
C
C     IC = 1  READ FAST & THERMAL FINE THEN PUT ALL CONDENSED
C     IC = 2  READ ALL FINE THEN PUT ALL CONDENSED
C
C     NXR  NUMBER OF SETS OF CROSS SECTIONS
C     NECF ARRAY FOR CORRESPONDING BETWEEN FINE AND COARSE GROUP
C     MATNM ARRAY CONTAINING ALPHABETIC MATERIAL NAMES
C     NM   NUMBER OF M-REGIONS USED IN THE PREVIOUS FLUX SOLVING ROUTINE
C     MATD ARRAY CONTAINING SIGNED POSITION IN MATNM OF M-REGION
C                NEGATIVE NUMBER DENOTES ANISOTROPIC MATERIAL
C     NISO NUMBER OF ISOTOPES IN A MATERIAL
C     MODIFIED ON 6.25/85 TO WRITE CONTA002 & CONTA000 IN 'FLUX' FILE
C
      CHARACTER *4   CASENM,FILENM,NUMB
C
      COMMON /MAINC / IOPT(54),NEF,NET,
     *                NERF,NERT,NMAT,DUM2(4),NOUT1,NOUT2,DUM3(6),
     *                NEF3,DUM73(5),IMICEF,DUM79(22),CASENM(2),DUM5(398)
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
      COMMON /WORK  / AA(7500)
C
      CHARACTER *4    NODE(2),MATNM(2,*)
C
      DIMENSION       DD(330),NREG(107),ASMPT(107)
      DIMENSION       NECF(1),II(1),WT(107),WX(107)
      DIMENSION       MATD(1),NISO(1),ICARR(150),WT1(107),WX1(107)
C
      EQUIVALENCE    (IOPT(42),ICOND),(IOPT(43),IP1C),(IOPT(39),ISCT)
      EQUIVALENCE    (AA(1),II(1)),
     *               (DD(2),WT(1)),(DD(110),WX(1)),(NER,DD(109))
C
C     DD(109) CONTAIN NER    : NUMBER OF COARSE ENERGY GROUPS
C     WX(1)    ...WX(NER)    : COARSE SPECTRUM
C     WX(NER+1)...(WX(2*NER) : COARSE ENERGY BOUNDARIES
C
C     START OF PROCESS
C
      IFLSW = 1
      NER   = NERF+NERT
      NEFT  = NEF+NET
C     TEST IP1C AND ISCT WHEN ICODE=2(ANISN)
      IF(ICODE.EQ.2.AND.ISCT.EQ.0) IP1C = 0
C
C === CONTROL MEMBER ON MACROWRK AND FLUX FILES
C
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
      NODE(1)   = 'CONT'
      NODE(2)   = 'A002'
      IF (IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      CALL SEARCH(NODE,LTH,ISW1)
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      CALL SEARCH(NODE,LTH,ISW)
      IF (ISW.EQ.0) THEN
                    CALL READ(NODE,II(2001),LTH)
                    GO TO 6
                    ENDIF
C === IF NOT ON MACROWRK, THEN
      NODE(2) (1:1) = 'F'
      LTH           = 2*(NEF+1)
      CALL SEARCH(NODE,LGT,ISW)
      IF (ISW.EQ.1) RETURN
C ===
      CALL READ(NODE,II,LTH)
      IF (IOPT(4).NE.0) THEN
                        NODE(2) (1:1) = 'T'
                        LTH           = 2*(NET+1)
                        CALL READ(NODE,II(1001),LTH)
                        ENDIF
C
      II(2001) = NEFT
      LOC1     = 1
      LOC2     = NEF+1
      LOC3     = 2001
      LOC4     = 2001+II(2001)
      DO   3 I = 1,NEF
      LOC1     = LOC1+1
      LOC2     = LOC2+1
      LOC3     = LOC3+1
      LOC4     = LOC4+1
      AA(LOC3) = AA(LOC1)
      AA(LOC4) = AA(LOC2)
    3 CONTINUE
C
      IF (IOPT(4).EQ.0) THEN
                        LOC2     = LOC2+1
                        LOC4     = LOC4+1
                        AA(LOC4) = AA(LOC2)
                        LTH      = 2*(NEF+1)
                        GO TO 6
                        ENDIF
C === THERMAL CONTROL DATA
      LOC1     = 1001
      LOC2     = 1001+NET
      LOC3     = 2001+NEF
      LOC4     = 2001+II(2001) + NEF
      DO   4 I = 1,NET
      LOC1     = LOC1+1
      LOC2     = LOC2+1
      LOC3     = LOC3+1
      LOC4      = LOC4+1
      AA(LOC3) = AA(LOC1)
      AA(LOC4) = AA(LOC2)
    4 CONTINUE
      LOC4     = LOC4+1
      LOC2     = LOC2+1
      AA(LOC4) = AA(LOC2)
      LTH      = 2*(NEFT+1)
      NODE(2) (4:4) = 'A'
C
    6 CONTINUE
      IF(ISW1.EQ.1) THEN
                    FILENM(1) = 'FLUX'
                    FILENM(2) = '    '
                    CALL WRITE(NODE,II(2001),LTH)
                    ENDIF
      LOC1     = 2001
      LOC2     = 2002+NEFT
      LOC3     = NER+1
C   HIGHEST ENERGY BOUNDARY OF COARSE GROUP
      WX(LOC3) = AA(LOC2)
C    FROM NECF INTO NREG
      J        = 0
      DO 10  N = 1,NER
      WX(N)    = 0.0
      LOC3     = LOC3+1
      DO  5  I = 1,NECF(N)
      LOC1     = LOC1+1
      LOC2     = LOC2+1
      J        = J+1
      NREG(J)  = N
      ASMPT(J) = AA(LOC1)
      WX(N)    = WX(N) + AA(LOC1)
    5 CONTINUE
      WX(LOC3) = AA(LOC2)
   10 CONTINUE
C
C === CONTROL MEMBER ON MACRO AND FLUX FILE
C
      NODE(1)   = 'CONT'
      NODE(2)   = 'A000'
      IF (IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
      CALL SEARCH(NODE,LTH1,ISW1)
      FILENM(1) = 'MACR'
      FILENM(2) = 'O   '
      CALL SEARCH(NODE,LTH1,ISW)
      IF(ISW.NE.0) THEN
                   CALL WRITE(NODE,NER,2*(NER+1))
                   CALL ENTAPR ('COARSE  ','WHOLE   ',NER,WX(NER+1))
                   ENDIF
C
      IF(ISW1.NE.0) THEN
                    FILENM(1) = 'FLUX'
                    FILENM(2) = '    '
                    CALL WRITE(NODE,NER,2*(NER+1))
                    ENDIF
C
C     END CONTA002
C
      CALL ICLEA (ICARR,150,0)
      IF(NMAT.GT.150) GO TO 9010
C
      IF (NM.GT.0) THEN
                   DO 30 MNO = 1,NM
                   MAT = IABS(MATD(MNO))
                   ICARR(MAT) = 1
   30              CONTINUE
                   ENDIF
C
  738 FORMAT(A,2A4,A)
C
C === LOOP OF MIXTURES
C
      DO 40 MAT = 1,NMAT
C     SKIP   IF THIS MATERIAL IS USED IN THE FIXED SOURCE PROBLEM
      IF (ICARR(MAT).EQ.1) GO TO 40
C     SKIP IF THE COMPOSITION OF THIS MATERIAL IS NOT GIVEN
      IFFLG     = 0
      IF (NISO(MAT).EQ.0 ) IFFLG=1
C     THE WEIGHTING SPECTRUM WILL BE THE ASYMPTOTIC ONE STORED IN
C     THE MEMBER CONTA002 IF THE PROPER ONE IS NOT PROVIDED
      FILENM(1) = 'MACR'
      FILENM(2) = 'O   '
      NODE(1)   = MATNM(1,MAT)
      NODE(2)   = MATNM(2,MAT)
      NODE(2) (4:4) = '0'
      NODE(2) (1:1) = 'A'
C     FIND WHETHER IF THE CONDENSED X-SECTIO IN MACRO FILE
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.0) GO TO 40
C
      DO   35 J = 1,NEFT
      WT(J)     = ASMPT(J)
      WT1(J)    = ASMPT(J)
   35 CONTINUE
      IF(ICOND.EQ.0 .AND. IFFLG.EQ.0) THEN
                                      WRITE(NOUT1,738)
     +                '  **',NODE,'** CONDENSED BY ASYMPTOTIC SPECTRUM'
                                      GO TO 39
                                      ENDIF
C
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
C     FIND THE PROPER SPECTRUM OF THE SAME MEMBER IN THE FLUX FILE
      IF(IOPT(4).EQ.0)   NODE(2) (1:1) = 'F'
      NODE(2)(4:4) = '2'
      CALL SEARCH(NODE,LTH,ISW)
C === ALL ENERGY RANGE
      IF (ISW.EQ.0) THEN
                    CALL READ(NODE,WT,NEFT)
                    WRITE(NOUT1,738) ' **',NODE,
     @              '** IS USED FOR CONDENSE AS P0 SPECTRUM '
C
                    DO 36 NG = 1,NEFT
                    WT1(NG)  = WT(NG)
   36               CONTINUE
C     P1 SPECTRUM FOR CONDENSE AFTER FINDING P0 SPECTRUM
CDEL                IF(IP1C.NE.0) THEN
                                  NODE(2)(4:4)='3'
                                  CALL SEARCH(NODE,LTH,JSW)
                                  IF (JSW.EQ.1) THEN
                                           WRITE(NOUT1,738) ' **',NODE,
     @    '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND THEN USER P0 FLUX.'
                                                ELSE
C
                                  CALL READ(NODE,WT1,NEFT)
                                  WRITE(NOUT1,738) ' **',NODE,
     @                         '** IS USED FOR CONDENSE AS P1 SPECTRUM '
CDEL                                            ENDIF
                                  ENDIF
C ================= FAST & THERMAL ENERGY RANGE
                    ELSE
                    NODE(2) (1:1) = 'F'
                    CALL SEARCH(NODE,LTH,ISW1)
                    IF(ISW1.EQ.1) THEN
                                  IF(IFFLG.EQ.1) THEN
                                  WRITE(NOUT1,738) ' **',NODE,
     @   ' HAS NO P0 SPECTRUM AND NISO=0 ,  THEN SKIP CONDENSE ]] '
                                                 GO TO 40
                                                 ENDIF
                                  WRITE(NOUT1,738) ' **',NODE,
     @                            '** CONDENSED BY ASYMPTOTIC SPECTRUM'
                                                 GO TO 39
                                  ENDIF
C
                    CALL READ(NODE,WT,NEF)
                    NODE(2) (1:1) = 'T'
                    CALL SEARCH(NODE,LTH,ISW1)
                    IF(ISW1.EQ.1) THEN
                                  WRITE(NOUT1,738) ' **',NODE,
     @        '** FOR CONDENSE BY P0 SPECTRUM NOT FOUND IN FLUX FILE ]]'
                                  STOP
                                  ENDIF
C
                    CALL READ(NODE,WT(NEF+1),NET)
C
                    DO 37 NG = 1,NEFT
                    WT1(NG)  = WT(NG)
   37               CONTINUE
C     P1 SPECTRUM FOR CONDENSE AFTER FINDING P0 SPECTRUM
CDEL                IF(IP1C.NE.0) THEN
                                  NODE(2)(1:1)='F'
                                  NODE(2)(4:4)='3'
                                  CALL SEARCH(NODE,LTH,JSW)
                                  IF (JSW.EQ.1) THEN
                                           WRITE(NOUT1,738) ' **',NODE,
     @  '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND THEN USER P0 FLUX.'
                                                GO TO 39
                                                ENDIF
                                  CALL READ(NODE,WT1,NEFT)
                                  WRITE(NOUT1,738) ' **',NODE,
     @                         '** IS USED FOR CONDENSE AS P1 SPECTRUM '

C
                                  NODE(2)(1:1)='T'
                                  CALL SEARCH(NODE,LTH,JSW)
                                  IF (JSW.EQ.1) THEN
                                           WRITE(NOUT1,738) ' **',NODE,
     @        '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND IN FLUX FILE ]]'
                                                STOP
                                                ENDIF
                                  CALL READ(NODE,WT1(NEF+1),NET)
CDEL                              ENDIF
                    ENDIF
C
   39 CALL CONDEM(NODE,IC,NER,NREG,WT,WX,WT1,WX1)
   40 CONTINUE
C
C     END OF ISOLATED MIXTURE
C
      IF (NXR.EQ.0) RETURN
C
C     PROCESS FOR EFFECTIVE X-SECTIONS FOLLOWS
C       = = =  LOOP OF X-REGIONS  = = =
C
      NODE(1)    = CASENM(1)
      DO 1000 IX = 1,NXR
      FILENM(1)  = 'FLUX'
      FILENM(2)  = '    '
      NODE(2)    = 'A0X2'
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
                        NODE(2) (3:3) = NUMB(IX      ) (4:4)
      IF(IOPT(4).EQ.0)  NODE(2) (1:1) = 'F'
      CALL SEARCH(NODE,LTH,ISW)
C===== ALL ENERGY RANGE
      IF(ISW.EQ.0)  THEN
                    CALL READ(NODE,WT,NEFT)
                    DO 43 NG = 1,NEFT
                    WT1(NG)  = WT(NG)
   43               CONTINUE
C
                    NODE(2)(4:4)='3'
                    CALL SEARCH(NODE,LTH,JSW)
C
                    IF(JSW.NE.0) THEN
                                 IF(IP1C.NE.0) THEN
                                          WRITE(NOUT1,738) ' **',NODE,
     @         '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND IN FLUX FILE '
                                               STOP
                                               ELSE
                                           WRITE(NOUT1,738) ' **',NODE,
     @  '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND THEN USER P0 FLUX.'
                                               GO TO 50
                                               ENDIF
                                  ENDIF
                    CALL READ(NODE,WT1,NEFT)
                    WRITE(NOUT1,738) ' **',NODE,
     @                               '** CONDENSED BY ITS P1 SPECTRUM'
C
C ================ FAST & THERMAL REANGE
C
                   ELSE
                   NODE(2) (1:1) = 'F'
                   CALL SEARCH(NODE,LTH,JSW)
                   IF(JSW.NE.0) THEN
                                WRITE(NOUT1,738) ' **',NODE,
     @         '** FOR CONDENSE BY P0 SPECTRUM NOT FOUND IN FLUX FILE '
                                STOP
                                ENDIF
                   CALL READ(NODE,WT,NEF)
C
                   NODE(2) (1:1) = 'T'
                   CALL SEARCH(NODE,LTH,JSW)
                   IF(JSW.NE.0) THEN
                                WRITE(NOUT1,738) ' **',NODE,
     @         '** FOR CONDENSE BY P0 SPECTRUM NOT FOUND IN FLUX FILE '
                                STOP
                                ENDIF
                   CALL READ(NODE,WT(NEF+1),NET)
C
                   DO 47 NG = 1,NEFT
                   WT1(NG)  = WT(NG)
   47              CONTINUE
C
                   NODE(2)(4:4)='3'
                   NODE(2)(1:1)='F'
                   CALL SEARCH(NODE,LTH,JSW)
                   IF(JSW.NE.0) THEN
                                IF(IP1C.NE.0) THEN
                                          WRITE(NOUT1,738) ' **',NODE,
     @         '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND IN FLUX FILE '
                                              STOP
                                              ELSE
                                           WRITE(NOUT1,738) ' **',NODE,
     @  '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND THEN USER P0 FLUX.'
                                              GO TO 50
                                              ENDIF
                                 ENDIF
C
                    CALL READ(NODE,WT1,NEF)
                    WRITE(NOUT1,738) ' **',NODE,
     @                               '** CONDENSED BY ITS P1 SPECTRUM'
C
                    NODE(2)(1:1)='T'
                    CALL SEARCH(NODE,LTH,JSW)
                    IF(JSW.NE.0) THEN
                                 WRITE(NOUT1,738) ' **',NODE,
     @         '** FOR CONDENSE BY P1 SPECTRUM NOT FOUND IN FLUX FILE '
                                 STOP
                                 ENDIF
                    CALL READ(NODE,WT1(NEF+1),NET)
                    ENDIF
C
   50 CALL CONDEM(NODE,IC,NER,NREG,WT,WX,WT1,WX1)
 1000 CONTINUE
C
C     END OF PROCESS
C
      RETURN
C
 9010 WRITE(NOUT1,9020) NMAT
      STOP
 9020 FORMAT(1H0,' WORK DIMENSION OVERFLOW AT SUBR(CONDEN) ]]] ',
     +      /1H ,' NMAT(',I3,') IS GREATER THAN 150 . ')
C
      END

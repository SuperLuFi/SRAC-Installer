C **********************************************************************
C                          CONCAT                        JULY 27 1988
C **********************************************************************
      SUBROUTINE CONCAT(NXR,MATNM,ICODE)
C
C     SUBPROGRAM TO CONCATENATION OF THE FAST AND THERMAL RANGE
C     X-SECTION WHEN NO CONDENSE DONE
C
C             ICODE = 0 FOR MIXTURE
C             ICODE = N FOR X-REGION  OF CODE N
C
C     CONCAT IS CALL BY SRAC ROUTINE AS FOLLOWED
C
C       16500 IF(IOPT(2).NE.0) CALL CONCAT(NXX,CASEID,1)
C       16600 CALL CONCAT(NMAT,II(LCMTNM),0)
C
C
      CHARACTER *4    CASENM,NUMB,FILENM
C
      COMMON /MAINC / IOPT(36),IUPSCT,IBSPCT,ISCT,DUM(15),
     *                NEF,NET,NERF,NERT,NMAT,DUM2(4),NOUT1,NOUT2,
     *                DUM3(6),NEF3,DUM4(26),ICF,I100,CASENM(2),DUM5(398)
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /WORK  / AA(10000),BB(10000),CC(10000)
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      CHARACTER *4    MATNM(2,*),NODE(2),NODE2(2)
      DIMENSION       II(1)
C
      EQUIVALENCE (AA(1),II(1))
C
C     START OF PROCESS
C
      IFLSW    = 1
      NODE(1)  = 'CONT'
      NODE(2)  = 'A002'
      FILENM(1)= 'FLUX'
      FILENM(2)= '    '
C ==  CHECK 'CONTA002' MEMBER IN FLUX FILE AND SET ISW1
      CALL SEARCH(NODE,LTH,ISW1)
      FILENM(1)= 'MACR'
      FILENM(2)= 'OWRK'
C ==  CHECK 'CONTA002' MEMBER IN MACROWRK FILE
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.0) GO TO 630
C
      NODE(2)  = 'F002'
      LTH      = 2*(NEF+1)
C ==  TEST IF FINE GROUP CROSS SECTIONS EXIST *** SEPT 24 82'
      CALL SEARCH(NODE,LGT,ISW)
      IF (ISW.EQ.1) RETURN
      CALL READ(NODE,II      ,LTH)
C
      NODE(2)  = 'T002'
      LTH      = 2*(NET+1)
      CALL READ(NODE,II(1001),LTH)
C
      II(2001) = NEF+NET
      LOC1     = 1
      LOC2     = NEF+1
      LOC3     = 2001
      LOC4     = 2001+II(2001)
      DO 500 I = 1,NEF
      LOC1     = LOC1 + 1
      LOC2     = LOC2 + 1
      LOC3     = LOC3 + 1
      LOC4     = LOC4 + 1
      AA(LOC3) = AA(LOC1)
      AA(LOC4) = AA(LOC2)
 500  CONTINUE
      LOC1     = 1001
      LOC2     = 1001 + NET
      LOC3     = 2001 + NEF
      LOC4     = 2001 + II(2001) + NEF
      DO 600 I = 1,NET
      LOC1     = LOC1 + 1
      LOC2     = LOC2 + 1
      LOC3     = LOC3 + 1
      LOC4     = LOC4 + 1
      AA(LOC3) = AA(LOC1)
      AA(LOC4) = AA(LOC2)
 600  CONTINUE
      LOC4     = LOC4+1
      LOC2     = LOC2+1
      AA(LOC4) = AA(LOC2)
      LTH      = II(2001)*2 + 2
      NODE(2)  = 'A002'
C === CONTROL DATA CONCATENATED
      CALL WRITE(NODE,II(2001),LTH)
C
      IF(ISW1.EQ.1) THEN
                    FILENM(1) = 'FLUX'
                    FILENM(2) = '    '
                    CALL WRITE(NODE,II(2001),LTH)
                    FILENM(1) = 'MACR'
                    FILENM(2) = 'OWRK'
                    ENDIF
C
C ==  CREATE 'NAMEABNP' MEBER IN MACROWRK FILE  :  P = '2' '4' '3' '....
C
  630 CONTINUE
      ISCT1       = 1
      IF(ISCT.GT.1) ISCT1 = ISCT
      DO 1000 IX  =  1,NXR
      DO 1000 IPL = -1,ISCT1
      IRNM        = 0
      ISW         = 0
      ISWN        = 1
      IF(ICODE.GT.0) GO TO 650
C-----CASE FOR MITXURE
      NODE(1)     = MATNM(1,IX)
      NODE(2)     = MATNM(2,IX)
      IF(IPL.EQ.-1) NODE(2) (4:4) = '2'
      IF(IPL.EQ. 0) NODE(2) (4:4) = '4'
      IF(IPL.EQ.+1) NODE(2) (4:4) = '3'
      IF(IPL.GT.+1) NODE(2) (4:4) = NUMB(IPL+3) (4:4)
      NODE(2)(1:1) = 'A'
      GO  TO 700
C-----CASE FOR X-REGION
  650 NODE(1)     = CASENM(1)
      NODE(2)     = 'A0X2'
      IF(IPL.EQ.-1) NODE(2) (4:4) = '2'
      IF(IPL.EQ. 0) NODE(2) (4:4) = '4'
      IF(IPL.EQ.+1) NODE(2) (4:4) = '3'
      IF(IPL.GT.+1) NODE(2) (4:4) = NUMB(IPL+3) (4:4)
      IF(IOPT(79).GT.0) NODE(2) (2:2) = NUMB(IOPT(79)) (4:4)
                        NODE(2) (3:3) = NUMB(IX)       (4:4)
C     LETTER 2 DENOTES P0 COMP FINE MESH
  700 CONTINUE
      JSW      = 0
      CALL SEARCH(NODE,LTH,JSW)
      IF(JSW.EQ.0) GO TO 1000
C
      ISW      = 0
      NODE (2) (1:1) = 'F'
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.1) GO TO 1000
C
      CALL READ(NODE,AA,LTH)
      LTH1     = LTH
C === SEARCH N2N DATA ON MEMBER ----F--M
      IF(IPL.EQ.0) THEN
                   NODE2(1) = NODE(1)
                   NODE2(2) = NODE(2)
                   NODE2(2) (4:4) = 'M'
                   CALL SEARCH(NODE2,LTHN,ISWN)
                   ENDIF
C === SERACH THERMAL DATA ON MEMBER ----T----
      NODE(2) (1:1) = 'T'
      ISW           = 0
      CALL SEARCH(NODE,LTH,ISW)
C
      IF(ISW.EQ.0) THEN
                   CALL READ(NODE,AA(LTH1+1),LTH)
                   LTH    = LTH1 + LTH
                   ELSE
                   IF(IPL.LE.0) THEN
                                WRITE(NOUT1,1904) NODE
                                STOP
                                ENDIF
                   WRITE(NOUT1,1903) NODE
                   DO 730   I = 1,NET
                   II(LTH1+1) = 1
                   II(LTH1+2) = 1
                   CALL CLEA(AA(LTH1+3),9,0.0)
                   LTH1       = LTH1+11
  730              CONTINUE
                   LTH        = LTH1
                   ENDIF
C ==  CHECK DIMENSION SIZE OF AA ARRAY
      IF(LTH.GT.8500) GO TO  1901
C
      NODE(2) (1:1) = 'A'
      CALL SEARCH(NODE,LTH1,ISW)
      IF(ISW.EQ.1) CALL WRITE(NODE,AA,LTH)
C
      IF (IPL.NE.0) GO TO 1000
C
C MODFIED FOR DOUBLE PIJ CELL CALCULATION **  JAN/1995 *****
C RENAME PROCESS IS OMITTED BUT NEW ----A--M NENBER WILL BE CREATED
C
C ==  RENAME FOR N2N DATA FROM ----F--M INTO ----A--M
C
      IF(ISWN.EQ.0) THEN
                    NODE2(2) (1:1) = 'A'
                    NODE (2) (1:1) = 'F'
                    NODE (2) (4:4) = 'M'
CDEL                CALL RINAME(NODE,NODE2)
                    CALL READ  (NODE ,CC,LTHN)
                    CALL WRITE (NODE2,CC,LTHN)
                    ENDIF
C     DELAYED NEUTRON DATA : SKIP WRITE IF  NON FISSILE MATERIAL
      NODE(2)(1:1) ='F'
      NODE(2)(4:4) ='Y'
      CALL SEARCH(NODE,LTHD,ISW)
      IF (ISW .EQ. 1) GO TO 1000
C
      NFAMLY       = 6
CM    IF(LTHD.EQ.NEF*30) NFAMLY=15
      IF(LTHD.EQ.NEF*45) NFAMLY=15
      CALL READ(NODE,AA,LTHD)
      NODE(2)(1:1) ='T'
C86/01/21 FERTILE (THEMAL NON-FISSILE AND FAST FISSILE)
      CALL SEARCH(NODE,LTHD1,ISWD)
      IF (ISWD.EQ.0) CALL READ(NODE,AA(LTHD+1),LTHD1)
      LOC          = (NEF+NET)*NFAMLY
      LOC2         = LOC + (NEF+NET)*NFAMLY
      DO 10010   I = 1,NEF*NFAMLY
      BB(I)        = AA(I)
      BB(LOC+I)    = AA(NEF*NFAMLY+I)
      BB(LOC2+I)   = AA(2*NEF*NFAMLY+I)
10010 CONTINUE
      LOC          = LOC  + NEF*NFAMLY
      LOC1         = LTHD + NET*NFAMLY
      LOCB         = LOC2 + NEF*NFAMLY
      LOC2         = LOC1 + NET*NFAMLY
      DO 10020       I = 1,NET*NFAMLY
      BB(NEF*NFAMLY+I) = 0.0
      BB(LOC+I  )      = 0.0
      BB(LOCB+I )      = 0.0
      IF (ISWD.NE.0) GO TO 10020
      BB(NEF*NFAMLY+I) = AA(LTHD+I)
      BB(LOC+I       ) = AA(LOC1+I)
      BB(LOCB+I )      = AA(LOC2+I)
10020 CONTINUE
C
      NODE(2)(1:1) ='A'
      CALL SEARCH(NODE,LTH1,ISW)
CM    LENG         = LTHD+NET*NFAMLY*2
      LENG         = LTHD+NET*NFAMLY*3
      IF (ISW.EQ.1) CALL WRITE(NODE,BB,LENG)
 1000 CONTINUE
C
C     END OF PROCESS
C
      RETURN
C
C     ERROR STOP LACK OF DIMENSION OF AA ARRAY
C
 1901 WRITE(NOUT1,1902)  LTH
      WRITE(NOUT2,1902)  LTH
      STOP
C
 1902 FORMAT(1H0,'**** LENGTH OVER OF MACRO X-SECTION IN THE CONCAT STEP
     *', I6,' WORDS REQUIRED')
 1903 FORMAT(' **** NO ANISOTROPIC COMPONENT IN THERMAL ',2A4)
 1904 FORMAT(' **** NO P0 COMPONENT IN THERMAL ',2A4,
     *       ' STOP IN CONCAT ]]]] ')
C
      END

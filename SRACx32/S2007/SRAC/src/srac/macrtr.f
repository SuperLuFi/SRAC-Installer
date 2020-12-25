      SUBROUTINE   MACRTR(NMAT,MTNM  ,NISO  ,TEMP  ,XL    ,
     *                         XCDC  ,IDNT  ,DN    ,IXMC  ,IRES ,NEF)
C
      PARAMETER   ( LENWRK = 2800 )
      PARAMETER   ( LNWRK2 = LENWRK*12 )
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
CDEL  PARAMETER   ( MXLISO= 2000 )
      INCLUDE  'BMICRINC'
      INCLUDE  'MATDTINC'
C
      CHARACTER*4     FILENM,NUMB
C
      COMMON /MAINC / IOPT(1000)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
CMOD  COMMON /$D2O  / ID2O
      COMMON /D2OCM / ID2O
      COMMON /PIJ2C / IGT
      COMMON /NEWDAN/ DANNEW(MXLISO)
C
C   ARRAYS SIGML,SIGTL,UPSCTL,AAL,IIL WERE ADDED FOR PL CALCULATION.
C                                               (DECEMBER 1987)
C
      COMMON /WORK/ SIGM(2600) ,SIGM1(2600),SIGML(2600,4),FTAB(6000),
     *              SIGF(48)   ,XNSIGF(48) ,SIGA(48)     ,
     *              UPSCAT(48) ,UPSCT1(48) ,UPSCTL(48,4) ,
     *              SIGT(48)   ,SIGT1(48)  ,SIGTL(48,4)  ,
     *              AA(LENWRK) ,AA1(LENWRK),AAL(LENWRK,4) ,
     *              SFCTR(48)  ,SIGZ(48)   ,XN(49)       ,
     *              DD(2160)   ,BVSIGF(15,48)            ,
     *              DMX1(11)   ,DMX2(11)   ,DMY1(11)     ,DMY2(11)  ,
     *              DMC(11)    ,DMD(11)    ,DME(11)      ,ISWDLY(150),
CM   *              ADATA(2600),BDATA(2600),CDATA(2600)  ,
     *              ADATA(2600,3),
     *              BB(20)     ,IORDER(20) ,
CKK  *              XNUTAB(48,MXNISO)      ,NARRAY(10)   ,BARRAY(20)
     *              XNUTAB(48,MXNISO)      ,NARRAY( 8)   ,BARRAY(22)
     *             ,BLSIGF(15,48),CCC(LNWRK2)
C
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
      CHARACTER*4  MTNM(2,NMAT)
      DIMENSION    NISO(NMAT),TEMP(NMAT),XL(NMAT),XCDC(NMAT)
      CHARACTER*8  IDENT,AIDENT,BIDENT,CIDENT,KIDENT
      CHARACTER*1  LPN(7),LETX
C
      REAL*4       CARRAY(50),SIGTAB(8),TMPTAB(12)
      REAL*4       BDATA(2600),CDATA(2600)
C
      INTEGER*4    IARRAY(50)
CKK   INTEGER*2    IPN(7)
      INTEGER*4    IPN(7)
C
      CHARACTER*4  IDNT(2,1)
      DIMENSION    DN(1),IXMC(1),IRES(1)
C
      DIMENSION    II(LENWRK),II1(LENWRK),IIL(LENWRK,4),DD1(761)
C
      EQUIVALENCE  (II(1)  ,AA(1)),(II1(1),AA1(1)),(IIL(1,1),AAL(1,1))
      EQUIVALENCE  (CARRAY(1),IARRAY(1))
      EQUIVALENCE  (DD(534),DD1(1))
      EQUIVALENCE  (IOPT(37),IUPSCT),(IOPT(39),ISCT)
      EQUIVALENCE  (IOPT(64),NOUT1),(IOPT(56),NET),(IOPT(65),NOUT2)
CMSASA ... GNU FORTRAN 0.5.14 HAS A BUG(?) TO FAIL TO CONNECT
C     IARRAY & EQUIVALENT VARIABLES. CHANGE EQUIVALENCE TO CARRAY.
C     EQUIVALENCE  (IARRAY(1),IFFS),(IARRAY(2),KPN),(IARRAY(3),IFT)
C     EQUIVALENCE  (IARRAY(4),NGMIN),(IARRAY(5),NGMAX)
C     EQUIVALENCE  (IARRAY(6),NSIG ),(IARRAY(7),NTEMP)
      EQUIVALENCE  (CARRAY(1),IFFS),(CARRAY(2),KPN),(CARRAY(3),IFT)
      EQUIVALENCE  (CARRAY(4),NGMIN),(CARRAY(5),NGMAX)
      EQUIVALENCE  (CARRAY(6),NSIG ),(CARRAY(7),NTEMP)
CEND.MSASA 
CKK9  EQUIVALENCE  (IARRAY(10),NTEMPF)
CKK9  EQUIVALENCE  (CARRAY(11),TMPTAB(1)),(CARRAY(21),SIGTAB(1))
      EQUIVALENCE  (CARRAY(09),TMPTAB(1)),(CARRAY(21),SIGTAB(1))
      EQUIVALENCE  (IARRAY(30),LENMEM)
CKK9END
      EQUIVALENCE  (CARRAY(29),XNU)
CKK9ADD
      EQUIVALENCE  (ADATA(1,2),BDATA(1))
      EQUIVALENCE  (ADATA(1,3),CDATA(1))
CKK9END
C
CM    DATA         LPN /1HK,1HK,1HP,1HQ,1HS,1HT,1HU/
      DATA         LPN /'K','K','P','Q','S','T','U'/
C
C-----START OF PROCESS
C
      CALL ICLEA( NARRAY ,  8 , 0 )
      CALL  CLEA( BARRAY , 22 , 0 )
C
      IFLSW     = 1
      FILENM(1) = 'THER'
      FILENM(2) = 'MALU'
      FILENM(3) = '    '
C
      IDENT     = 'THERMAL1'
      DO 10 ITEMP=1,11
      IDENT(8:8) = NUMB(ITEMP) (4:4)
      CALL SEARCH(IDENT,LTH,ISW)
      IF(ISW.EQ.0) GO TO 13
   10 CONTINUE
C
                   WRITE(NOUT1,12)
                   STOP
C
   12 FORMAT(1H0,10X,'*** ANY MEMBER IN FORM OF ''THERMALX'' NOT '
     * ,'FOUND IN USER''S THERMAL LIB')
C
   13 CALL READ(IDENT,II,2*(NET+1))
C---------------------- OUTPUT 'CONTT002' MEMBER TO MACROWRK-FILE
CDEL  IF(IOPT(79).EQ.0) THEN
C---------------------- DELETED BECAUSE RESTART BURNUP CASE ---------
                        FILENM(1) = 'MACR'
                        FILENM(2) = 'OWRK'
                        IDENT     ='CONTT002'
                        CALL SEARCH(IDENT,LTH1,ISW)
                        IF(ISW.NE.0) THEN
                                     CALL WRITE(IDENT,II,2*(NET+1))
                                     ENDIF
CDEL                    ENDIF
C@ADD------------------ OUTPUT 'CONTT002' MEMBER TO MICREF-FILE
      IMCEF    = IOPT(78)
      IF(MOD(IMCEF,2).EQ.1)  THEN
                        FILENM(1) = 'MICR'
                        FILENM(2) = 'EF  '
                        IDENT     ='CONTT002'
                        CALL SEARCH(IDENT,LTH1,ISW)
                        IF(ISW.NE.0) THEN
                                     CALL WRITE(IDENT,II,2*(NET+1))
                                     ENDIF
                        ENDIF
C@END
C---------------------- E BOUNDARIES IN XN
      LOC     = NET+1
      DO 20 N = 1,NET+1
      LOC     = LOC+1
      XN(N)   = AA(LOC)
   20 CONTINUE
C-----CHECK ISCT VALUE
      IF(ISCT.GT.5) THEN
                    WRITE(NOUT1,22) ISCT
                    ISCT = 5
                    ENDIF
C
   22 FORMAT(1H0,10X,'***** ISCT WAS RESET FROM ',I3,' TO  5 . *****'/)
C
CMOD  LGT    = NET*(NET+4)
      LGT    = NET*(NET+5)
      ISCT1  = ISCT-1
      LISO   = 0
      BELL   = 1.200
      IF(IGT.EQ.1)  BELL = 1.40
C ----------------------------------------------------------------------
C ---- MACRO CROSS SECTIONS FOR EACH MATERIALS -------------------------
C ----------------------------------------------------------------------
      DO 1000 MAT=1,NMAT
      IF(NISO(MAT).EQ.0) GO TO 1000
C-----MEMBER CHECK
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      IDENT     = MTNM(1,MAT) // MTNM(2,MAT)
      IDENT(5:5)= 'T'
      IDENT(8:8)= '2'
      CALL SEARCH(IDENT,LGTF0,ISW)
      IF(ISW.EQ.0) THEN
                   LISO = LISO + NISO(MAT)
                   WRITE(NOUT1,25) IDENT
                   GO TO 1000
                   ENDIF
C
      CALL  CLEA( EFFMIC , LENEFF , 0.0 )
C
      FILENM(1) = 'MICR'
      FILENM(2) = 'EF  '
      IDENT     = MTNM(1,MAT) // 'BMIC'
      LENG     = MAXNG*MAXMT3*NISO(MAT)
      CALL READ ( IDENT , EFFMIC , LENG )
C
   25 FORMAT(1H0,10X,'** MEMBER ',A8,' ALREADY EXISTS IN MACRO FILE'
     *  ,' FORMATION SKIPPED **')
C
C --- DEUTERIUM (GAMMA,N) DELAYED NEUTRON
C
      NFAMLY = 6
      CALL  CLEA ( DD , 2160 , 0.0 )
      IF(ID2O.EQ.1) THEN
                    NFAMLY    = 15
                    LENGD     = 21 + 10*NEF
                    FILENM(1) = 'FAST'
                    FILENM(2) = 'U   '
                    CALL READ('YD020000',DD1,LENGD)
                    ENDIF
C
      FILENM(1) = 'THER'
      FILENM(2) = 'MALU'
C
C     NULL NISO VALUE ASSUMES MACRO X-SECTION IS ALREADY COMPOSED
C     CLEAR MACRO STORAGE
C
      CALL  CLEA (XNSIGF,    48  , 0.0 )
      CALL  CLEA (SIGM  ,   LGT  , 0.0 )
      CALL  CLEA (SIGM1 ,   LGT  , 0.0 )
      CALL  CLEA (SIGML , 4*2600 , 0.0 )
      CALL  CLEA (BVSIGF, 15*NET , 0.0 )
      CALL  CLEA (BLSIGF, 15*NET , 0.0 )
      CALL ICLEA (ISWDLY,    150 , 0   )
C
      CALL  CLEA (XNUTAB, 48*MXNISO , 0.0 )
C
C     SUM UP
C
C  MATRIX K P0  COMPONENT
C  MATRIX P P1  COMPONENT
C  MATRIX Q P2  COMPONENT
C  MATRIX S P3  COMPONENT
C  MATRIX T P4  COMPONENT
C  MATRIX U P5  COMPONENT
C
C  ISTATE 1  ONE-DIMENSIONAL DATA
C  ISTATE 2  K ONLY
C  ISTATE 3  K AND P USED
C  ISTATE 4  FROM O TO L-TH PL USED FOR ANISOTROPIC SN(L > 1)
C  (ISTATE 4  N AND P  :  REPLACED BY NEW OPTION  AUGUST 1983 )
C  INTC(2)=1*M+2*K+4*P+8*Q+16*S+32*T+64*U
C  IPN     (1) (2) (3) (4) (5)  (6)  (7)
C          P0  P0  P1  P2  P3   P4   P5
C
C  IST3 MAX VALUE OF ISTATE
C
      IST3     = 0
      NDELAY   = 0
      MORDER   = 0
      RTEMP    = TEMP(MAT)
      XLL      = XL(MAT)
      IF(XLL.LE.0.0)  XLL = 1.0E+20
C ----------------------------------------------------------------------
C -------------MICROSCOPIC CROSS SECTIONS FOR EACH ISOTOPES ------------
C ----------------------------------------------------------------------
      DO 400 ISO = 1,NISO(MAT)
      LISO       = LISO+1
CTFREE
CDEL  IF(DN(LISO).EQ.0.0) GO TO 400
C ---- PSEUDO NON RESONANT NUCLIDE CHECK
      IF(IRES(LISO).EQ.-1)    GO TO 400
      CALL CLEA (CARRAY,    50  , 0.0 )
      IDENT     = IDNT(1,LISO) // IDNT(2,LISO)
      IDENT(1:1)= 'C'
CDEL  IDENT(8:8)= '1'
C
      IEFF      = 0
C
      IF(IRES(LISO).EQ.1) THEN
                          FILENM(1) ='MICR'
                          FILENM(2) ='EF  '
                          IDENT(5:5)='T'
                          CALL SEARCH(IDENT,LGTC,ISW)
C
                          IF (ISW.EQ.0) THEN
                                        IEFF = 1
                                        GO TO 90
                                        ENDIF
C
                          JSW = 1
                          IDENT(6:7) = '00'
                          CALL SEARCH(IDENT,LGTC,JSW)
                          IF (JSW.EQ.0) THEN
                                        IEFF = 1
                                        GO TO 90
                                        ENDIF
      WRITE(NOUT1,*) ' *** MEMBER ',IDENT,' NOT FOUND IN FILE DD=MICREF'
                          ENDIF
C
      FILENM(1)  = 'THER'
      FILENM(2)  = 'MALU'
      IDENT(5:5) = IDNT(2,LISO)(1:1)
      IDENT(6:8) = '000'
      CALL SEARCH(IDENT,LGTC,ISW)
C
   90 CALL READ(IDENT,IARRAY,LGTC)
CWRITE
*     WRITE(6,7001) IDENT
*     WRITE(6,7002) (IARRAY(I),I=1,LGTC)
*     WRITE(6,7003) (CARRAY(I),I=1,LGTC)
*7001 FORMAT(1H ,' ## NUCLID AT MACROT ## ',A8)
*7002 FORMAT(1H ,' ## II (CONT) ## ',10I11)
*7003 FORMAT(1H ,' ## AI (CONT) ## ',1P10E11.4)
C-----TEMPERATURE CHECK
      IF(NTEMP.LE.1) GO TO 145
C
      IF(RTEMP.GT.TMPTAB(NTEMP)) THEN
      WRITE(NOUT1,140) RTEMP,IDNT(1,LISO),IDNT(2,LISO),TMPTAB(NTEMP)
      WRITE(NOUT2,140) RTEMP,IDNT(1,LISO),IDNT(2,LISO),TMPTAB(NTEMP)
                                 ENDIF
C
CKSK  IF(RTEMP.LT.TMPTAB(    1)) THEN
      IF(RTEMP.LT.(TMPTAB(1)-1.0)) THEN
      WRITE(NOUT1,141) RTEMP,IDNT(1,LISO),IDNT(2,LISO),TMPTAB(1)
      WRITE(NOUT2,141) RTEMP,IDNT(1,LISO),IDNT(2,LISO),TMPTAB(1)
                                 ENDIF
C
  140 FORMAT(1H0,' ** TEMPERATURE DESIGNATED ',E12.5,' FOR ',2A4,
     * ' EXCEEDS THE UPPER LIMIT OF TABULATION ',E12.5,' **',/1H ,
     * ' ** LIBRARY CALCULATED AT UPPER TEMPERATURE WILL BE USED **')
  141 FORMAT(1H0,' ** TEMPERATURE DESIGNATED ',E12.5,' FOR ',2A4,
     * ' EXCEEDS THE LOWER LIMIT OF TABULATION ',E12.5,' **',/1H ,
     * ' ** LIBRARY CALCULATED AT LOWER TEMPERATURE WILL BE USED **')
C
  145 CONTINUE
      IKI       = KPN
      DO 151 LL = 1,4
      IPN(LL)   = IKI-2*(IKI/2)
      IKI       = IKI/2
  151 CONTINUE
C
      IF( IOPT(16).EQ.0.AND. ISCT.EQ.0) GO TO 152
C IC16 0 TRNSPORT CORRECTION :1 P1:2:B1:3 ANISOTROPIC SN
      IF(IPN(4).EQ.0) GO TO 152
C  (P2 COMPONENT IN P FOR ANISOTROPIC SN :OPTION ELIMINATED)
C  PL COMPONENT FOR ANISOTROPIC SN (L > 1)
      ISTATE = 4
      LETX   = LPN(2)
      GO TO 159
  152 IF(IPN(3).EQ.0) GO TO 154
      IF(IPN(2).EQ.0) GO TO 155
C  K P  FOR CONSISTENT PN
      ISTATE = 3
      LETX   = LPN(2)
      GO TO 159
  154 IF(IPN(1).EQ.1) GO TO 155
C  K
      ISTATE = 2
      LETX   = LPN(2)
      GO TO 159
C  K  P0 WITH TRANSPORT CROSS SECTION CORRECTED
  155 ISTATE = 1
      LETX   = LPN(1)
C
  159 IST3   = MAX0(IST3,ISTATE)
C
*     WRITE(6,7041) KPN,IST3,ISTATE,IPN
*7041 FORMAT(1H ,' ## KPN IST3 ISTATE IPN ## ',10I10)
C
      IDENT(1:1) = LETX
C-----INTERPOLATE DESIRED TEMPERATURE DATA FORM THERMALU LIBRARY
      IF(IEFF.EQ.0) IDENT(8:8) = '1'
C
      AIDENT     =  '        '
      BIDENT     =  '        '
      CIDENT     =  '        '
      ATEMPT     = 0.0
      BTEMP      = 0.0
      CTEMP      = 0.0
      ITLOOP     = 0
      ITPOS      = 0
C
      IF(NTEMP.EQ.1) THEN
                     ITLOOP = 1
                     AIDENT = IDENT
                     ITPOS  = 1
                     ENDIF
C
      IF(NTEMP.EQ.2) THEN
                     ITLOOP = 2
                     AIDENT = IDENT
                     BIDENT = IDENT(1:7) // '2'
                     ATEMP  = TMPTAB(1)
                     BTEMP  = TMPTAB(2)
                     ITPOS  = 1
                     IF(RTEMP.LE.ATEMP) THEN
                                        ITLOOP = 1
                                        ENDIF
                     IF(RTEMP.GE.BTEMP) THEN
                                        ITLOOP = 1
                                        ITPOS  = 2
                                        AIDENT = BIDENT
                                        ENDIF
                     ENDIF
C
      IF(NTEMP.EQ.3) THEN
                     ITPOS  = 1
                     IF(RTEMP.LE.TMPTAB(1)) THEN
                                            ITLOOP = 1
                                            AIDENT = IDENT
                                            ENDIF
                     IF(RTEMP.EQ.TMPTAB(2)) THEN
                                            ITLOOP = 1
                                            ITPOS  = 2
                                            AIDENT = IDENT(1:7)//'2'
                                            ENDIF
                     IF(RTEMP.GE.TMPTAB(3)) THEN
                                            ITLOOP = 1
                                            ITPOS  = 3
                                            AIDENT = IDENT(1:7)//'3'
                                            ENDIF
                     IF(ITLOOP.EQ.0) THEN
                                     ITLOOP = 3
                                     AIDENT = IDENT
                                     BIDENT = IDENT(1:7)// '2'
                                     CIDENT = IDENT(1:7)// '3'
                                     ATEMP  = TMPTAB(1)
                                     BTEMP  = TMPTAB(2)
                                     CTEMP  = TMPTAB(3)
                                     ENDIF
                     ENDIF
C
      IF(NTEMP.GT.3) THEN
                     IF(RTEMP.LE.TMPTAB(1)) THEN
                                            ITLOOP = 1
                                            ITPOS  = 1
                                            AIDENT = IDENT
                                            ENDIF
                     IF(RTEMP.GE.TMPTAB(NTEMP)) THEN
                                                ITLOOP = 1
                                                ITPOS  = NTEMP
                          AIDENT = IDENT(1:7)// NUMB(NTEMP) (4:4)
                                                ENDIF
                    ENDIF
C
      IF(ITLOOP.GT.0) GO TO 174
                      DO 163 LOP = 1 ,NTEMP
                      BB(LOP)    = ABS( RTEMP - TMPTAB(LOP))
                      IORDER(LOP)= LOP
  163                 CONTINUE
C
*                     WRITE(6,7011) (BB(I),I=1,NTEMP)
C
                      DO 165   I = 1     , NTEMP - 1
                      SMALL      = BB(I)
                      DO 164   J = I + 1 , NTEMP
                      IF(BB(J).LT.SMALL) THEN
                                         ISAVE     = IORDER(I)
                                         BB(I)     = BB(J)
                                         IORDER(I) = IORDER(J)
                                         BB(J)     = SMALL
                                         IORDER(J) = ISAVE
                                         SMALL     = BB(I)
                                         ENDIF
  164                 CONTINUE
  165                 CONTINUE
C
*                     WRITE(6,7012) (BB(I),I=1,NTEMP)
*                     WRITE(6,7013) (IORDER(I),I=1,NTEMP)
C
                      IF(BB(1).LT.0.1) THEN
                                       ITLOOP = 1
                                       IST    = IORDER(1)
                                       ITPOS  = IST
                                    AIDENT = IDENT(1:7)//NUMB(IST)(4:4)
                                       ATEMP  = TMPTAB(IST)
                                       GO TO 174
                                       ENDIF
C
                      DO 167 I = 1 , 2
                      IMIN     = IORDER(I)
                      DO 167 J = I+1 , 3
                      IF(IORDER(J).LT.IMIN) THEN
                                           IORDER(I) = IORDER(J)
                                           IORDER(J) = IMIN
                                           IMIN      = IORDER(I)
                                           ENDIF
  167                 CONTINUE
*                     WRITE(6,7014) (IORDER(I),I=1,3)
C
                      ITLOOP = 3
                      IST    = IORDER(1)
                      IF(IST.GE.NTEMP-1) IST = NTEMP - 2
                      ITPOS  = IST
                      AIDENT = IDENT (1:7) // NUMB(IST) (4:4)
                      ATEMP  = TMPTAB(IST)
                      IST    = IST + 1
                      BIDENT = IDENT (1:7) // NUMB(IST) (4:4)
                      BTEMP  = TMPTAB(IST)
                      IST    = IST + 1
                      CIDENT = IDENT (1:7) // NUMB(IST) (4:4)
                      CTEMP  = TMPTAB(IST)
C
*                     WRITE(6,7015) AIDENT,BIDENT,CIDENT
*                     WRITE(6,7016) ATEMP ,BTEMP , CTEMP
C
 7011 FORMAT(1H ,' ## BB (BEFORE) ## ',1P10E11.4)
 7012 FORMAT(1H ,' ## BB (AFTER ) ## ',1P10E11.4)
 7013 FORMAT(1H ,' ## IOR(BEFORE) ## ',10I11)
 7014 FORMAT(1H ,' ## IOR(AFTER ) ## ',10I11)
 7015 FORMAT(1H ,' ## IDENT ## ',A8,2X,A8,2X,A8)
 7016 FORMAT(1H ,' ## TEMP  ## ',3F8.2)
 7018 FORMAT(1H ,' ## XNUTAB## ',1P10E11.4)
C
  174 CONTINUE
      IF(IEFF.EQ.0) IDENT(8:8) = '0'
CDEL  CALL GETLEN(IDENT,LENGTH)
CDEL  L1         = LGT-LENGTH+1
C
      LENGTH     = NTEMP*LENMEM
      L1         = 1
      IF(KPN.EQ.0) L1 = NET*NET + 1
      CALL READ (IDENT,CCC,LENGTH)
C
      L0         = L1 - 1
      CALL CLEA ( AA , LGT , 0.0 )
C
      IF(ITLOOP.EQ.1) THEN
CDEL                  CALL READ(AIDENT,AA(L1),LENGTH)
                      ISW       = LENMEM*(ITPOS-1)
                      DO 7010 I = 1 , LENMEM
                      AA(L0+I)  = CCC(ISW+I)
 7010                 CONTINUE
C
                      ELSE
                      ISW         = LENMEM*(ITPOS-1)
                      DO 7020  IT = 1 , ITLOOP
                      DO 7020   I = 1 , LENMEM
                      ISW         = ISW + 1
                      ADATA(I,IT) = CCC(ISW)
 7020                 CONTINUE
C
                      CALL MACRIN(AA(L1),LENMEM,RTEMP,ITLOOP,NET ,
     1                            AIDENT,BIDENT,CIDENT,
     2                            ATEMP ,BTEMP ,CTEMP ,
     3                            ADATA ,BDATA ,CDATA        )
                      ENDIF
C
C@MOD IF(IEFF.EQ.0) THEN
      IF(IEFF.EQ.0.AND.MOD(IXMC(LISO),2).EQ.1) THEN
                    KIDENT       = IDNT(1,LISO) // IDNT(2,LISO)
                    KIDENT (5:5) = 'T'
            IF(DN(LISO).EQ.0.0.OR.IFT.EQ.0) THEN
C
                    IFLSW        = 1
                    FILENM(1)    = 'MICR'
                    FILENM(2)    = 'EF  '
                    KIDENT (1:1) = 'C'
                    NARRAY (1)   = IFFS
                    NARRAY (2)   = 2
                    IF(KPN.GT.4)  NARRAY (2) = 6
                    ISTO         = 1
                    LENGO        = NET*(NET+5)
                    NARRAY (7)   = 1
                    BARRAY (1)   = RTEMP
                    BARRAY (21)  = XNU
                    IF(KPN.EQ.0) THEN
                                 NARRAY(2) = 0
                                 ISTO      = ISTO + NET*NET
                                 LENGO     = 5*NET
                                 ENDIF
                    NARRAY (30)  = LENGO
C
                    CALL SEARCH ( KIDENT , LEN , ISW )
                    IF(ISW.EQ.1) THEN
                                 CALL WRITE( KIDENT , NARRAY ,  30 )
                                 KIDENT (1:1) = 'K'
                                 CALL WRITE( KIDENT , AA(ISTO) , LENGO )
                                 ENDIF
C
                    FILENM(1)    = 'THER'
                    FILENM(2)    = 'MALU'
C
                    ELSE
                    KIDENT(6:8) = MTNM(2,MAT)(2:3) // IDNT(2,LISO)(4:4)
                    ENDIF
*                   WRITE(6,*) ' ** IEFF KIDENT : ',IEFF,KIDENT
C@END
                    ENDIF
C
      IF(IFFS.EQ.1) THEN
                    LOCNU    = LGT - NET
                    DO 185 I = 1 , NET
                    XNUTAB(I,ISO) = AA(LOCNU + I)
  185               CONTINUE
*                   WRITE(6,7018) (XNUTAB(I,ISO),I=1,NET)
                    ENDIF
C
CTF   IF(ISTATE.LE.2.AND.ISCT.LE.0) GO TO  180
C
      IDENT(1:1)  = LPN(3)
      AIDENT(1:1) = LPN(3)
      BIDENT(1:1) = LPN(3)
      CIDENT(1:1) = LPN(3)
C
      CALL SEARCH(IDENT, LLTH , ISW )
      CALL CLEA( AA1   ,  LGT , 0.0 )
C
      IF(ISW.EQ.0) THEN
                   CALL READ( IDENT , CCC , LLTH )
                   IF(ITLOOP.EQ.1) THEN
CDEL                               CALL READ( AIDENT , AA1 , LGT )
                                   ISW       = LENMEM*(ITPOS-1)
                                   DO 8010 I = 1 , LENMEM
                                   AA1(L0+I) = CCC(ISW+I)
 8010                              CONTINUE
C
                       ELSE
                       ISW         = LENMEM*(ITPOS-1)
                       DO  8020 IT = 1 , ITLOOP
                       DO  8020 I  = 1 , LENMEM
                       ISW         = ISW + 1
                       ADATA(I,IT) = CCC(ISW)
 8020                  CONTINUE
C
                       CALL MACRIN(AA1(L1),LENMEM,RTEMP ,ITLOOP,NET ,
     1                             AIDENT ,BIDENT,CIDENT,
     2                             ATEMP  ,BTEMP ,CTEMP ,
     3                             ADATA  ,BDATA ,CDATA        )
                      ENDIF
C
                   MORDER = MAX0(MORDER,1)
C@ADD
                   IF(IEFF.EQ.0.AND.MOD(IXMC(LISO),2).EQ.1) THEN
                    IFLSW        = 1
                    FILENM(1)    = 'MICR'
                    FILENM(2)    = 'EF  '
                    KIDENT (1:1) = 'P'
                    CALL SEARCH ( KIDENT , LEN , JSW )
                    IF(JSW.EQ.1) THEN
                                 CALL WRITE( KIDENT , AA1 , LGT )
                                 ENDIF
C
                    FILENM(1)    = 'THER'
                    FILENM(2)    = 'MALU'
                    ENDIF
C
                   ENDIF
C
C-----READ PL DATA FROM THERMALU
C
      IF(ISCT.GT.1) THEN
                    DO  175 L  = 1,ISCT1
                    LL         = L+3
                    IDENT(1:1) = LPN(LL)
                    AIDENT(1:1)= LPN(LL)
                    BIDENT(1:1)= LPN(LL)
                    CIDENT(1:1)= LPN(LL)
                    CALL SEARCH( IDENT    , LLTH , ISW )
                    CALL CLEA  ( AAL(1,L) , LGT  , 0.0 )
C
                    IF (ISW.EQ.0) THEN
                                  CALL READ( IDENT , CCC , LLTH )
C
                        IF(ITLOOP.EQ.1) THEN
CDEL                                   CALL READ(AIDENT,AAL(1,L),LGT)
                                       ISW        = LENMEM*(ITPOS-1)
                                       DO 9010  I = 1 , LENMEM
                                       AAL(L0+I,L)= CCC(ISW+I)
 9010                                  CONTINUE
C
                                  ELSE
                                  ISW         = LENMEM*(ITPOS-1)
                                  DO 9020  IT = 1 , ITLOOP
                                  DO 9020  I  = 1 , LENMEM
                                  ISW         = ISW + 1
                                  ADATA(I,IT) = CCC(ISW)
9020                              CONTINUE
C
                       CALL MACRIN(AAL(L1,L),LENMEM,RTEMP ,ITLOOP,NET ,
     1                             AIDENT   ,BIDENT,CIDENT,
     2                             ATEMP    ,BTEMP ,CTEMP ,
     3                             ADATA    ,BDATA ,CDATA        )
                                   ENDIF
C
                                  MORDER = MAX0(MORDER,L + 1)
                                  ENDIF
  175               CONTINUE
                    ENDIF
C
  180 CONTINUE
C
C-----STORE EFFECTIVE MICRO SCOPIC CROSS SECTION
C
      LOC1 = NET*(NET+1)
      LOC2 = NET*(NET+3)
      DO 190 I = 1 , NET
      EFFMIC(NEF+I,1,ISO) = AA(LOC1+I)
      EFFMIC(NEF+I,2,ISO) = AA(LOC2+I)
      EFFMIC(NEF+I,6,ISO) = AA(LOC2+I)*XNUTAB(I,ISO)
  190 CONTINUE
C
C-----CALCULATE INFINITE MACROSCOPIC CROSS SECTION
C
CTFREE
      IF(DN(LISO).EQ.0.0) GO TO 400
CADD END
      DO 200 L = 1 , LGT
      SIGM(L)  = SIGM(L) + AA(L)*DN(LISO)
      IF(ISTATE.GT.2)  SIGM1(L) = SIGM1(L) + 3.0*AA1(L)*DN(LISO)
  200 CONTINUE
C
      IF(ISCT.GT.1) THEN
                    DO 205 LL  = 1 , ISCT1
                    LODD       = 2*LL + 3
                    DO 205 L   = 1 , LGT
  205               SIGML(L,LL)=SIGML(L,LL)+LODD*AAL(L,LL)*DN(LISO)
                    ENDIF
C
C **  FORMATION OF ONE DIMENSIONAL REACTION **
C
      IF(IFFS.EQ.0) GO TO 400
                    FILENM(1)  = 'FAST'
                    FILENM(2)  = 'U   '
                    IDENT(1:1) = 'Y'
                    IDENT(5:8) = '0000'
                    IDELAY     = 0
                    CALL SEARCH(IDENT,LENGY,ISW1)
                    IF (ISW1.EQ.0) THEN
                                   CALL READ(IDENT,DD,LENGY)
                                   IDELAY      = 1
                                   NDELAY      = 1
                                   ISWDLY(ISO) = 1
                                   ENDIF
C
                    FILENM(1) ='THER'
                    FILENM(2) ='MALU'
C
                    LOC      = LGT-2*NET
                    DO 300 N = 1 , NET
                    LOC      = LOC + 1
CMOD                XNSIGF(N)=XNSIGF(N)+ XNU*AA(LOC)*DN(LISO)
                    XNSIGF(N)=XNSIGF(N)+ XNUTAB(N,ISO)*AA(LOC)*DN(LISO)
                    IF(IDELAY.EQ.0) GO TO 300
                    DO 220 IG=1,6
          BVSIGF(IG,N)=BVSIGF(IG,N)+DD(8+IG)*DD(15+NEF)*AA(LOC)*DN(LISO)
          BLSIGF(IG,N)=BLSIGF(IG,N)+DD(8+IG)*DD(15+NEF)*AA(LOC)*DN(LISO)
     +                /DD(2+IG)
  220               CONTINUE
                        IF(NFAMLY.EQ.6) GO TO 300
                        DO 225 IG    = 7,15
                        BVSIGF(IG,N) = BVSIGF(IG,N) +
     +                  DD1(5+IG)*DD(15+NEF)*AA(LOC)*DN(LISO)
                        BLSIGF(IG,N) = BLSIGF(IG,N) +
     +                  DD1(5+IG)*DD(15+NEF)*AA(LOC)*DN(LISO)/DD1(IG-4)
  225                   CONTINUE
  300               CONTINUE
  400 CONTINUE
C
C
C
      LOC1     = NET*NET
      LOC2     = NET*(NET+1)
      LOC3     = NET*(NET+2)
      LOC4     = NET*(NET+3)
      DO 430 N = 1,NET
      LOCGG    = (N-1)*NET + N
      LOC1     = LOC1+1
      LOC2     = LOC2+1
      LOC3     = LOC3+1
      LOC4     = LOC4+1
      SIGA(N)  = SIGM(LOC2)+SIGM(LOC4)
      SIGF(N)  = SIGM(LOC4)
      SIGT(N)  = SIGM(LOC3)
      SIGT1(N) = SIGM1(LOC3)
C
      IF(ISCT.GT.1) THEN
                    DO 405 L   = 1,ISCT1
  405               SIGTL(N,L) = SIGML(LOC3,L)
                    ENDIF
C *** IF IUPSCT=1  ASSUME UPSCATER AS SELF-SCATTER
      IF(IUPSCT.NE.0) THEN
                      UPSCAT(N)    = 0.0
                      SIGM(LOCGG)  = SIGM(LOCGG)+SIGM(LOC1)
                      UPSCT1(N)    = 0.0
                      SIGM1(LOCGG) = SIGM1(LOCGG)+SIGM1(LOC1)
C
                      IF(ISCT.GT.1) THEN
                            DO 412 L      = 1,ISCT1
                            UPSCTL(N,L)   = 0.0
  412                       SIGML(LOCGG,L)=SIGML(LOCGG,L)+SIGML(LOC1,L)
                                    ENDIF
C
                    ELSE
                    UPSCAT(N) = SIGM(LOC1)
                    UPSCT1(N) = SIGM1(LOC1)
                    IF(ISCT.GT.1) THEN
                                  DO 425    L = 1,ISCT1
  425                             UPSCTL(N,L) = SIGML(LOC1,L)
                                  ENDIF
                    ENDIF
  430 CONTINUE
C ----------------------------------------------------------------------
C     STEP FOR SELF-SHIELDING  501-600
C     CORRECTION TO SIGT,SIGF,SIGA
C ----------------------------------------------------------------------
      FILENM(1)  = 'THER'
      FILENM(2)  = 'MALU'
      LISO       = LISO-NISO(MAT)
C
      DO 600 ISO = 1,NISO(MAT)
      LISO       = LISO+1
      IF(DN(LISO).EQ.0.0) GO TO 600
C ---- OBSOLUTE OPTION FOR PSEUDO RESONANT NUCLIDE
      IF(IRES(LISO).EQ.-1)    GO TO 600
C ---- EFFECTIVE  CROSS SECTION IN MICREF
      IF(IRES(LISO).EQ. 1)    GO TO 600
C
      IDENT      = IDNT(1,LISO) // IDNT(2,LISO)
      IDENT(1:1) = 'C'
      IDENT(6:8) = '000'
      CALL SEARCH(IDENT,LGTC,ISW)
      CALL READ(IDENT,CARRAY,LGTC)
      IF(IFT.EQ.0) GO TO 600
C
CKK9  IDENT(8:8) = '1'
      NTEMPF     = NTEMP
C === READ K-MATRIX TO EXTRACT TOTAL CROSS SECTION
      IDENT(1:1) = 'K'
CMOD  CALL READ(IDENT,AA,LGT)
C === FATAL ERROR FOUND FOR KPN=0 NUCLIDE WITH F-TABLE =============
C === MODIFIED THE LENGTH OF K-MATRIX FOR KPN=0 CASE ==9/9/1996=====
      IST0     = 1
      LENG0    = LGT
      IF(KPN.EQ.0) THEN
                   IST0  = NET*NET + 1
                   LENG0 = LGT - NET*NET
                   ENDIF
      CALL CLEA(AA   ,LGT , 0.0 )
      CALL READ(IDENT,AA(IST0),LENG0)
C ==================================================================
C
      LOCT     = NET*(NET+2)
C-----CALCULATE HETEROGENEITY CORRECTION TERM
      DANCOF   = DANNEW(LISO)
      SS       = (BELL/(1.0+(BELL-1.0)*DANCOF))*(1.0-DANCOF)/ XLL
C
*     SSS      = SS / DN(LISO)
CWRITE
*     WRITE(6,7005) IDENT,LISO,DANCOF,XLL,BELL,DN(LISO),SS,SSS
*7005 FORMAT(1H ,' ## IDENT LISO DANCOF XLL BELL DN SS SSS ## ',
*    +      /1H ,2X,A8,I8,1P7E12.5)
C-----CALCULATE SIGMA-0 VALUE
      DNTMP    = DN(LISO)
CMOD  IF(DNTMP.LE.1.0E-50) THEN
      IF(DNTMP.LE.1.0E-30) THEN
                           CALL  CLEA( SIGZ , 48 , 1.0E+10 )
                           ELSE
                           DNTMP = 1.0 / DNTMP
      DO 510 N = 1,NET
      LOCT     = LOCT+1
CM    SIGZ(N)  = (SIGT(N)-DN(LISO)*AA(LOCT))/DN(LISO) + SS/DN(LISO)
      SIGZ(N)  =  SIGT(N)*DNTMP -  AA(LOCT) +  SS*DNTMP
  510 CONTINUE
                           ENDIF
CWRITE
*     WRITE(6,7006) (SIGZ(N),N=1,NET)
*7006 FORMAT(1H ,' ## SIG0 ## ',1P10E11.4)
C
      IDENT(1:1) = 'F'
      IDENT(8:8) = 'C'
      LGTF       = NSIG*NET*NTEMPF
CWRITE
*     WRITE(6,7017) IDENT,NSIG,NET,NTEMPF,LGTF
*7017 FORMAT(1H ,' ## IDENT NSIG NET NTEMPF LGTF ## ',A8,5I6)
CEND
      IF(LGTF.GT.6000) THEN
            WRITE(NOUT1,*) ' ** FATAL PROGRAMING ERROR AT MACRTR !!! '
            WRITE(NOUT1,*) ' ** SHORTAGE OF F-TABLE ARRAY SIZE   !!! '
            WRITE(NOUT2,*) ' ** FATAL PROGRAMING ERROR AT MACRTR !!! '
            WRITE(NOUT2,*) ' ** SHORTAGE OF F-TABLE ARRAY SIZE   !!! '
            STOP   999
            ENDIF
C
      CALL SEARCH(IDENT,LNOW,ISW )
      IF(ISW.GT.0) THEN
                   WRITE(NOUT1,*) ' MEMBER(',IDENT,') IS NOT FOUND IN '
     +                           ,'USER THERMAL LIBRARY ]] '
                   GO TO 521
                   ENDIF
C
      CALL READ  (IDENT,FTAB,LGTF)
C
C     CALL SPLINE FOR CAPTURE
C
      STEMP = RTEMP
      IF(RTEMP.LT.TMPTAB(1)     ) STEMP = TMPTAB(1)
      IF(RTEMP.GT.TMPTAB(NTEMPF)) STEMP = TMPTAB(NTEMPF)
C
      CALL SPLINE(FTAB  ,SFCTR  ,SIGZ  ,STEMP ,SIGTAB,TMPTAB,
     *            DMX1  ,DMX2   ,DMY1  ,DMY2  ,DMC   ,DMD   ,
     *            DME   ,NSIG   ,NTEMPF,11    ,NET   ,NTEMPF,
     *            NSIG  ,NGMIN  ,NGMAX                        )
CWRITE
*     WRITE(6,7007) (SFCTR(N),N=1,NET)
*7007 FORMAT(1H ,' ## SSF-CAP ## ',1P10E11.4)
C
      LOCT     = NET*(NET+2)
      LOCA     = NET*(NET+1)
      DO 520 N = 1,NET
      LOCA     = LOCA+1
      LOCT     = LOCT+1
C
      XX       = (1.-SFCTR(N))*DN(LISO)*AA(LOCA)
      AA(LOCT) = AA(LOCT)-AA(LOCA)*(1.-SFCTR(N))
CM    AA(LOCA) = AA(LOCA)*SFCTR(N)
      SAVE     = AA(LOCA)*SFCTR(N)
      AA(LOCA) = SAVE
      SIGA(N)  = SIGA(N)-XX
      SIGT(N)  = SIGT(N)-XX
      EFFMIC(NEF+N,1,ISO) = SAVE
C
  520 CONTINUE
CWRITE
      LOCA     = NET*(NET+1)
*     WRITE(6,7008) (AA(LOCA+N),N=1,NET)
*7008 FORMAT(1H ,' ## SIGC-EF ## ',1P10E11.4)
C
C     CALL SPLINE FOR FISSION
C
  521 CONTINUE
      IF(IFFS.EQ.0) GO TO 535
      IDENT(8:8) = 'F'
C
      CALL SEARCH(IDENT,LNOW,ISW )
      IF(ISW.GT.0) THEN
                   WRITE(NOUT1,*) ' MEMBER(',IDENT,') IS NOT FOUND IN '
     +                           ,'USER THERMAL LIBRARY ]] '
                   GO TO 535
                   ENDIF
C
      LGTF       = NSIG*NET*NTEMPF
      IF(LGTF.GT.6000) THEN
            WRITE(NOUT1,*) ' ** FATAL PROGRAMING ERROR AT MACRTR !!! '
            WRITE(NOUT1,*) ' ** SHORTAGE OF F-TABLE ARRAY SIZE   !!! '
            WRITE(NOUT2,*) ' ** FATAL PROGRAMING ERROR AT MACRTR !!! '
            WRITE(NOUT2,*) ' ** SHORTAGE OF F-TABLE ARRAY SIZE   !!! '
            STOP   999
            ENDIF
C
      CALL READ(IDENT,FTAB,LGTF)
C
      CALL SPLINE(FTAB  ,SFCTR  ,SIGZ  ,STEMP ,SIGTAB,TMPTAB,
     *            DMX1  ,DMX2   ,DMY1  ,DMY2  ,DMC   ,DMD   ,
     *            DME   ,NSIG   ,NTEMPF,11    ,NET   ,NTEMPF,
     *            NSIG  ,NGMIN  ,NGMAX                        )
CWRITE
*     WRITE(6,7009) (SFCTR(N),N=1,NET)
*7009 FORMAT(1H ,' ## SSF-FIS ## ',1P10E11.4)
C
      LOCA      = NET*(NET+3)
      LOCT      = NET*(NET+2)
      LOCNU     = NET*(NET+4)
      IDELAY    = ISWDLY(ISO)
C
      DO 530 N  = 1,NET
      LOCA      = LOCA + 1
      LOCT      = LOCT + 1
      LOCNU     = LOCNU+ 1
C
      XX        = (1.-SFCTR(N))*DN(LISO)*AA(LOCA)
      AA(LOCT)  = AA(LOCT) - AA(LOCA)*(1.-SFCTR(N))
CM    AA(LOCA)  = AA(LOCA)*SFCTR(N)
      SAVE      = AA(LOCA)*SFCTR(N)
      AA(LOCA)  = SAVE
      AA(LOCNU) = XNUTAB(N,ISO)
      SIGF(N)   = SIGF(N)  - XX
      SIGA(N)   = SIGA(N)  - XX
      SIGT(N)   = SIGT(N)  - XX
CMOD  XNSIGF(N) = XNSIGF(N)- XX*XNU
      XNSIGF(N) = XNSIGF(N)- XX*XNUTAB(N,ISO)
      EFFMIC(NEF+N,2,ISO) = SAVE
      EFFMIC(NEF+N,6,ISO) = SAVE*XNUTAB(N,ISO)
C
      IF(IDELAY.EQ.1) THEN
                      DO 525 IG=1,6
              BVSIGF(IG,N)=BVSIGF(IG,N)-XX*DD(8+IG)*DD(15+NEF)
              BLSIGF(IG,N)=BLSIGF(IG,N)-XX*DD(8+IG)*DD(15+NEF)/DD(2+IG)
  525                 CONTINUE
                      IF(NFAMLY.EQ.6) GO TO 530
                      DO 526 IG= 7,15
            BVSIGF(IG,N)=BVSIGF(IG,N)-XX*DD1(5+IG)*DD(15+NEF)
            BLSIGF(IG,N)=BLSIGF(IG,N)-XX*DD1(5+IG)*DD(15+NEF)/DD1(IG-4)
  526                 CONTINUE
                      ENDIF
  530 CONTINUE
CWRITE
      LOCA     = NET*(NET+3)
*     WRITE(6,7019) (AA(LOCA+N),N=1,NET)
*7019 FORMAT(1H ,' ## SIGF-EF ## ',1P10E11.4)
C
  535 IF(MOD(IXMC(LISO),2).LE.0) GO TO 600
C     WRITE ONE-DIMENSIONAL DATA ONLY INTO MICROEF FILE BY K-MATRIX
      FILENM(1)  = 'MICR'
      FILENM(2)  = 'EF  '
      IDENT(6:8) = MTNM(2,MAT) (2:3) // IDNT(2,LISO) (4:4)
      IDENT(5:5) = 'T'
      IDENT(1:1) = 'C'
      CALL ICLEA (IARRAY(3) ,  6 , 0   )
      CALL  CLEA (SIGTAB    ,  8 , 0.0 )
      CALL  CLEA (TMPTAB    , 12 , 0.0 )
      NTEMP      = 1
      TMPTAB(1)  = RTEMP
      ISTO       = 1
      LENGO      = NET*(NET+5)
      IF(KPN.LE.0) THEN
                   KPN    = 0
                   ISTO   = ISTO + NET*NET
                   LENGO  = 5*NET
                   ENDIF
      IF(KPN.EQ.1) KPN    = 2
      IF(KPN.GT.4) KPN    = 6
      LENMEM     = LENGO
CWRITE
*     WRITE(6,7004) IDENT
*     WRITE(6,7002) (IARRAY(I),I=1,LGTC)
*     WRITE(6,7003) (CARRAY(I),I=1,LGTC)
*7004 FORMAT(1H ,' ## NUCLID AT MICREF ## ',A8)
C
      CALL SEARCH(IDENT , LEN , ISW )
      IF(ISW.EQ.1) THEN
                   CALL WRITE( IDENT , CARRAY ,LGTC )
C                  SAVE STRAGE FOR K-MATRIX
                   IDENT(1:1) = 'K'
                   CALL WRITE(IDENT,AA(ISTO),LENGO)
                   ELSE
                   WRITE(NOUT2,302) IDENT,MTNM(1,MAT),MTNM(2,MAT)
                   ENDIF
C
  302 FORMAT(1H ,'  WARNING ----> ',A8,'(',2A4,') ALREADY EXISTS IN ',
     &'MICREF-FILE. OUTPUT OF EFFECTIVE CROSS SECTION IS SKIPPED ]] ')
C@END
      FILENM(1)  = 'THER'
      FILENM(2)  = 'MALU'
  600 CONTINUE
C
C **  FORMATION OF TWO DIMENSIONAL DATA INTO ONE-DIMENSIONAL ARRAY
C
      LOC  = 0
      LOCB = 0
      LOCC = NET*(NET+1)
      LOCX = NFAMLY*NET
      CALL CLEA (AA1 ,LENWRK ,0.0)
      CALL CLEA (AAL ,4*LENWRK,0.0)
C
      DO 800 N  = 1,NET
      LOCC      = LOCC+1
      AA(LOC+3) = 1./SQRT((XN(N)+XN(N+1))/2.)
      AA(LOC+4) = SIGF(N)
      AA(LOC+5) = XNSIGF(N)
      AA(LOC+6) = SIGT(N)
CTF   IF(IOPT(16).EQ.0 .AND.ISCT.EQ.0) AA(LOC+6) = SIGT(N)-SIGT1(N)/3.
C     IF(ISCT.EQ.1.AND.IST3.EQ.4) AA(LOC+6)=SIGT(N)-SIGM1(LOCC)/3.
      AA1(LOC+6)=SIGT1(N)
C
      IF(ISCT.GT.1) THEN
                    DO 620  L    = 1,ISCT1
  620               AAL(LOC+6,L) = SIGTL(N,L)
                    ENDIF
C
      AA(LOC +7)  = 0.
      AA(LOC +8)  = 0.33333333 / ( SIGT(N) - 0.33333333*SIGT1(N) )
      AA(LOC +9)  = AA(LOC+8)
      AA(LOC+10)  = SIGA(N)
      IFUP        = 0
C **  NON-ZERO SCATTERING ELEMENT **
      LD          = NET*(N-1)
C *** CORRECTION TO SELF-SCATTER
CTF   IF (IOPT(16).EQ.0.AND.ISCT.EQ.0) SIGM(LD+N)=SIGM(LD+N)-SIGT1(N)/3.
C     IF (ISCT.EQ.1.AND.IST3.EQ.4) SIGM(LD+N)=SIGM(LD+N)-SIGM1(LOCC)/3.
      IF(UPSCAT(N).EQ.0.0) THEN
                           DO 650 ND = 1,N
                           LD        = LD + 1
                           IF(SIGM(LD).NE.0.) GO TO 670
  650                      CONTINUE
C ************************* CASE FOR NON-ZERO UPSCATTERING **
                           ELSE
                           AA (LOC+11) = UPSCAT(N)
                           AA1(LOC+11) = UPSCT1(N)
                           IF(ISCT.GT.1) THEN
                                         DO 664 L=1,ISCT1
  664                                    AAL(LOC+11,L)=UPSCTL(N,L)
                                         ENDIF
C
                           LD    = LD + 1
                           IFUP  = 1
                           ENDIF
C **  FIND TAIL OF SLOWING-DOWN
  670 LDD       =  NET*N + 1
      DO 680 ND =  N , NET
      LDD       = LDD-1
      IF(SIGM(LDD).NE.0.) GO TO 690
  680 CONTINUE
  690 II(LOC+1) = N  + NET*(N-1) - LD + 1 + IFUP
      II(LOC+2) = LDD-LD+1 + IFUP
      II1(LOC+1)= II(LOC+1)
      II1(LOC+2)= II(LOC+2)
C
      IF(ISCT.GT.1) THEN
                    DO 692     L = 1,ISCT1
                    IIL(LOC+1,L) = II(LOC+1)
  692               IIL(LOC+2,L) = II(LOC+2)
                    ENDIF
C
      LTH  = II(LOC+2) - IFUP
      LOC  = LOC  + 10 + IFUP
      LD   = LD-1
      DO 700 L = 1 , LTH
      LOC      = LOC + 1
      LD       = LD+1
      AA (LOC) = SIGM (LD)
      AA1(LOC) = SIGM1(LD)
C
      IF(ISCT.GT.1) THEN
                    DO 696  LL  = 1,ISCT1
  696               AAL(LOC,LL) = SIGML(LD,LL)
                    ENDIF
  700 CONTINUE
C
      IF (NDELAY.EQ.0) GO TO 800
      DO 710 IG = 1,NFAMLY
      LOCB      = LOCB+1
      LOCX      = LOCX+1
      DD(LOCB)  = BVSIGF(IG,N)
      DD(LOCX)  = 0.0
      LOCL      = LOCX + NET*NFAMLY
      DD(LOCL)  = BLSIGF(IG,N)
  710 CONTINUE
C
  800 CONTINUE
C ----------------------------------------------------------------------
      LTH        = LOC
  950 IDENT      = MTNM(1,MAT) // MTNM(2,MAT)
      IDENT(5:5) = 'T'
      IDENT(8:8) = '4'
CTF   IF(IST3.LT.2.OR.(IOPT(16).EQ.0.AND.ISCT.EQ.0)) THEN
CTF                                                  IDENT(8:8) = '2'
CTF                                                  ENDIF
C
C   LAST CHARACTER 4:P0  3:P1 2:TRANSPORT CORRECTED
C     LTH = TOTAL LENGTH OF MAT
C
      IF(LTH.GT.LENWRK) THEN
                        WRITE(NOUT1,2801)  LENWRK,LTH
                        WRITE(NOUT2,2801)  LENWRK,LTH
                        ENDIF
C
 2801 FORMAT(' *** WORK AREA SHORTAGE ERROR IN MACROT STEP , RESET '
     * ,'WORK AREA'/' ALLOCATED MEMORY =',I6,' USED MEMORY =',I6)
C
      FILENM(1)  = 'MACR'
      FILENM(2)  = 'OWRK'
      CALL WRITE(IDENT,AA,LTH)
*     WRITE(6,7031) IST3,IOPT(16),ISCT
*7031 FORMAT(1H ,' ## IST3 IOPT(16) ISCT ## ',4I6)
C
CTF   IF(IST3.LE.2 )                  GO TO 960
CTF   IF(IOPT(16).EQ.0.AND.ISCT.EQ.0) GO TO 960
C
      IDENT(8:8) = '3'
      CALL WRITE(IDENT,AA1,LTH)
C
      IF(ISCT.GT.1)  THEN
                     DO 954  L  = 1 , ISCT1
                     LN         = L+4
                     IDENT(8:8) = NUMB(LN) (4:4)
  954                CALL WRITE(IDENT,AAL(1,L),LTH)
C
                     IDENT(8:8) = 'X'
                     IF(MORDER.LT.ISCT) THEN
                                        WRITE (NOUT1,958) IDENT,MORDER
                                        ENDIF
                      ENDIF
C
  958 FORMAT(1H0,10X,'( ',A8,' : MEMBERS FOR PL-ORDERS ',
     &               'HIGHER THAN ',I2,' ARE ALL-ZERO. )'/)
C
C    DELAYED NEUTRON DATA WRITE
C
  960 CONTINUE
      IF (NDELAY.NE.0) THEN
                       IDENT(8:8) = 'Y'
CM                     CALL WRITE(IDENT,DD,LOCX)
                       CALL WRITE(IDENT,DD,NET*NFAMLY*3)
                       ENDIF
C
      FILENM(1) = 'MICR'
      FILENM(2) = 'EF  '
      IDENT     = MTNM(1,MAT) // 'BMIC'
      LENG       = MAXNG*MAXMT3*NISO(MAT)
      CALL OVRWRT( IDENT , EFFMIC , LENG )
 1000 CONTINUE
C
C-----END OF PROCESS
C
      RETURN
      END

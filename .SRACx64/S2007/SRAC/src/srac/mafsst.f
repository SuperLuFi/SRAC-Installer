C***********************************************************************
C                          MAFSST
C***********************************************************************
      SUBROUTINE MAFSST(MTNAME,NISO  ,ISW   ,IDENT ,IRES  ,
     1                  DN    ,VOLM  ,WTFLUX,SSFNU ,SFACT ,
     2                  IDENTH,DENHM ,SIG0HM,SSTHM ,SIGTHM,
     3                  NCODE ,SIGC  ,SIGF  ,SIGFNU,SIGT  ,
     4                  SIGA  ,SFCTR ,X1    ,X2    ,Y1    ,
     5                  Y2    ,WK1   ,WK2   ,WK3   ,LTH   ,
     6                  LA    ,LD    ,FTEMP ,FSIG0 ,SSC   ,
     7                  SSF   ,SSNU  ,SCHI  ,SSTR  ,SSE   ,
     8                  SST   ,SSIN  ,SS2N  ,UM    ,SMTX  ,
     9                  STR   ,FTAB  )
C
C     MAFCAL ----- EFFECTIVE CROSS SECTION CALCULATION ROUTINE
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
C
      COMMON  /MAFCNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     1                 IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
      COMMON  /MAFWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
C
      CHARACTER *4     NFILE,MTNAME,IDENT,NAMEP
      CHARACTER *8     IDMT
C
      DIMENSION   MTNAME(2,NMAT),NISO(NMAT),VOLM(NMAT),
     1            ISW(NMAT),IDENT(2,KNMAX,NMAT),IRES(KNMAX,NMAT),
     2            DN(KNMAX,NMAT)
C
      DIMENSION   WTFLUX(IGMAX,NMAT),SSFNU(IGMAX,KNMAX,NMAT)
      DIMENSION   SFACT (KNMAX,NMAT)
C
      DIMENSION   DENHM(NISOHM,2),SIG0HM(NEF,NISOHM)
      DIMENSION   SSTHM(NEF,2,NISOHM),SIGTHM(NEF,NMAT)
      DIMENSION   NCODE(NISOHM,NMAT)
      CHARACTER*4 IDENTH(2,NISOHM)
C
      DIMENSION   SIGC(NEF),SIGF(NEF),SIGFNU(NEF),SIGT(NEF),SIGA(NEF)
C
      DIMENSION  SFCTR(NEF,MXREAC),X1(MXTEMP),X2(MXSIG0),Y1(MXTEMP),
     1           Y2(MXSIG0),WK1(LNMAX),WK2(LNMAX),WK3(LNMAX),LTH(MXMTX),
     2           LD(MXMTX),LA(MXMTX),FTEMP(MXTEMP),FSIG0(MXSIG0)
C
      DIMENSION  SSC(NEF),SSF(NEF),SSNU(NEF),SCHI(NEF),SSTR(NEF),
     1           SSE(NEF),SST(NEF),SSIN(NEF),SS2N(NEF),UM(NEF),
     2           SMTX(IDS,NEF),STR(IDS,NEF),FTAB(MXSIG0,MXTEMP,NEF)
C
      DIMENSION   IA(17000)
      EQUIVALENCE (A(1),IA(1))
C
C     DEFINE NUMBER DENSITY OF HOMO. CELL
C
      SAVE     = 1.0E+10
      CALL    CLEA ( DENHM  , NISOHM*2     , 0.0 )
      CALL    CLEA ( SIG0HM , NEF*NISOHM   , SAVE)
      CALL    CLEA ( SSTHM  , 2*NEF*NISOHM , 0.0 )
      CALL    CLEA ( SIGTHM , NEF*NMAT     , 0.0 )
      CALL   ICLEA ( NCODE  , NISOHM*NMAT  , 0   )
C-----SET TEMPERATURE FOR EACH NUCLIDE
      DO 110   K = 1 , NISOHM
      DENHM(K,2) = A(K)
  110 CONTINUE
C-----SET TOTAL VOLUME
      TVOL     = 0.0
      DO 120 K = 1 , NMAT
      TVOL     = TVOL + VOLM(K)
  120 CONTINUE
C-----SET NCODE
      DO 150 K = 1 , NMAT
      MMK      = NISO(K)
      IF(MMK.LE.0)        GO TO 150
      DO 140 M = 1 , MMK
      IF(IRES(M,K).EQ.-1) GO TO 140
      DO 130 I = 1 , NISOHM
      IF(IDENT(1,M,K).NE.IDENTH(1,I).OR.IDENT(2,M,K).NE.IDENTH(2,I))
     +                    GO TO 130
      NCODE(I,K) = M
      GO TO 135
  130 CONTINUE
  135 CONTINUE
  140 CONTINUE
*     WRITE(6,151) K
*     WRITE(6,152) (NCODE(J,K),J=1,NISOHM)
  150 CONTINUE
C
* 151 FORMAT(1H ,' << CHECK WRITE AT SUB(MAFSST) : NCODE : REGION NO =',
*    +       I4 ,' >> ')
* 152 FORMAT(1H ,' ## NCODE ## ',20I5)
* 153 FORMAT(1H ,' ## DENHM ## ',1P10E11.4)
* 154 FORMAT(1H ,' ## TEMP  ## ',10F11.4)
C     SET HOMO DENSITY
      IF(TVOL.LE.0.0) GO TO 191
      DO 190 K = 1, NMAT
      RVOL     = VOLM(K)
      IF(RVOL.LE.0.0) GO TO 190
      SAVE     = RVOL/TVOL
      MMK      = NISO(K)
      IF(MMK.LE.0)    GO TO 190
      DO 180 M = 1 , NISOHM
      IPOS     = NCODE(M,K)
      IF(IPOS.LE.0) GO TO 180
      DENHM(M,1) = DENHM(M,1) + SAVE*DN(IPOS,K)
  180 CONTINUE
  190 CONTINUE
C
*     WRITE(6,153) (DENHM(M,1),M=1,NISOHM)
*     WRITE(6,154) (DENHM(M,2),M=1,NISOHM)
C
  191 CONTINUE
C
C     SET INITIAL TOTAL CROSS SECTION
C
      IFLSW    = 1
C
      DO 200 M = 1 , NISOHM
      NAMEP(1) = IDENTH(1,M)
      NAMEP(2) = IDENTH(2,M)
      DNTMP    = DENHM (M,1)
      IF(NAMEP(2)(1:1).EQ.'0') THEN
                               NFILE(1) = 'FAST'
                               NFILE(2) = 'U   '
                               NAMEP(2) (4:4) = '0'
                               ELSE
                               NFILE(1) = 'MICR'
                               NFILE(2) = 'EF  '
                               NAMEP(1) (1:1) = 'C'
                               NAMEP(2) (1:1) = 'F'
                               KANS     = 0
                               CALL  SEARCH(NAMEP(1),LENG,KANS)
                               IF(KANS.EQ.1)  THEN
                                              NAMEP(2)(4:4) = '0'
                                              JANS = 0
                                    CALL  SEARCH(NAMEP(1),LENG,JANS)
                                    IF(JANS.EQ.1) THEN
                                                  NFILE(1) = 'FAST'
                                                  NFILE(2) = 'U   '
                                                  NAMEP(2) = '0000'
                                                  ENDIF
                                              ENDIF
                               ENDIF
      CALL MAFCON(LTH,LA,LD,FTEMP,FSIG0)
      IST          = LOCAM(5) -1
*     WRITE(6,201) M,NAMEP,IST,DNTMP
      IF(LTOT.LE.0.OR.IST.LE.0) GO TO 200
      NAMEP(1) (1:1) = 'M'
      CALL CLEA( A      , 17000 , 0.0  )
      CALL READ(NAMEP(1),     A , LTOT )
C
C
      DO 195 I = 1 , NEF
      SSTHM(I,1,M) = A(IST+I)
      SSTHM(I,2,M) = A(IST+I)
      SIGTHM(I,1)  = SIGTHM(I,1) + SSTHM(I,1,M)*DNTMP
  195 CONTINUE
  200 CONTINUE
C
  201 FORMAT(1H ,' ## M NAMEP IST DNTMP (MAFSST)## ',I3,2X,2A4,I6,F10.6)
C
C     SET INITIAL SIGMA0
C
      IF(TVOL.GT.0.0) THEN
                      DO 205 M = 1 , NISOHM
                      DNTMP    = DENHM (M,1)
CM                    IF(DNTMP.LE.0.0)     GO TO 205
CM                    IF(DNTMP.LT.1.0E-50) GO TO 205
                      IF(DNTMP.LT.1.0E-30) GO TO 205
C
                      DNTMP    = 1.0/DNTMP
                      DO 204 I = 1 , NEF
                      SIG0HM(I,M) = SIGTHM(I,1)*DNTMP - SSTHM(I,1,M)
  204                 CONTINUE
*         WRITE(6,202)  M,IDENTH(1,M),IDENTH(2,M)
*         WRITE(6,203) (SIG0HM(I,M),I=1,NEF)
  205                 CONTINUE
                      ENDIF
C
* 202 FORMAT(1H ,' ## M IDENTH ## ',I3,2A4)
* 203 FORMAT(1H ,' ## SIG0HM ## ',1P10E11.4)
C
C     INITIAL SET
C
      LENG2    = NEF*4
      LENG3    = NEF*(10+2*IDS)
      LENG4    = NEF*KNMAX
      LENG5    = MXSIG0*MXTEMP*NEF
      LENG6    = NEF*MXREAC
      LENG7    = NEF*IDS
      LOOP     = 1
C
C     NUCLIDE LOOP
C
  206 CONTINUE
      CALL  CLEA(SIGC,LENG2,0.0)
C
      DO 600 MM = 1,NISOHM
      DNTMP     = DENHM(MM,1)
      NAMEP(1)  = IDENTH(1,MM)
      NAMEP(2)  = IDENTH(2,MM)
      RTEMP     = DENHM(MM,2)
      IF(NAMEP(2)(1:1).EQ.'0') THEN
                               NFILE(1) = 'FAST'
                               NFILE(2) = 'U   '
                               NAMEP(2) (4:4) = '0'
                               ELSE
                               NFILE(1) = 'MICR'
                               NFILE(2) = 'EF  '
                               NAMEP(1) (1:1) = 'C'
                               NAMEP(2) (1:1) = 'F'
                               KANS     = 0
                               CALL  SEARCH(NAMEP(1),LENG,KANS)
                               IF(KANS.EQ.1)  THEN
                                              NAMEP(2)(4:4) = '0'
                                              JANS = 0
                                    CALL  SEARCH(NAMEP(1),LENG,JANS)
                                    IF(JANS.EQ.1) THEN
                                                  NFILE(1) = 'FAST'
                                                  NFILE(2) = 'U   '
                                                  NAMEP(2) = '0000'
                                                  ENDIF
                                              ENDIF
                               ENDIF
C
      CALL  MAFCON(LTH   ,LA    ,LD    ,FTEMP ,FSIG0)
C
C     F-TABLE SEARCH START
C
      CALL  CLEA(SFCTR,LENG6,1.0)
      IF(IFS.EQ.0.OR.DNTMP.LE.0.0) GO TO 301
      IF(NGMIN.LE.0)     NGMIN=1
      IF(NGMAX.LE.0)     GO TO 301
      IF(NGMIN.GT.NGMAX) GO TO 301
C
      CALL  CLEA(A,17000,1.0)
      NAMEP(1)(1:1) = 'F'
      CALL  SEARCH(NAMEP(1),LENG,IANS)
      IF(IANS.EQ.1)  GO TO 301
      CALL  READ(NAMEP(1),A,LENG)
C
      DO 300 MT=1,MXREAC
      CALL  CLEA(FTAB,LENG5,1.0)
      GO TO (210,220,230,240,250,300,300) ,MT
  210 CONTINUE
      GO TO 300
  220 CONTINUE
      IF(ICAPT.EQ.0) GO TO 300
      IF(IFC.EQ.0)   GO TO 300
CKSK  IDMT=8HCAPT
      IDMT='CAPT    '
      GO TO 280
  230 CONTINUE
      IF(IFISS.EQ.0) GO TO 300
      IF(IFF.EQ.0)   GO TO 300
CKSK  IDMT=8HFISS
      IDMT='FISS    '
      GO TO 280
  240 CONTINUE
      IF(IFE.EQ.0)  GO TO 300
CKSK  IDMT=8HELAS
      IDMT='ELAS    '
      GO TO 280
  250 CONTINUE
      GO TO 300
  280 CONTINUE
      IST        = LOCAF(MT)
      IF(IST.LE.0)  GO TO 300
      DO 290 I   = NGMIN,NGMAX
      DO 290 K   = 1,NTEMP
      DO 290 J   = 1,NSIG
      FTAB(J,K,I)= A(IST)
      IST        = IST+1
  290 CONTINUE
C
      CALL SPLINE(FTAB  ,SFCTR(1,MT)  ,SIG0HM(1,MM) ,RTEMP ,FSIG0 ,
     +            FTEMP ,X1    ,X2    ,Y1    ,Y2    ,WK1   ,WK2   ,
     +            WK3   ,MXSIG0,MXTEMP,LNMAX ,NEF   ,NTEMP ,NSIG  ,
     +            NGMIN ,NGMAX )
  300 CONTINUE
C
  301 CONTINUE
      IF(LTOT.LE.0)  GO TO 600
      CALL  CLEA(A,17000,0.0)
      NAMEP(1) (1:1) = 'M'
      CALL  READ(NAMEP(1),A,LTOT)
      CALL  CLEA(SSC,LENG3,0.0)
C ----CAPTURE
      IF(ICAPT.EQ.0)  GO TO 311
      IST       = LOCAM(1)
      DO 1311 I = 1,NEF
      SSC(I)    = A(IST)
      IST       = IST+1
 1311 CONTINUE
C ----FISSION
  311 CONTINUE
      IF(IFISS.EQ.0)  GO TO 313
      IST       = LOCAM(2)
      IF(IST.LE.0)    THEN
                      IFISS = 0
                      GO TO 313
                      ENDIF
C
      DO 1301 I = 1,NEF
      SSF(I)    = A(IST)
      IST       = IST+1
 1301 CONTINUE
      IST       = LOCAM(4)
      DO 1302 I = 1,NEF
      SCHI(I)   = A(IST)
      IST       = IST+1
 1302 CONTINUE
      IST       = LOCAM(3)
      DO 1303 I = 1,NEF
      SSNU(I)   = A(IST)
      IST       = IST+1
      STR(I,1)  = SSNU(I)*SSF(I)
 1303 CONTINUE
C-----ELASTIC SCATTERING
  313 CONTINUE
      IST       = LOCAM(7)
      DO 1315 I = 1,NEF
      SSE(I)    = A(IST)
      IST       = IST+1
 1315 CONTINUE
C
C     MATRIX DATA READ
C
      DO 400 MT=1,2
      IF(LTH(MT).LE.0)            GO TO 400
      IDWN = LD(MT) + 1
      IEXT = LA(MT)
      IF(IDWN.LE.0.OR.IEXT.LE.0)  GO TO 400
      IST  = LOCAM(7+MT)
      IF(IST.LE.0)                GO TO 400
      CALL  CLEA(SMTX,LENG7,0.0)
      DO  360 I = 1,IEXT
      DO  360 J = 1,IDWN
      SMTX(J,I) = A(IST)
      IST       = IST+1
  360 CONTINUE
C
      DO 390 I= 1,IEXT
      SUM     = 0.0
      DO 370 J= 1,IDWN
      SUM     = SUM   + SMTX(J,I)
  370 CONTINUE
      IF(MT.EQ.1) SSIN(I) = SUM
      IF(MT.EQ.2) SS2N(I) = SUM * 0.500000
  390 CONTINUE
  400 CONTINUE
C
C     EFFECTIVE MICROSCOPIC CROSS SECTION CALCULATION
C
      DO 405 I = 1 , NEF
      SSTR(I)  =  SSC(I) + SSF(I) + SSE(I)  + SSIN(I) + SS2N(I)
  405 CONTINUE
C-----CAPTURE
      IF(ICAPT.GT.0) THEN
                     DO 410 I= 1,NEF
                     SSC(I)  = SSC(I)*SFCTR(I,2)
  410                CONTINUE
                     ENDIF
C-----FISSION
      IF(IFISS.GT.0) THEN
                     DO 420 I= 1,NEF
                     SSF(I)  = SSF(I)*SFCTR(I,3)
  420                CONTINUE
                     ENDIF
C-----ELASTIC SCATTERING
      DO 430 I = 1 , NEF
      FACT     = SFCTR(I,4)
      SAVE     = FACT*SSE(I)
      SSE(I)   = SAVE
      SST(I)   = SSF(I) + SSC(I) + SAVE + SSIN(I) + SS2N(I)
      SSTHM(I,1,MM) = SST(I)
      SSTHM(I,2,MM) = SSTR(I)
      STR(I,2) = SSF(I)*SSNU(I)
  430 CONTINUE
C-----CALCULATE MACROSCOPIC X-SECTION
      DO 500 I = 1 , NEF
      SIGT(I)  = SIGT(I)   + SST(I)*DNTMP
      SIGC(I)  = SIGC(I)   + SSC(I)*DNTMP
      SIGF(I)  = SIGF(I)   + SSF(I)*DNTMP
      SIGFNU(I)= SIGFNU(I) + SSF(I)*SSNU(I)*DNTMP
  500 CONTINUE
C-----STORE NU*SIGF CROSS SECTION IN SSFNU ARRAY
      IF(IFISS.GT.0) THEN
                     DO 550 K = 1, NMAT
                     JJ       = NCODE(MM,K)
                     IF(JJ.LE.0) GO TO 550
                     IPOS     = 1
                     IF(VOLM(K).GT.0.0) IPOS = 2
                                 DO 540 I = 1 , NEF
                                 SSFNU(I,JJ,K) = STR(I,IPOS)
  540                            CONTINUE
  550                            CONTINUE
                     ENDIF
  600 CONTINUE
C
      DO 610 I = 1 , NEF
      SIGA(I)  = SIGC(I) + SIGF(I)
  610 CONTINUE
      IF(TVOL.LE.0.0) GO TO 801
C
C     CHECK CONVERGENCE OF EFFECTIVE TOTAL X-SECTION
C
*     WRITE(6,651) LOOP
      ESP      = 0.001
      LAST     = 0
      DO 650 I = 1 , NEF
      DEL      = SIGT(I) - SIGTHM(I,1)
*     WRITE(6,652) I,SIGTHM(I,1),SIGT(I),DEL,DEL/SIGT(I)
      IF(ABS(DEL/SIGT(I)).GT.ESP) GO TO 660
  650 CONTINUE
      LAST     = 1
C
  651 FORMAT(1H ,' ## SIGT CONVERGENCE : LOOP NO IS ',I4,' ## ')
  652 FORMAT(1H ,' ## GROUP SIGTHM SIGT DEL DEL/SIGT ## ',I3,1P5E12.5)
C
C     SET NEXT SIGMA0
C
  660 CONTINUE
      DO 700    I = 1 , NEF
      SIGTHM(I,1) = SIGT(I)
  700 CONTINUE
      DO 800 M = 1 , NISOHM
      DNTMP    = DENHM (M,1)
CM    IF(DNTMP.LE.0.0)     GO TO 800
CMOD  IF(DNTMP.LT.1.0E-50) GO TO 800
      IF(DNTMP.LT.1.0E-30) GO TO 800
C
      DNTMP    = 1.0/DNTMP
      DO 750 I = 1 , NEF
      SIG0HM(I,M) = SIGTHM(I,1)*DNTMP - SSTHM(I,1,M)
  750 CONTINUE
*         IF(LAST.EQ.1) THEN
*         WRITE(6,202)  M,IDENTH(1,M),IDENTH(2,M)
*         WRITE(6,203) (SIG0HM(I,M),I=1,NEF)
*                       ENDIF
  800 CONTINUE
      LOOP = LOOP + 1
      IF(LAST.EQ.0) GO TO 206
C
C     SET EFECTIVE TOTAL X-SECTION
C
  801 CONTINUE
      CALL CLEA ( SIGTHM , NEF*NMAT , 0.0 )
      DO 900   K = 1 , NMAT
      MMK        = NISO(K)
      IF(MMK.LE.0) GO TO 900
      VOLR       = VOLM(K)
      IPOS       = 1
      IF(VOLR.LE.0.0) IPOS = 2
      DO 850   M = 1 , NISOHM
      MPOS       = NCODE(M,K)
      IF(MPOS.LE.0)          GO TO 850
      DNTMP      = DN(MPOS,K)
      IF(DNTMP.LE.0.0)       GO TO 850
      DO 820 I   = 1 , NEF
      SIGTHM(I,K)= SIGTHM(I,K) + DNTMP*SSTHM(I,IPOS,M)
  820 CONTINUE
      IF(ISW(K).EQ.3) THEN
      DO 830 I   = 1 , IGMAX
      SFACT(MPOS,K)= SFACT(MPOS,K) + WTFLUX(I,K)*SSFNU(I,MPOS,K)
  830 CONTINUE
*     IF(SFACT(MPOS,K).GT.0.0) THEN
*                WRITE(6,903)  MTNAME(1,K),MTNAME(2,K),IDENT(1,MPOS,K),
*    +                         IDENT(2,MPOS,K),MPOS,SFACT(MPOS,K)
*                WRITE(6,904) (WTFLUX(I,K),I=1,IGMAX)
*                WRITE(6,905) (SSFNU(I,MPOS,K),I=1,IGMAX)
*                              ENDIF
                      ENDIF
  850 CONTINUE
*     WRITE(6,901) K
*     WRITE(6,902) (SIGTHM(I,K),I=1,NEF)
  900 CONTINUE
C
* 901 FORMAT(1H ,' << CHECK WRITE AT SUB(MAFSST) : SIGT : REGION NO =',
*    +       I4 ,' >> ')
* 902 FORMAT(1H ,' ## SIGT ## ',1P10E11.4)
* 903 FORMAT(1H ,' ## MTNAME IDENT MPOS SFACT ## ',
*    +           2A4,2X,2A4,2X,I6,2X,1PE12.5)
* 904 FORMAT(1H ,' ## WTFLUX ## ',1P10E11.4)
* 905 FORMAT(1H ,' ## SSFNU  ## ',1P10E11.4)
C
C     MAFSST END
C
      RETURN
      END

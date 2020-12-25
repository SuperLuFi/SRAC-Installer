C***********************************************************************
C                          MAFFLX
C***********************************************************************
      SUBROUTINE MAFFLX(MTNAME,NISO  ,TEMP  ,ISW   ,IDENT ,
     1                  IRES  ,DN    ,NMFLUX,WTFLUX,SSFNU ,
     2                  SIGMA ,SFACT ,ISKIP )
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
C
      CHARACTER *4     NFILE,MTNAME,IDENT,NUMB
      CHARACTER *4     ID
C
      COMMON  /MAINC / JOPT(100),ID(2)
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
C
CKK9  COMMON  /MAFWRK/ IA(10),T(10),SIG0(8),XNU,XDUM,WORK(1000)
      COMMON  /MAFWRK/ IA( 8),T(12),SIG0(8),XNU,LENMEM,WORK(1000)
CEND
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
      COMMON  /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB
C
      CHARACTER *4     NAMEP(2)
      CHARACTER *8     FLUXNM
C
      DIMENSION   MTNAME(2,NMAT),NISO(NMAT),IRES(KNMAX,NMAT),TEMP(NMAT)
      DIMENSION   ISW(NMAT),IDENT(2,KNMAX,NMAT),DN(KNMAX,NMAT)
C
      CHARACTER*8 NMFLUX(NMAT)
      DIMENSION   SSFNU(IGMAX,KNMAX,NMAT)
      DIMENSION   WTFLUX(IGMAX,NMAT)
      DIMENSION   SIGMA(NET,NET5),SFACT(KNMAX,NMAT)
      DIMENSION   WTINF(107),EBOUND(50)
C
      EQUIVALENCE (IFISS,IA(1)),(KPN,IA(2)),(NTEMP,IA(7))
C
      DATA  BOLTSC / 8.616562E-5 /
      DATA  ECUT   / 5.000000    /
C
C     INITIAL SET
C
      CALL CLEA( WTFLUX  , IGMAX*NMAT       , 0.0 )
      CALL CLEA( WTINF   , 107              , 0.0 )
      CALL CLEA( SSFNU   , IGMAX*KNMAX*NMAT , 0.0 )
      CALL CLEA( SFACT   , KNMAX*NMAT       , 0.0 )
      CALL CLEA( EBOUND  , 50               , 0.0 )
C
C     MATERIAL CHECK  --- NEW NAME CHECK
C
      IFLSW    = 1
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
C
      DO 10 NN = 1,NMAT
      ISW(NN)  = 3
      NAMEP(1) = MTNAME(1,NN)
      NAMEP(2) = MTNAME(2,NN)
      NAMEP(2)(1:1) = 'F'
      NAMEP(2)(4:4) = '2'
      CALL  SEARCH(NAMEP(1),LENG,IANS)
      IF(IANS.EQ.0) THEN
                    WRITE(NOUT2,11) NAMEP
                    ISW(NN)=1
                    ENDIF
      NAMEP(2)(4:4) = '4'
      CALL  SEARCH(NAMEP(1),LENG,IANS)
      IF(IANS.EQ.0) ISW(NN)=ISW(NN)-1
   10 CONTINUE
C
   11 FORMAT(1H ,' ## MEMBER(',2A4,') ALREADY EXISTS IN MACRO LIB. ##')
C
      ISKIP   = 0
      DO 20 N = 1,NMAT
      IF(ISW(N).NE.0) GO TO 21
   20 CONTINUE
      ISKIP   = 1
C
      RETURN
C
C     DEFINE WEIGHTING FLUX FOR FISSION SPECTRUM CALCULATION
C
   21 CONTINUE
      IFLSW    = 1
      NFILE(1) = 'FAST'
      NFILE(2) = 'U   '
      NAMEP(1) = 'FAST'
      NAMEP(2) = 'LIB '
      LENG     = NEF+4
      CALL  READ( NAMEP , WORK , LENG )
      DO 30 I  = 1 , NEF
      WTINF(I) = WORK(4+I)
   30 CONTINUE
C
      NFILE(1) = 'FLUX'
      NFILE(2) = '    '
C-----LOOP OF MATERIAL
      DO 60  K = 1 , NMAT
      DO 45  I = 1 , NEF
      WTFLUX(I,K) = WTINF(I)
   45 CONTINUE
C
      IF(NMFLUX(K).EQ.'ASYMPTO.') THEN
                                  FLUXNM = MTNAME(1,K) // MTNAME(2,K)
                                  FLUXNM (5:5) = 'A'
                IF(IOPT(4).EQ.0)  FLUXNM (5:5) = 'F'
                                  FLUXNM (8:8) = '2'
                                  IANS   = 0
                                  CALL SEARCH( FLUXNM , LENG , IANS )
                                  IF(IANS.EQ.0) THEN
                                                NMFLUX(K) = FLUXNM
                                                ELSE
*                      WRITE(6,*) ' ** FLUX(',FLUXNM,') NOT FOUND *** '
                                                  FLUXNM (1:4) = ID(1)
                                                  FLUXNM (6:7) = '01'
                    IF(IBURN.GT.0.AND.NXRB.GT.1)  FLUXNM (6:7) = '0T'
                                                  JANS = 0
                                     CALL SEARCH( FLUXNM , LENG , JANS )
                                                  IF(JANS.EQ.0) THEN
                                IF(JOPT(79).GT.1) FLUXNM (6:6) =
     +                                            NUMB(JOPT(79)-1) (4:4)
                                                  NMFLUX(K) = FLUXNM
*                                                               ELSE
*                      WRITE(6,*) ' ** FLUX(',FLUXNM,') NOT FOUND *** '
                                                                ENDIF
                                                ENDIF
                                  ENDIF
C
      IF(NMFLUX(K).EQ.'ASYMPTO.') GO TO 60
C
      NMFLUX(K) (5:5) = 'A'
      IF(IOPT(4).EQ.0)  NMFLUX(K) (5:5) = 'F'
      NMFLUX(K) (8:8) = '2'
      LENG            = 0
      CALL SEARCH( NMFLUX(K) , LENG , IANS )
      IF(IANS.EQ.0) THEN
*                   WRITE(6,63) NMFLUX(K)
                    CALL READ ( NMFLUX(K) , WORK , LENG )
                    DO 50     I = 1 , IGMAX
                    WTFLUX(I,K) = WORK(I)
   50               CONTINUE
                    ELSE
                    NMFLUX(K)   =  'ASYMPTO.'
                    ENDIF
C
      NAMEP(1)  = NMFLUX(K) (1:4)
      NAMEP(2)  = NMFLUX(K) (5:8)
*     WRITE(6,61) K,LENG,(MTNAME(J,K),J=1,2),NAMEP
*     IF(IANS.EQ.0) WRITE(6,62) (WORK(I),I=1,LENG)
*     IF(IANS.NE.0) WRITE(6,62) (WTFLUX(I,K),I=1,NEF)
C
   60 CONTINUE
C
   61 FORMAT(1H ,' ## L LENG MTNAME NAMEP ## ',2I6,2X,2A4,2X,2A4)
   62 FORMAT(1H ,' ## FLUX ## ',1P10E11.4)
   63 FORMAT(1H ,10X,' MEMBER ',A8,' WAS READ FROM FLUX-FILE ',
     *               'FOR FISSION SPECTRUM MIXING. (MAFFLX) ')
C
      IF(IOPT(4).EQ.0) GO TO 301
C
C     READ THERMAL FISSION X-SECTION
C
      DO 200 K = 1 , NMAT
      IF(ISW(K).NE.3)     GO TO 200
      MMK      = NISO(K)
      IF(MMK.LE.0)        GO TO 200
      RTEMP    = TEMP(K)
      IF(NMFLUX(K).NE.'ASYMPTO.') GO TO 125
C
               NFILE(1) = 'THER'
               NFILE(2) = 'MALU'
               NAMEP(1) = 'THER'
               NAMEP(2) = 'MAL1'
               LENG     = 2*NET+2
               CALL  READ( NAMEP , WORK , LENG )
               NET1     = NET + 1
               DO 110 I = 1 , NET1
               EBOUND(I)= WORK(NET1+I)
  110          CONTINUE
C
               RTEMPF  = RTEMP + 50.0
               FACT    = RTEMPF*BOLTSC
               ACOEF   = ECUT*ECUT*EXP(-ECUT)*FACT
               BCOEF   = 1.0 / ACOEF
               EBOLTS  = ECUT*FACT
C
*     WRITE(6,*)  ' ** RTEMPF FACT EBOLTS ** ',RTEMPF,FACT,EBOLTS
*     WRITE(6,*)  ' ** ACOEF  BCOEF       ** ',ACOEF,BCOEF
C
               DO 120 I = 1 , NET
               X1       = EBOUND(I) /FACT
               X2       = EBOUND(I+1)/FACT
               IF(X2.GE.ECUT) THEN
                              FSAVE = ALOG(EBOUND(I)/EBOUND(I+1))
C
                              ELSE
                              FSAVE = 0.0
                              IF(X1.GT.ECUT) THEN
                                       FSAVE = ALOG(EBOUND(I)/EBOLTS)
                                       X1    = EBOLTS/FACT
                                             ENDIF
                              Y1    =-BCOEF*EXP(-X1)*(X1+1.0)*FACT
                              Y2    =-BCOEF*EXP(-X2)*(X2+1.0)*FACT
                              FSAVE = FSAVE + Y1 - Y2
                              ENDIF
C
*     WRITE(6,*) ' ** I EHI ELOW WT ** ',I,EBOUND(I),EBOUND(I+1),FSAVE
               WTFLUX(NEF+I,K) = FSAVE
  120          CONTINUE
C
*              WRITE(6,61) K,LENG,(MTNAME(J,K),J=1,2),NAMEP
*              WRITE(6,62) (WTFLUX(I,K),I=1,IGMAX)
C
  125 CONTINUE
      DO 190 M = 1 , MMK
      IIRES    = IRES(M,K)
      IF(IIRES.EQ.-1)     GO TO 190
      IF(DN(M,K).LE.0.0 ) GO TO 190
      NAMEP(1) = IDENT(1,M,K)
      NAMEP(2) = IDENT(2,M,K)
      NAMEP(1) (1:1) = 'C'
      IANS     = 0
C
      IF(IIRES.EQ.1) THEN
                     IF(NAMEP(2)(4:4).EQ.'0') THEN
                     TDIFF1  = 10000.
                     NT      = 1
                     DO 7140 J= 1,NTDUMY
                     TDIFF2  = ABS(RTEMP-STND(J))
                     IF(TDIFF2.GT.TDIFF1) GO TO 7141
                     NT      = J
7140                 TDIFF1  = TDIFF2
7141                 CONTINUE
                     NAMEP(2) (4:4)     = NUMB(NT) (4:4)
                     IDENT(2,M,K) (4:4) = NUMB(NT) (4:4)
                     ENDIF
C
                     NFILE(1) = 'MICR'
                     NFILE(2) = 'EF  '
                     NAMEP(2) (1:1) = 'T'
CM                   CALL SEARCH( NAMEP , LENG , IANS)
CM                   IF(IANS.EQ.0) THEN
CM                                 CALL READ( NAMEP , IA , 30 )
CM                                 ENDIF
                     JANS     = 0
                     CALL SEARCH( NAMEP , LENG , JANS)
                     IF(JANS.EQ.0) THEN
                                   CALL READ( NAMEP , IA , 30 )
C
                                   ELSE
CM                                 NAMEP(2) (1:3) = '000'
                                   NAMEP(2) (1:3) = 'T00'
                                   CALL SEARCH( NAMEP , LENG , IANS)
                                   IF(IANS.EQ.0) THEN
                                        CALL READ( NAMEP , IA , 30 )
                                        ELSE
                                        IIRES = 0
                                        ENDIF
                                    ENDIF
                     ENDIF
C
      IF(IIRES.NE.1.OR.IANS.NE.0) THEN
                     NFILE(1) = 'THER'
                     NFILE(2) = 'MALU'
                     NAMEP(2) (4:4) = '0'
                     CALL READ( NAMEP , IA , 30 )
                     ENDIF
C
      IF(IFISS.EQ.0) GO TO 190
      NAMEP(1) (1:1) = 'K'
CKK9*********** DELETE **********************************************
C     IF(IIRES.NE.1)  THEN
C                     TDIFF1  = 10000.
C                     NT      = 1
C                     DO 140 J= 1,NTEMP
C                     TDIFF2  = ABS(RTEMP-T(J))
C                     IF(TDIFF2.GT.TDIFF1) GO TO 141
C                     NT      = J
C140                  TDIFF1  = TDIFF2
C141                  CONTINUE
C                     NAMEP(2) (4:4) = NUMB(NT) (4:4)
C                     ENDIF
CEND*********** DELETE **********************************************
C
      CALL SEARCH( NAMEP , LENG , IANS )
CKK9
      IF(NTEMP.GT.1) THEN
                     LENG = LENG/NTEMP
*                    WRITE(6,*) ' ** NAMEP LENG NTEMP LENMEM : ',
*    +                               NAMEP,LENG,NTEMP,LENMEM
                     ENDIF
CEND
      IST   = 1
      JLENG = NET*NET
      IF(LENG.LT.JLENG)  IST = NET + 1
      CALL READ( NAMEP , SIGMA(1,IST) , LENG )
C
      DO  150 I = 1 , NET
      SSFNU(I+NEF,M,K) = SIGMA(I,NET+4)*SIGMA(I,NET+5)
  150 CONTINUE
C
*     WRITE(6,201)  K,M,IFISS,KPN,NTEMP,LENG,NAMEP,NFILE(1),NFILE(2)
*     WRITE(6,202)  (SIGMA(I,NET+4),I=1,NET)
*     WRITE(6,203)  (SIGMA(I,NET+5),I=1,NET)
C
  190 CONTINUE
  200 CONTINUE
C
  201 FORMAT(1H ,' ## K M IFISS KPN NTEMP LENG IDENT NFILE ## ',
     +           6I6,2X,2A4,2X,2A4)
  202 FORMAT(1H ,' ## SSF ## ',1P10E11.4)
  203 FORMAT(1H ,' ## NU  ## ',10F11.4)
C
C     MASK 5-TH CHARACTER OF IDENT FOR FAST-ENERGY REGION
C
  301 CONTINUE
      DO 400 K = 1,NMAT
      MMK      = NISO(K)
      IF(MMK.LE.0) GO TO 400
      DO 350 M = 1,MMK
      IF(IRES(M,K).NE.1) IDENT(2,M,K) (1:1) = '0'
  350 CONTINUE
  400 CONTINUE
C
C     MAFFLX END
C
      RETURN
      END

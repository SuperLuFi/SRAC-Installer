C***********************************************************************
C                          MAFDAT
C***********************************************************************
      SUBROUTINE  MAFDAT(MTNAME,NISO  ,TEMP  ,XL    ,DC    ,
     +                   LISO  ,IDENT ,IRES  ,DN    ,LXMICR,
     +                   DANCOF,MATD  ,NMFLUX,NAMRES)
C
      DOUBLE PRECISION JNEFST,FNEFST,JNMACR,FNMACR
      CHARACTER*4      NAMRES,IDENT,CA,MTNAME,NMFLUX
C
CDEL  PARAMETER   (MXLISO= 2000)
      INCLUDE  'MATDTINC'
C
      COMMON  /MAINC / NN(100),ID(2),TITLE(18),AA(380)
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON  /TMPSET/ STND(35),IDTEMP(61),NTDUMY
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
      COMMON  /PIJ2C / IPAA(1000)
      COMMON  /NEWDAN/ DANNEW(MXLISO)
C
      CHARACTER *4     NFILE,NMTEMP(2)
C
      DIMENSION        MTNAME(2,NMAT),NISO(NMAT),TEMP(NMAT)
      DIMENSION        XL(NMAT),DC(NMAT),LISO(NMAT),NMFLUX(2,NMAT)
      DIMENSION        IDENT(2,KNMAX,NMAT),DN(KNMAX,NMAT)
      DIMENSION        IRES(KNMAX,NMAT),LXMICR(KNMAX,NMAT)
      DIMENSION        DANCOF(KNMAX,NMAT)
      DIMENSION        MATD(NMP)
      DIMENSION        NAMRES(1)
C
      DIMENSION        IA(380),CA(380)
      EQUIVALENCE     (IA(1),AA(1)),(CA(1),AA(1))
C
C-----START OF PROCESS
C
      NMTEMP(1) = 'TEMP'
      NMTEMP(2) = 'SET '
C
      IFLSW    = 1
      NFILE(1) = 'FAST'
      NFILE(2) = 'U '
C
      CALL SEARCH(NMTEMP(1),LENG,IANS)
      IF(IANS.EQ.0)  CALL READ(NMTEMP(1),STND(1),41)
C
      IF(KNMAX.LE.0)  RETURN
C
C-----RETRIEVE MATERIAL DATA FROM COMMON/MAINC/
C
      LCMTNM = NN(85)
      DO 11 J= 1,NMAT
      DO 11 I= 1,2
      MTNAME(I,J)= CA(LCMTNM)
   11 LCMTNM = LCMTNM+1
C
      LCNISO = NN(86)
      DO 12 J= 1,NMAT
      NISO(J)= IA(LCNISO)
   12 LCNISO = LCNISO+1
C
      LCTEMP = NN(87)
      DO 13 J= 1,NMAT
      TEMP(J)= AA(LCTEMP)
   13 LCTEMP = LCTEMP+1
C
      LCXL   = NN(88)
      DO 14 J= 1,NMAT
      XL(J)  = AA(LCXL)
   14 LCXL   = LCXL+1
C
      LCXCDC = NN(89)
      DO 15 J= 1,NMAT
      DC(J)  = AA(LCXCDC)
   15 LCXCDC = LCXCDC+1
C
      LCLISO = NN(90)
      DO 16 J= 1,NMAT
      LISO(J)= IA(LCLISO)
      LCLISO = LCLISO+1
   16 CONTINUE
C
      LCXIWT = NN(45)
      DO 77 J= 1,NMAT
      NMFLUX(1,J) = CA(LCXIWT)
      NMFLUX(2,J) = CA(LCXIWT+1)
      LCXIWT      = LCXIWT + 2
   77 CONTINUE
C
      LCIRES   = NN(93)
      DO 119 J = 1,NMAT
      ISW      = LCIRES+LISO(J)-1
      MM       = NISO(J)
      IF(MM.LE.0)   GO TO 119
      DO 19 M  = 1,MM
      IRES(M,J)= IA(ISW)
      ISW      = ISW+1
   19 CONTINUE
  119 CONTINUE
C
      LCIDNT   = NN(91)
      DO 117 J = 1,NMAT
      RTEMP    = TEMP(J)
      TDIFF    = 10000.
      ISET     = NTDUMY
      DO 217 K = 1,NTDUMY
      IF (ABS(STND(K)-RTEMP).GT.TDIFF) GO TO 218
      ISET      = K
  217 TDIFF     = ABS(STND(K)-RTEMP)
  218 IDTMP     = IDTEMP(ISET)
      ISW       = LCIDNT+(LISO(J)-1)*2
      MM        = NISO(J)
      IF(MM.LE.0)   GO TO 117
      DO 317   M  = 1,MM
      DO  17    I = 1,2
      IDENT(I,M,J)= CA(ISW)
      ISW         = ISW+1
   17 CONTINUE
      IF(IRES(M,J).NE.1) THEN
      CALL PACK(IDENT(2,M,J),4,IDTMP)
      CALL PACK(CA(ISW-1)   ,4,IDTMP)
                         ENDIF
  317 CONTINUE
  117 CONTINUE
C
      LCDN    = NN(92)
      DO 118 J= 1,NMAT
      ISW     = LCDN+LISO(J)-1
      MM      = NISO(J)
      IF(MM.LE.0)   GO TO 118
      DO 18 M = 1,MM
      DN(M,J) = AA(ISW)
      ISW     = ISW+1
   18 CONTINUE
  118 CONTINUE
C
      LCIXMC  = NN(94)
      DO 120 J= 1,NMAT
      ISW     = LCIXMC+LISO(J)-1
      MM      = NISO(J)
      IF(MM.LE.0)   GO TO 120
      DO 20 M = 1,MM
      LXMICR(M,J) = IA(ISW)
      ISW         = ISW+1
   20 CONTINUE
  120 CONTINUE
C
      CALL CLEA( DANCOF , KNMAX*NMAT , 0.0 )
C
      DO 121 N = 1 , NMAT
      ISW      = LISO(N) -1
      MM       = NISO(N)
      IF(MM.LE.0) GO TO 121
      DO 21  M = 1 , MM
      ISW      = ISW + 1
      DANCOF(M,N) = DANNEW(ISW)
   21 CONTINUE
  121 CONTINUE
C
C ---- MASK 1-ST,5-TH,6-TH,7-TH CHARACTER OF NUCLIDE IDENT
C
      DO 140 N = 1,NMAT
      MMK      = NISO(N)
      IF(MMK.LE.0)  GO TO 140
      DO  40 MM= 1,MMK
      IDENT(1,MM,N) (1:1) = ' '
                 IF(IRES(MM,N).NE.1) THEN
                                     IDENT(2,MM,N) (2:3) = '00'
                                     ELSE
                                     IDENT(2,MM,N) (1:1) = 'F'
                                     ENDIF
   40 CONTINUE
  140 CONTINUE
C
C-----SET NORES & NAMRES
C
      NORES  = 0
      IPASS  = 0
C
      IF(IOPT(1).EQ.1) THEN
      LCMATD    = IPAA(50)+50
      DO 1250 K = 1 , NMP
      MATD(K)   = IPAA(LCMATD+K-1)
 1250 CONTINUE
                       ENDIF
C
      IF(IOPT(3).LT.1)  GO TO 999
      IF(IOPT(3).GT.2)  GO TO 999
C
CM    LCMATD    = IPAA(50)+50
CM    DO 1250 K = 1 , NMP
CM    MATD(K)   = IPAA(LCMATD+K-1)
C1250 CONTINUE
C
      DO 1350 K = 1 , NMP
      LL        = MATD(K)
      IF(XL(LL).EQ.0.0)  GO TO 1350
      MMK       = NISO(LL)
      IF(MMK.LE.0)       GO TO 1350
      IF(IPASS.EQ.0) THEN
                     DO 1310     M = 1,MMK
                     IF(IRES(M,LL).NE.2)  GO TO 1310
                     NORES         = NORES + 1
                     NAMRES(NORES) = IDENT(1,M,LL)
 1310                CONTINUE
C
                 ELSE
                 DO 1330 M = 1,MMK
                 IF(IRES(M,LL).NE.2) GO TO 1330
                 IEXT      = 0
                     DO 1320 J = 1 , NORES
                     IF(NAMRES(J).EQ.IDENT(1,M,LL))  IEXT = 1
 1320                CONTINUE
                 IF(IEXT.EQ.1) GO TO 1330
C
                     NORES         = NORES + 1
                     NAMRES(NORES) = IDENT(1,M,LL)
 1330                CONTINUE
                 ENDIF
C
      IPASS = 1
 1350 CONTINUE
C
C     COMPOSITION DATA STORAGE END
C
  999 CONTINUE
      WRITE(NOUT2,208)  ID,TITLE
      IF(NN(100).EQ.0)  THEN
      WRITE(NOUT2,211)  NEF,IGT,NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,MXREAC,
     +                  IDS,NEF1,IGMAX,NET,NET5
                        ENDIF
C
      WRITE(NOUT2,212)
      DO 200 K = 1,NMAT
      WRITE(NOUT2,201)  K,(MTNAME(I,K),I=1,2)
      MMK      = NISO(K)
      WRITE(NOUT2,202)  MMK
      IF(MMK.LE.0)      GO TO 200
      WRITE(NOUT2,209)  TEMP(K),XL(K),DC(K),(NMFLUX(J,K),J=1,2)
      WRITE(NOUT2,203) ((IDENT(J,M,K),J=1,2),M=1,MMK)
      WRITE(NOUT2,204) (DN(M,K),M=1,MMK)
      WRITE(NOUT2,205) (IRES(M,K),M=1,MMK)
      WRITE(NOUT2,206) (LXMICR(M,K),M=1,MMK)
CJAIS ADDED FOR NUCLIDE-WISE DANCOFF FACTOR ****4/9/1985***
      MODEDC = 0
      IF(IOPT(3).EQ.1) THEN
                       DO 190 J = 1 , NMP
                       KK       = MATD(J)
                       IF(KK.NE.K) GO TO 190
                                   DO 185 M = 1 , MMK
                                   IF(IRES(M,K).EQ.2) MODEDC = 1
  185                              CONTINUE
  190                  CONTINUE
                       ENDIF
C
      IF(MODEDC.EQ.1)  THEN
                       WRITE(NOUT2,215)
                       ELSE
                       IF(DC(K).LT.0.0) THEN
                             WRITE(NOUT2,216) (DANCOF(M,K),M=1,MMK)
                             ENDIF
                       ENDIF
CJAIS END
  200 CONTINUE
C
CJAIS ADDED FOR NUCLIDE-WISE DANCOFF FACTOR ****4/9/1985***
      IF(IOPT(3).EQ.1) THEN
                       WRITE(NOUT2,518) NORES
                       WRITE(NOUT2,519) (NAMRES(M),M=1,NORES)
                       ENDIF
CJAIS END
C     WRITE(NOUT2,207)
      WRITE(NOUT2,515)
      WRITE(NOUT2,516) (IDTEMP(I),I=1,NTDUMY)
      WRITE(NOUT2,517) (STND(I)  ,I=1,NTDUMY)
C
  201 FORMAT(/1H ,30X,'## MATERIAL NAME ----- ',I2,3X,2A4,' ##'/)
  202 FORMAT(1H ,10X,'NUMBER OF NUCLIDE ----------- ',I8)
  203 FORMAT(1H ,10X,'IDENTIFICATION OF NUCLIDE --- ',5(2X,2A4,2X)
     +    /,(1H ,40X,5(2X,2A4,2X)))
  204 FORMAT(1H ,10X,'NUMBER DENSITY -------------- ',1P5E12.5
     +    /,(1H ,40X,1P5E12.5))
  205 FORMAT(1H ,10X,'RESONANT INDICATOR ---------- ',5(I6,6X)
     +    /,(1H ,40X,5(I6,6X)))
  206 FORMAT(1H ,10X,'LXMICR ---------------------- ',5(I6,6X)
     +    /,(1H ,40X,5(I6,6X)))
  207 FORMAT(1H1)
  208 FORMAT(1H1//1H ,20X,'MACROF'//1H ,10X,'CASE I.D. : ',
     +2A4/1H ,10X,'TITLE     : ',18A4)
  209 FORMAT(1H
     +          ,10X,'TEMPERATURE (K) ------------- ',F10.2,
     +      /1H ,10X,'MEAN CHORD LENGTH ----------- ',F10.5,
     +      /1H ,10X,'DANCOFF FACTOR -------------- ',F10.5,
     +      /1H ,10X,'WEIGHTING FLUX NAME FOR CHI - ',2A4)
  211 FORMAT(1H ,10X,'NUMBER OF ENERGY GROUPS ------------ ',I6,
     1      /1H ,10X,'GEOMETRY TYPE ---------------------- ',I6,
     2      /1H ,10X,'NUMBER OF MATERIAL ----------------- ',I6,
     3      /1H ,10X,'KNMAX,MXMTX,MXTEMP,MXSIG0,MXREAC --- ',5I6,
     4      /1H ,10X,'IDS,NEF1,IGMAX,NET,NET5 ------------ ',5I6)
  212 FORMAT(///1H ,20X,'COMPOSITION DATA LIST'/)
  215 FORMAT(1H ,10X,'NUCLIDE-WISE DANCOFF FACTOR WILL BE CALCULATED ',
     *               'AT SUBROUTINE(MAFSIG) ]] ')
  216 FORMAT(1H ,10X,'NUCLIDE-WISE DANCOFF FACTOR-- ',5F12.6,
     +    /,(1H ,40X,5F12.6)  )
  518 FORMAT(/1H ,30X,'## RESONANT NUCLIDE NAME LIST ##',
     +      //1H ,10X,'NUMBER OF RESONANT NUCLIDE -- ',I6)
  519 FORMAT( 1H ,10X,'RESONANT NUCLIDE NAME ------- ',10(A4,2X)
     +     /,(1H ,40X,10(A4,2X)) )
C
  515 FORMAT(/1H ,30X,'## STANDARD TEMPERATUE ARRAY LIST (STND) ##'/)
  516 FORMAT(1H ,' TAG         : ',10(A4,5X)
     +      /1H ,' TAG         : ',10(A4,5X))
  517 FORMAT(1H ,' TEMPERATURE : ',10F9.2)
C
      RETURN
      END

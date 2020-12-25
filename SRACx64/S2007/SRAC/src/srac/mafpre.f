C***********************************************************************
C                          MAFPRE
C***********************************************************************
      SUBROUTINE  MAFPRE(NISO  ,TEMP  ,IDENT ,IRES  ,ISWF  ,MATD  ,
     +                   VOLM  ,IDENTH )
C
      DOUBLE PRECISION JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON  /MAINC / NN(100),ID(2),TITLE(18),AA(380)
      COMMON  /PIJ2C / IPAA(1000)
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON /MAFWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
C
      DIMENSION        NISO(NMAT),TEMP(NMAT),IRES(KNMAX,NMAT)
      DIMENSION        MATD(NMP) ,VOLM(NMAT),ISWF(NMAT)
C
      CHARACTER*8      IDENT(KNMAX,NMAT)
      CHARACTER*8      IDENTH(1)
C
      DIMENSION        PAA(1000)
      EQUIVALENCE     (IPAA(1),PAA(1))
C
C-----START OF PROCESS
C
      CALL  CLEA( VOLM , NMAT , 0.0 )
      CALL  CLEA(    A ,17000 , 0.0 )
      CALL ICLEA( ISWF , NMAT ,   0 )
C
C-----SET NISOHM & IDENTH
C
      IF(IOPT(3).NE.1.AND.IOPT(3).NE.2) GO TO 200
      LCVOLM   = IPAA(49)+50
      DO 100 K = 1 , NMP
      VOLM(MATD(K)) = PAA(LCVOLM+K-1)
  100 CONTINUE
C
  200 CONTINUE
      NISOHM    = 0
      IPASS     = 0
C
      DO 500 K  = 1 , NMAT
      MMK       = NISO(K)
      TEMPR     = TEMP(K)
      ISW       = 0
      IF(MMK.LE.0)   GO TO 500
C
      IF(IPASS.EQ.0) THEN
                     ICNT        = 0
                     DO 300 M    = 1,MMK
                     IIRES = IRES(M,K)
                     IF(IIRES.EQ.-1) GO TO 300
                     IF(IIRES.EQ. 2) ISW = 1
                     ICNT        = ICNT + 1
                     IDENTH(ICNT)= IDENT(M,K)
                     A     (ICNT)= TEMPR
  300                CONTINUE
                     NISOHM      = ICNT
C
                     ELSE
                     DO 400 M = 1,MMK
                     IIRES = IRES(M,K)
                     IF(IIRES.EQ.-1)             GO TO 400
                     IF(IIRES.EQ. 2) ISW = 1
                     DO 350 J = 1 , NISOHM
                     IF(IDENTH(J).EQ.IDENT(M,K)) GO TO 400
  350                CONTINUE
                     NISOHM   = NISOHM + 1
                     IDENTH(NISOHM) = IDENT(M,K)
                     A     (NISOHM) = TEMPR
  400                CONTINUE
                     ENDIF
C
      IPASS   = 1
      ISWF(K) = ISW
  500 CONTINUE
C
C     CHECK WRITE
C
CM    WRITE(6,501) IOPT(3),NMP,NMAT,NISOHM
CM    WRITE(6,502) (VOLM(I),I=1,NMAT)
CM    IF(NMP.GT.0)  WRITE(6,504)  (MATD(I),I=1,NMP)
CM    IF(NMP.GT.0)  WRITE(6,505)  (ISWF(I),I=1,NMAT)
CM    WRITE(6,503) (IDENTH(I),I=1,NISOHM)
CM    WRITE(6,506) (A     (I),I=1,NISOHM)
C
  501 FORMAT(/1H ,30X,'## CHECK WRITE AT SUB(MAFPRE)  ##',
     +      //1H ,10X,' IOPT(3) ------------------- ',I6,
     +       /1H ,10X,' NMP     ------------------- ',I6,
     +       /1H ,10X,' NMAT    ------------------- ',I6,
     +       /1H ,10X,' NISOHM  ------------------- ',I6/)
  502 FORMAT(1H ,' ## VOLM   ## ',1P10E11.4)
  503 FORMAT(1H ,' ## IDENTH ## ',10A10)
  504 FORMAT(1H ,' ## MATD   ## ',10I10)
  505 FORMAT(1H ,' ## ISWF   ## ',10I10)
  506 FORMAT(1H ,' ## TEMP   ## ',10F10.3)
C
      RETURN
      END

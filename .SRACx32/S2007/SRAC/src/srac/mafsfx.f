      SUBROUTINE  MAFSFX(ADDRES,LONG)
C
      DOUBLE PRECISION JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
C
      INTEGER*4   ADDRES(LONG)
C
C       *********************************************
C       *    ADDRESS SETTING FOR VAIRABLE DIMENION  *
C       *********************************************
C
C
C           ADDRESS   VARIABLE           SIZE
C        ----------   ------------    ----------------------------
C               1      MTNAME           2*NMAT
C               2      NSIO               NMAT
C               3      TEMP               NMAT
C               4      XL                 NMAT
C               5      DC                 NMAT
C               6      LISO OR ISW        NMAT
C               7      IDENT            2*NMAT*KNMAX
C               8      IRES               NMAT*KNMAX
C               9      DN                 NMAT*KNMAX
C              10      LXMICR             NMAT*KNMAX
C              11      DANCOF             NMAT*KNMAX
C              12      ISWF               NMAT
C              13      MATD               NMP
C              14      VOLM               NMAT
C              15      NMLFUX           2*NMAT
C              16      WTLFUX             NMAT*IGMAX    (IGMAX=NEF+NET)
C              17      SSFNU              NMAT*IGMAX*KNMAX
C              18      SIGMA              NET*(NET+5)
C              19      SFACT              NMAT*KNMAX
C              20      NAMRES             NORES
C              21      IND                NORES*NMAT
C              22      MCODE              NORES*NMAT
C              23      DENRES             NORES*NMAT
C              24      KCODE              NORES*NMAT
C              25      IDENTH           2*NISOHM
C              26      DENHM            2*NISOHM
C              27      SIG0HM             NISOHM*NEF
C              28      SSTHM            2*NISOHM*NEF
C              29      SIGTHM             NEF*NMAT
C              30      NCODE              NISOHM*NMAT
C              31      ENBND              NEF1
C              32      LSS                NEF
C              33      LGV                NEF
C              34      SIGC               NEF
C              35      SIGF               NEF
C              36      SIGFNU             NEF
C              37      SIGT               NEF
C              38      CHI                NEF
C              39      D1                 NEF
C              40      D2                 NEF
C              41      SIGA               NEF
C              42      SIGN2T             NEF
C              43      SIGS               NEF*IDS
C              44      SIGN2N             NEF*IDS
C              45      SIG0               NEF*KNMAX
C              46      SIGWF              NEF
C              47      SIGWM              NEF
C              48      SIGWT              NEF*KNMAX
C              49      FMXT             3*NEF*KNMAX
C              50      TSIG   OR SIGT     NMAT
C              51      SIGTIJ OR SIGT0J   NMP*NEF
C              52      GAMMA              NMP*NMP
C              53      PIJ                NMP*NMP*NEF
C              54      DENWRK             NISOHM
C              55      SSTWRK             NISOHM
C              56      SFCTR              NEF*MXREAC
C              57      X1                 MXTEMP
C              58      X2                 MXSIG0
C              59      Y1                 MXTEMP
C              60      Y2                 MXSIG0
C              61      WK1                LNMAX
C              62      WK2                LNMAX
C              63      WK3                LNMAX
C              64      LTH                MXMTX
C              65      LA                 MXMTX
C              66      LD                 MXMTX
C              67      FTEMP              MXTEMP
C              68      FSIG0              MXSIG0
C              69      SSC                NEF
C              70      SSF                NEF
C              71      SSNU               NEF
C              72      SCHI               NEF
C              73      SSTR               NEF
C              74      SSE                NEF
C              75      SST                NEF
C              76      SSIN               NEF
C              77      SS2N               NEF
C              78      UM                 NEF
C              79      SMTX               NEF*IDS
C              80      STR                NEF*IDS
C              81      FTAB               MXSIG0*MXTEMP*NEF
C              82      CHIMTX             NEF*IGMAX
C
      LOC           = 25
C-----SET 26 TO 35
      ADDRES(LOC+1) = ADDRES(LOC) + 2*NISOHM
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + 2*NISOHM
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NISOHM*NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + 2*NISOHM*NEF
      LOC           = LOC + 1
CM    ADDRES(LOC+1) = ADDRES(LOC) + NISOHM*NEF
      ADDRES(LOC+1) = ADDRES(LOC) + NMAT*NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NISOHM*NMAT
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF1
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
C-----SET 36 TO 45
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*IDS
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*IDS
C-----SET 46 TO 55
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*KNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*KNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + 3*NEF*KNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NMAT
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*NMP
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NMP*NMP
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NMP*NMP*NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NISOHM
C-----SET 56 TO 65
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NISOHM
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*MXREAC
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXTEMP
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXSIG0
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXTEMP
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXSIG0
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + LNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + LNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + LNMAX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXMTX
C-----SET 65 TO 75
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXMTX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXMTX
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXTEMP
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXSIG0
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
C-----SET 76 TO 92
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*IDS
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*IDS
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + MXSIG0*MXTEMP*NEF
      LOC           = LOC + 1
      ADDRES(LOC+1) = ADDRES(LOC) + NEF*IGMAX - 1
C
      IF(IOPT(19).GT.0) WRITE(NOUT2,1)  (ADDRES(I),I=1,LONG)
C
    1 FORMAT(///1H ,5X,'* ADDRESS OF ARGUMENT.(MACROF) *',
     &//1H ,10X,')SUFX>'//(1H ,5X,20I6:/))
C
      RETURN
      END

C
C     MAIN ROUTINE FOR PEACO
C
      SUBROUTINE PEACO
C
CMOD  PARAMETER   (MAXMAT=100)
      INCLUDE  'MATDTINC'
C
      CHARACTER*4     TIL,ID
C
      COMMON /MAINC / IMAA (120),AA(380)
      COMMON /PIJ2C / IPAA (1000)
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB,KCOREK(MAXMAT)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX,
     +                TABLAG(20)
      COMMON /DOUBLE/ LDOUBL(50)
C-----ADDED BY JAIS K.KANEKO  6/13/84-----------------------------------
CKSK  COMMON /PLTCNT/ JJPLT
C-----END END END END END END END END-----------------------------------
C
      DIMENSION   IAA(380)
      EQUIVALENCE (AA(1),IAA(1))
C
      COMMON /PCOSF1/ K1 ,K2 ,K3 ,K4 ,K5 ,K6 ,K7 ,K8 ,K9 ,K10,
     1                K11,KK1,KK2,KK3,KK4,KK5,KK6,KK7,KK8,KK9,
     2                KKA,K12,K13,K14,K15,K16,K17,K18,K19,K20,
     3                K21,K22,K222,K23,K24,K25,K26,K27,K28,K29,K30,
     4                K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,
     5                K41,K42,K43,K44,K45,K46,K47,K48,K49,K50,
     6                K51,K52,K53,K54,K55,K56,K57,K58,K59,K60,
     8                K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,
     8                K71,K72,K73,KKMAX,MAXX
      COMMON /PCOSF2/ J1 ,J2 ,J3 ,J4 ,J5 ,J6 ,J7 ,JJMAX
      COMMON /PCOSF3/ M1 ,M2 ,M3 ,MMMAX
C
      COMMON /WORK  / A(100000)
C
C     START OF PROCESS
C
      MEMORY   = IMAA(96)
      NOUT1    = IMAA(64)
      NOUT2    = IMAA(65)
C
      CALL  CLEA(A     ,MEMORY,0.0)
C
      IBURN    = IMAA(79)
      IF(IMAA(20).GT.0) IBURN = IBURN + 1
      IF(IMAA(20).GT.0) IPCNT = IPCNT + 1
CADD96 BY K.KANEKO
      IF(IMAA(20).LE.0) IPCNT = 0
C
      IF(IPCNT.LE.1) THEN
                     WRITE(NOUT1,1)
CKSK                 CALL  REAI(KCOREK,1,4HPLOT,4H OPT)
                     CALL  REAI(KCOREK,1,'PLOT',' OPT')
                     IPLOT  =   KCOREK(1)
                     CALL CLEA ( KCOREK , MAXMAT , 0 )
                     ENDIF
C
    1 FORMAT(1H ,' PLOT OPTION READ IN FREE FORMAT (0/1/2:NO/YES/YES)')
C
C----- IPATH SETTING
C
      IPATH = 0
      ISWISW=IMAA(5)
      IF(ISWISW.EQ.-2) IPATH= 1
      IF(LDOUBL(1).EQ.1.OR.LDOUBL(1).EQ.3)  IPATH = 2
C
      LCOMP =IPAA(33)
      KCOMP =LCOMP
      KSREG =IPAA(4)
      LSREG =KSREG
      IF(IPATH.EQ.2)   KCOMP=LCOMP+2
      IF(IPATH.EQ.2)   KSREG=LDOUBL(5)
      NMAT  =IMAA(59)
      KNMAX =0
      IPLT  =0
      IF(IPLOT.GT.0)  IPLT=1
      LCNISO=IMAA(86)
C
      DO 20 I=1,NMAT
      ISW=IAA(LCNISO+I-1)
      IF(ISW.GT.KNMAX) KNMAX=ISW
   20 CONTINUE
C
C       *********************************************
C       *    ADDRESS SETTING FOR VAIRABLE DIMENION  *
C       *********************************************
C
C
C           ADDRESS   VARIABLE         SIZE
C           -------   --------    ----------------------------
C             K01      VOLM        KCOMP
C             K02      VOLR        KSREG
C             K03      MMR         KSREG
C             K04      MTNAME      KCOMP *   2
C             K05      XL (BL)     KCOMP
C             K06      TEMP        KCOMP
C             K07      NISO        KCOMP
C             K08      IDENT       KNMAX * KCOMP *   2
C             K09      IRES        KNMAX * KCOMP
C             K10      DN (DEN)    KNMAX * KCOMP
C             K11      LXMICR      KNMAX * KCOMP
C             KK1      IDRREG      KSREG
C             KK2      IUSE        KCOMP
C             KK3      VOLR0       LSREG
C             KK4      MAR0        LSREG
C             KK5      KCODEL      KNMAX * KCOMP *   2
C             KK6      KREST       KNMAX * KCOMP
C             KK61     KUCPOS      KNMAX * KCOMP
C             KK7      DEN         KRES  * KCOMP
C             KK8      RATIO       KSREG
C             KK9      PDSFLX      LSREG * NEF
C             K12      FLUXS       KSREG * NGMAX
C             K13      NCOR        KCOMP
C             K14      ISWF        KCOMP
C             K15      MCODE       KNMAX * KCOMP
C             K16      NCODEL      KMAT  *   2
C             K17      IREST       KMAT
C             K18      AMU         KMAT
C             K19      SIGF        KMAT  *  NG
C             K20      SIGNU       KMAT  *  NG
C             K21      SIGS        KMAT  *  NG
C             K22      SIGA        KMAT  *  NG
C             K222     SIGS0       KMAT
C             K23      VCAP        KMAT
C             K24      SSFEFC      3     *  NG *  KRES * KCOMP
C             K25      IRCONT      6     * KRES
C             K26      ISWFIS      KRES
C             K27      BETA        KMAT  *  2             ( REAL*8 )
C             K28      UMAX        KMAT  *  2             ( REAL*8 )
C             K29      ADEN        KMAT
C             K30      SIG         KCOMP
C             K31      RATD        KCOMP
C             K32      SUM         KCOMP
C             K33      PHI         KSREG *  2             ( REAL*8 )
C             K34      S           KSREG *  2             ( REAL*8 )
C             K35      SA          KMAT  * KCOMP
C             K36      SF          KMAT  * KCOMP
C             K37      SS          KMAT  * KCOMP
C             K38      PIJ         KSREG * KSREG
C             K39      PHIRR       KSREG * NG
C             K40      PHIXR       KCOMP * NG
C             K41      RIAR        KRES  * KCOMP * NG
C             K42      RIFR        KRES  * KCOMP * NG
C             K43      RISR        KRES  * KCOMP * NG
C             K44      XESCAB      KCOMP * NG
C             K45      XSECFI      KCOMP * NG
C             K46      XSECSS      KCOMP * NG
C             K47      XSECFN      KCOMP * NG
C             K48      SIGMA       KCOMP
C             K49      C           KCOMP *  2                  (REAL*8)
C             K50      X           KCOMPF
C             K51      NX          KCOMPF
C             K52      FLUX        KSREG * NBB * KFGP * 2      (REAL*8)
C             K53      SCATH       KSREG * KRES* KFGP * NBH*2  (REAL*8)
C             K54      FLUXSS      KSREG
C             K55      QQ          KPIJ1 * MAXN  * MAXP
C             K56      DANX        MAXN  * KDAN
C             K57      DANY        MAXN  * KDAN
C             K58      DAN         MAXP
C             K59      SIGWW       KPIJ2 * KCOMP
C             K60      SAA         KRES  * MAXINT* KFGP
C             K61      SFF         KRES  * MAXINT* KFGP
C             K62      SSS         KRES  * MAXINT* KFGP
C             K63      SUMA        KRES
C             K64      SUMF        KRES
C             K65      SUMS        KRES
C             K66      RIAS        KMAT  * KSREG
C             K67      RIFS        KMAT  * KSREG
C             K68      RISS        KMAT  * KSREG
C             K69      QIJ         LSREG * LSREG
C             K70      NGFLX       NBB
C             K71      RIER        KRES  * KCOMP * NG
C             K72      XSECER      KCOMP * NG
C             K73      RIES        KMAT  * KSREG
C             K74      RFLUX       KSREG * KFGP  * MAXINT
C
C             J1(=K27) SSMIC       NGMAX *  10 * 3
C             J2       Y1          NGMAX *  10  + 3
C             J3       X1          NGMAX *  10  + 3
C             J4       Y           NGMAX *  10  + 3
C
C             M1(=K12) FLUXS       NGMAX * KSREG
C             M2       X1          NGMAX *  5
C             M3       Y1          NGMAX *  5
C
C
      K1 =1
      K2 =K1 +KCOMP
      K3 =K2 +KSREG
      K4 =K3 +KSREG
      K5 =K4 +KCOMP*2
      K6 =K5 +KCOMP
      K7 =K6 +KCOMP
      K8 =K7 +KCOMP
      K9 =K8 +KNMAX*KCOMP*2
      K10=K9 +KNMAX*KCOMP
C
      LOC00 = K10
      IF(MOD(K10,2).EQ.0) LOC00 = LOC00 + 1
      LEMORY= MEMORY - LOC00 + 1
      CALL  UMCROS(A(LOC00),LEMORY,A(K6),A(K7),A(K8),A(K9),0)
C
      K11=K10+KNMAX*KCOMP
      MXKMAT = KCOMP*KNMAX
      LENFLX = NEF*LSREG
C
      KK1=K11+KNMAX*KCOMP
      KK2=KK1+KSREG
      KK3=KK2+KCOMP
      KK4=KK3+LSREG
      KK5=KK4+LSREG
      KK6=KK5+MXKMAT*2
      KK61=KK6+MXKMAT
      KK62=KK61+MXKMAT
C@ADD
      LCMATD = IPAA(50) + 50
C
      CALL PCOIN1(A(K1 ),A(K2 ),A(K3 ),A(K4 ),A(K5 ),A(K6 ),A(K7 ),
     1            A(K8 ),A(K9 ),A(K10),A(K11),ID    ,TIL   ,KCOMP ,
     2            KNMAX ,KSREG ,KMAT  ,NDOUBL,NEF   ,KRES  ,NMAT  ,
     3            A(KK1),A(KK2),A(KK3),A(KK4),A(KK5),A(KK6),
C@MOD4            A(KK61),MXKMAT)
     4            A(KK61),A(KK62),MXKMAT , IPAA(LCMATD) )
C
      IF(KRES.LE.0)   GO TO 601
C
      KK7=KK5
      KK8=KK7+KRES*KCOMP
      KK9=KK8+KSREG
      KKA=KK9+NEF*LSREG
      K12=KKA
      K13=K12+KSREG*NGMAX
      K14=K13+KCOMP
      K15=K14+KCOMP
      K16=K15+KNMAX*KCOMP
      K17=K16+KMAT*2
      K18=K17+KMAT
      K19=K18+KMAT
      K20=K19+KMAT*NG
      K21=K20+KMAT*NG
      K22=K21+KMAT*NG
      K222=K22+KMAT*NG
CM    K23=K22+KMAT*NG
      K23=K222+KMAT
      K24=K23+KMAT
      K25=K24+KRES*NG*3*KCOMP
      K26=K25+KRES*6
      K27=K26+KRES
C
      CALL PCOIN2(A(K1 ),A(K2 ),A(K3 ),A(K4 ),A(K5 ),A(K6 ),A(K7 ),
     1            A(K8 ),A(K9 ),A(K10),A(K13),A(K14),A(K15),A(K16),
     2            A(K17),A(K18),A(K19),A(K20),A(K21),A(K22),A(K23),
C@MOD3            A(KK1),A(KK2),A(KK7),A(K222))
     3            A(KK1),A(KK2),A(KK7),A(K222),NMAT,IPAA(LCMATD) )
C
      IF(KCOMPF.LE.0)  GO TO 701
C
      LOC00 = K26
      IF(MOD(K26,2).EQ.0) LOC00 = LOC00 + 1
      LEMORY= MEMORY - LOC00 + 1
C
      CALL  UMCROS(A(LOC00),LEMORY,A(K6),A(K7),A(K8),A(K9),1)
C
      LENG  = MEMORY - K24 + 1
      CALL  CLEA  ( A(K24)  , LENG , 0.0 )
      CALL PCOIN3(A(K4 ),A(K6 ),A(K7 ),A(K8 ),A(K9 ),A(K10),A(K13),
     1            A(K14),A(K15),A(K16),A(K24),A(K25),A(K26) )
C
      KPIJ1 = 11
      IF(IPATH.EQ.2)  KPIJ1 = 20
      MAXP  = (KSREG*(KSREG+1))/2
      IF(IPATH.EQ.2)  MAXP = KSREG*KSREG
      MAXN  =  1
      IF(KCOMPF.EQ.2) MAXN  = 11
      KDAN  = MAXP
      IF(KCOMPF.EQ.1) KDAN  =  1
      KPIJ2 = 11
      IF(IPATH .EQ.2) KPIJ2 = 20
      IF(KCOMPF.GT.1) KPIJ2 =121
C
CM    K27=K27+MOD(K27,2)
      K27=K27+MOD(K27,2)+1
      K28=K27+KMAT*2
      K29=K28+KMAT*2
      K30=K29+KMAT
      K31=K30+KCOMP
      K32=K31+KCOMP
      K33=K32+KCOMP
CM    K33=K33+MOD(K33,2)
      K33=K33+MOD(K33,2)+1
      K34=K33+KSREG*2
      K35=K34+KSREG*2
      K36=K35+KMAT*KCOMP
      K37=K36+KMAT*KCOMP
      K38=K37+KMAT*KCOMP
      K39=K38+KSREG*KSREG
C
      CALL PCODAT(A(K2 ),A(K3 ),A(K5 ),A(K7 ),A(K9 ),A(K10),A(K13),
     1            A(K14),A(K15),A(K16),A(K18),A(K19),A(K21),A(K22),
     2            A(K27),A(K28),A(K29),A(K30),A(K31),A(K32),A(K33),
     3            A(K34),A(K35),A(K36),A(K37),A(K38),
     4            A(KK1),A(KK3),A(KK7),A(KK8),A(KK9),A(K39),A(K222))
C
C     WRITE(NOUT2,7001) NOUT1,NOUT2,MAXN,MAXP,KDAN,KPIJ1,KPIJ2,IPLOT
C7001 FORMAT(//1H ,' NOUT1,NOUT2,MAXN,MAXP,KDAN,KPIJ1,KPIJ2,IPLOT ',
C    +8I6/)
C
      K40=K39+KSREG*NG
      K41=K40+KCOMP*NG
      K42=K41+KRES*KCOMP*NG
      K43=K42+KRES*KCOMP*NG
      K44=K43+KRES*KCOMP*NG
      K45=K44+KCOMP*NG
      K46=K45+KCOMP*NG
      K47=K46+KCOMP*NG
      K48=K47+KCOMP*NG
      K49=K48+KCOMP
CM    K49=K49+MOD(K49,2)
      K49=K49+MOD(K49,2)+1
      K50=K49+KSREG*2
      K51=K50+KCOMPF
      K52=K51+KCOMPF
CM    K52=K52+MOD(K52,2)
      K52=K52+MOD(K52,2)+1
      K53=K52+KSREG*NBB*KFGP*2
      K54=K53+KSREG*KRES*KFGP*NBH*2
      K55=K54+KSREG
      K56=K55+KPIJ1*MAXN*MAXP
      K57=K56+MAXN*KDAN
      K58=K57+MAXN*KDAN
      K59=K58+MAXP
      K60=K59+KPIJ2*KCOMP
      K61=K60+KRES*MAXINT*KFGP
      K62=K61+KRES*MAXINT*KFGP
      K63=K62+KRES*MAXINT*KFGP
      K64=K63+KRES
      K65=K64+KRES
      K66=K65+KRES
      K67=K66+KMAT*KSREG
      K68=K67+KMAT*KSREG
      K69=K68+KMAT*KSREG
      K70=K69+LSREG*LSREG
      K71 =K70+NBB
      K72 =K71+KRES*KCOMP*NG
      K73 =K72+KCOMP*NG
CM    KKMAX=K73+KMAT*KSREG
      K74 =K73+ KMAT*KSREG
      KKMAX=K73+KFGP*NGMAX*KSREG
C
      LENG  = NGMAX*KFGP
      LENG3 = LENG + 3
      J1    = K27
      J2    = J1 + LENG3*NDOUBL
      J3    = J2 + LENG3*NDOUBL
      J4    = J3 + LENG3*NDOUBL
      J5    = J4 + LENG3*IPLT*NDOUBL
      J6    = J5 + LENG3*IPLT*NDOUBL
      J7    = J6 + LENG3*IPLT*NDOUBL
      JJMAX = J7 + LENG3*IPLT*NDOUBL
C
      M1    = K12
      M2    = M1 + NGMAX*KSREG
      M3    = M2 + NGMAX*5*IPLT + 10
      MMMAX = M3 + NGMAX*5*IPLT + 10
C
      MAXX  = MAX0(KKMAX,JJMAX,MMMAX)
      IMAA(80)=MAXX
      IF(MAXX.GT.MEMORY)  GO TO 999
C
      IF(IMAA(19).GT.0)  CALL PCOSFX(NOUT2)
C
      CALL  PCOAVG(A(K1 ),A(K2 ),A(K3 ),A(K4 ),A(K5 ),A(K6 ),A(K7 ),
     1             A(K8 ),A(K9 ),A(K10),A(K12),A(K13),A(K14),A(K15),
     2             A(K16),A(K18),A(K19),A(K20),A(K21),A(K22),A(K23),
     3             A(K24),A(K25),A(K26),A(K27),A(K28),A(K29),A(K30),
     4             A(K31),A(K32),A(K33),A(K34),A(K35),A(K36),A(K37),
     5             A(K38),A(K39),A(K40),A(K41),A(K42),A(K43),A(K44),
     6             A(K45),A(K46),A(K47),A(K48),A(K49),A(K50),A(K51),
     7             A(K52),A(K53),A(K54),A(K55),A(K56),A(K57),A(K58),
     8             A(K59),A(K60),A(K61),A(K62),A(K63),A(K64),A(K65),
     9             A(K66),A(K67),A(K68),A(K69),A(KK1),A(KK3),A(KK4),
     A             A(KK8),A(K222),A(K70),A(K71),A(K72),A(K73),A(K74))
C
      CALL  PCOOUT(A(K1 ),A(K2 ),A(K3 ),A(K4 ),A(K5 ),A(K6 ),A(K7 ),
     1             A(K8 ),A(K9 ),A(K10),A(K11),A(K12),A(K13),A(K14),
     2             A(K15),A(K16),A(K17),A(K39),A(K40),A(K41),A(K42),
     3             A(K43),A(K44),A(K45),A(K46),A(K47),A(KK1),A(KK2),
     4             A(KK9),A(KK4),A(K71),A(K72))
C
      IF(NDOUBL.EQ.1) THEN
                      LENG  = NGMAX*10
                      LENG3 = LENG + 3
                      CALL  PCOMCR(A(K16),A(K25),A(J1),A(J2),A(J3),
     +                      A(J4) ,A(J5 ),A(J6 ),A(J7),LENG ,LENG3)
                      ENDIF
C
      IF(IPLOT.GT.0) THEN
                CALL PCOPLT(A(K3 ),A(K4 ),A(K6 ),A(M1 ),A(M2 ),A(M3 ))
                     ENDIF
C
CKSK FOR UNIX VERSION(PIFLIB) 2/22/96 BY K.OKUMURA
CKSK  IF(IMAA(97).GT.0) THEN
CKSK                    CALL PLOT( 0.0 , 0.0 , 999 )
CKSK                    IMAA(97) = 0
CKSK                    JJPLT    = 0
CKSK                    ENDIF
C
C     END OF PROCESS
C
      IMAA(80) = 0
      RETURN
C
C-----SKIPPED CASE 1
C
  601 CONTINUE
      WRITE(NOUT1,61)
      RETURN
C
   61 FORMAT(/1H ,10X,'NO RESONANT NUCLIDE EXISTS IN THIS PROBREM',
     +       /1H ,10X,'SO PEACO ROUTINE IS SKIPPED ]] ')
C
C-----SKIPPED CASE 2
C
  701 CONTINUE
      WRITE(NOUT1,71)
      RETURN
C
   71 FORMAT(/1H ,10X,'NO FUEL MATERIAL EXISTS IN THIS PROBREM',
     +       /1H ,10X,'SO PEACO ROUTINE IS SKIPPED ]] ')
C
  999 CONTINUE
      WRITE(NOUT1,99) MAXX,MEMORY
      WRITE(NOUT2,99) MAXX,MEMORY
C
   99 FORMAT(/1H ,10X,'DIMENSION OVER ----- STOP EXECUTED AT PEACO ]]',
     +       /1H ,10X,'REQUIRED MEMORY ---- ',I10,
     +       /1H ,10X,'RESERVED MEMORY ---- ',I10)
C
      STOP
      END

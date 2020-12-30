      SUBROUTINE  PCOOUT
     1 (VOLM  ,VOLR  ,MAR   ,MTNAME,BL    ,TEMP  ,NISO  ,IDENT ,IRES  ,
     2  DN    ,LXMICR,FLUXS ,NCOR  ,ISWF  ,MCODE ,NCODEL,IREST ,PHIRR ,
     3  PHIXR ,RIAR  ,RIFR  ,RISR  ,XSECAB,XSECFI,XSECEL,XSECFN,
     4  IDRREG,IUSE  ,PDSFLX,MAR0  ,RIER  ,XSECER )
C
      CHARACTER*4     TIL,ID,NFILE,IDTEMP
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK3/ A(14500),LOCAM(8)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
      COMMON /TMPSET/ STND(35),IDTEMP(61),NTDUMY
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
      COMMON /MAINC / IMAA(500)
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
C
CMOD  PARAMETER   ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
      INCLUDE  'BMICRINC'
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
      DIMENSION
     1  VOLM(KCOMP),VOLR(KSREG),NCOR(KCOMP),ISWF(KCOMP),MAR(KSREG),
     2  FLUXS(NGMAX,KSREG),BL(KCOMP),TEMP(KCOMP),
     3  NISO(KCOMP),IRES(KNMAX,KCOMP),
     4  DN(KNMAX,KCOMP),MCODE(KNMAX,KCOMP),NCODEL(2,KMAT),IREST(KMAT),
     5  PHIRR(KSREG,NG),PHIXR(KCOMP,NG),RIAR(KRES,KCOMP,NG),
     6  RIFR(KRES,KCOMP,NG),RISR(KRES,KCOMP,NG),XSECAB(KCOMP,NG),
     7  XSECFI(KCOMP,NG),XSECEL(KCOMP,NG),XSECFN(KCOMP,NG),
     8  LXMICR(KNMAX,KCOMP),MAR0(LSREG)
C
       DIMENSION     IDRREG(KSREG),IUSE(KCOMP),PDSFLX(LSREG,NEF)
       DIMENSION     RIER(KRES,KCOMP,NG),XSECER(KCOMP,NG)
       CHARACTER*8   IDENT(KNMAX,KCOMP),MTNAME(KCOMP)
C
       DIMENSION     IA(9000),TMPSUM(10),FLXTOT(74)
       DIMENSION     SIGDEL(74),SIGS(104),SIGP1(74)
       DIMENSION     RIANS(74),RIFLX(74)
       CHARACTER*8   MEMBER,MEMBE2
C
       EQUIVALENCE (IA(1),A(1))
C
C      START OF PROCESS
C
       DELU= ALOG(EE(1)/EE(NG+1))
      IF(IPLOT.EQ.0) GO TO 220
C     *********************************************************
C     * EDITTING OF CALCULATED FLUX     (PRINT-OUT,DISK-OUT)  *
C     *********************************************************
C ----- FLUX PRINT OUT
      WRITE(NOUT2,1000) TIL,ID
      MCUT     = FLOAT(KCOMP)/10.0 + 0.99
      DO 110 M = 1,MCUT
      M1       =(M-1)*10+1
      M2       =  M  *10
      IF(M2.GT.KCOMP)  M2=KCOMP
      CALL CLEA( TMPSUM , 10 , 0.0)
C
      WRITE(NOUT2,1001) M,(MTNAME(J),J=M1,M2)
      WRITE(NOUT2,1006)    (VOLM(J),J=M1,M2)
      WRITE(NOUT2,1008)
      WRITE(NOUT2,1007)
      DO 100 I = 1,NG
      WRITE(NOUT2,1002) I,EE(I),(PHIXR(J,I),J=M1,M2)
      DO 100 J = M1,M2
      TMPSUM(J-M1+1) = TMPSUM(J-M1+1) + PHIXR(J,I)
  100 CONTINUE
      WRITE(NOUT2,1007)
      WRITE(NOUT2,1009) DELU,(TMPSUM(J-M1+1),J=M1,M2)
  110 CONTINUE
C
      MCUT     = FLOAT(KSREG)/10.0 + 0.99
C
      DO 210 M = 1,MCUT
      M1       = (M-1)*10 + 1
      M2       =  M  *10
      IF(M2.GT.KSREG)  M2=KSREG
      CALL CLEA( TMPSUM , 10 , 0.0)
C
      WRITE(NOUT2,1004) M,(I,I=M1,M2)
      WRITE(NOUT2,1005) (MTNAME(MAR(I)),I=M1,M2)
      WRITE(NOUT2,1006) (VOLR(I),I=M1,M2)
      WRITE(NOUT2,1008)
      WRITE(NOUT2,1007)
      DO 200 I = 1,NG
      WRITE(NOUT2,1002) I,EE(I),(PHIRR(K,I),K=M1,M2)
      DO 200 J = M1,M2
      TMPSUM(J-M1+1) = TMPSUM(J-M1+1) + PHIRR(J,I)
  200 CONTINUE
      WRITE(NOUT2,1007)
      WRITE(NOUT2,1009) DELU,(TMPSUM(J-M1+1),J=M1,M2)
  210 CONTINUE
C
C ---- FLUX DISK-OUT TO PDS-FILE
C
  220 IFLSW    = 1
CKSK  NFILE(1) = 4HFLUX
      NFILE(1) = 'FLUX'
CKSK  NFILE(2) = 4H
      NFILE(2) = '    '
      MEMBER   = ID(1) (1:4) // 'F002'
C
      IBURN    = IMAA(79)
      IF(IBURN.GT.0) MEMBER(6:6) = IDTEMP(IBURN) (4:4)
C
      CALL SEARCH(MEMBER,LENG,ISW)
      IF(ISW.EQ.1)        GO TO 301
      IF(LENG.GT.LENFLX)  THEN
                          WRITE(NOUT2,1061) LENG,LENFLX,MEMBER
                          GO TO 301
                          ENDIF
C
      CALL CLEA(PDSFLX,LENFLX,0.0)
      CALL READ(MEMBER,PDSFLX,LENG)
C
      LENGL = NG*LSREG
      CALL CLEA( PDSFLX(1,ISTART) , LENGL , 0.0 )
C
           DO 240  N = 1 , NG
           NGROUP = ISTART + N - 1
           DO 230 J = 1 , KSREG
           ISWPOS = IABS(IDRREG(J))
           PDSFLX(ISWPOS,NGROUP) = PDSFLX(ISWPOS,NGROUP) + PHIRR(J,N)
  230      CONTINUE
CM         WRITE(NOUT2,238) N,(PDSFLX(J,NGROUP),J=1,LSREG)
  240      CONTINUE
C
      CALL DELETE(MEMBER)
      CALL WRITE (MEMBER,PDSFLX,LENG)
C --- DISK-OUT END
  301 CONTINUE
C     **************************************************************
C     * EDITTING OF EFFECTIVE MACRO X-SECTION  (PRINT-OUT,DISK-OUT)*
C     **************************************************************
C ----- MACRO PRINT OUT
      SUMRAT    = 0.0
      DO  400  K=1,KCOMP
      IF(IPLOT.GT.0) WRITE(NOUT2,1010) MTNAME(K)
      DO  300  N=1,NG
      IF(IPLOT.GT.0)    THEN
      WRITE(NOUT2,1011) N,EE(N),EE(N+1),XSECAB(K,N),XSECFI(K,N),
     +                  XSECFN(K,N),XSECEL(K,N),XSECER(K,N)
                        ENDIF
      IF(IUSE(K).GT.1)  GO TO 300
      TOTAL     = XSECAB(K,N) + XSECFI(K,N) + XSECEL(K,N)
      SUMRAT    = SUMRAT      + PHIXR(K,N)*TOTAL
  300 CONTINUE
      IF(IPLOT.GT.0) WRITE(NOUT2,1012)
  400 CONTINUE
C ----- DISK-OUT TO PDS-FILE (MACRO)
      IFLSW=1
CKSK  NFILE(1)=4HMACR
      NFILE(1)='MACR'
CKSK  NFILE(2)=4HOWRK
      NFILE(2)='OWRK'
C-----LOOP OF MATERIAL
      DO  440 K=1,KCOMP
      IF(IBURN.GT.0) THEN
      IF(K.LE.LCOMP.AND.ISWF(K).NE.3.AND.IUSE(K).EQ.0)  GO TO 440
      IF(K.EQ.MICMOD)                                   GO TO 440
                     ENDIF
C
      MEMBER  = MTNAME(K)
      CALL SEARCH(MEMBER,LENG,ISW)
      IF(ISW.EQ.1)      GO TO 440
      IF(LENG.GT.9000)  THEN
                        WRITE(NOUT2,1041) LENG,MEMBER
                        GO TO 440
                        ENDIF
C
      MEMBE2       = MTNAME(K)
      MEMBE2 (8:8) = '4'
      CALL CLEA(A     ,9000,0.0 )
      CALL READ(MEMBE2,A   ,LENG)
      IST       = 0
      DO 435  I = 1,NEF
      LGV       = IA(IST+2)
      SIGDEL(I) = A(IST+6)
      DIFF      = A(IST+8)
      SIGP1 (I) = A(IST+6) -  0.33333333333/DIFF
  435 IST       = IST + 10 + LGV
C
CM    WRITE(NOUT2,436) MEMBE2
CM    WRITE(NOUT2,437) (SIGDEL(I),I=1,NEF)
C
      CALL CLEA(A     ,9000,0.0 )
      CALL READ(MEMBER,A   ,LENG)
      IST       = 0
      DO 430  I = 1,NEF
      LGV       = IA(IST+2)
      SIGDEL(I) =  SIGDEL(I) - A(IST+6)
      IF(I.LT.ISTART) GO TO 425
      II        = I-ISTART+1
      SIGC      = XSECAB(K,II)
      SIGF      = XSECFI(K,II)
      SIGE      = XSECEL(K,II)
      SIGFNU    = XSECFN(K,II)
      SIGER     = XSECER(K,II)
      SIGT      = SIGC + SIGF + SIGE - SIGDEL(I)
      IF(SIGT.NE.0.0) SAVET = 0.33333333333/SIGT
      A(IST+ 4) = SIGF
      A(IST+ 5) = SIGFNU
      A(IST+ 6) = SIGT
      A(IST+ 8) = SAVET
      A(IST+ 9) = SAVET
      A(IST+10) = SIGC + SIGF
      CALL CLEA(SIGS,104,0.0)
      SUM       = 0.0
      DO 421  J = 2 , LGV
      IPOS      = IST + 10 + J
      SUM       = SUM + A(IPOS)
      SIGS(J)   = A(IPOS)
  421 CONTINUE
      SIGS(1)   = SIGE - SIGER - SIGDEL(I)
      FACTOR    = 1.0
      IF(SUM.NE.0.0)  FACTOR = SIGER/SUM
      DO 422 J  = 2 , LGV
      SIGS(J)   = FACTOR * SIGS(J)
  422 CONTINUE
      DO 423 J  = 1 , LGV
      IPOS      = IST + 10 + J
      A   (IPOS)= SIGS(J)
  423 CONTINUE
  425 IST       = IST + 10 + LGV
  430 CONTINUE
CM    WRITE(NOUT2,438) (SIGDEL(I),I=1,NEF)
      CALL  DELETE(MEMBER)
      CALL  WRITE (MEMBER,A,LENG)
C
      IST       = 0
      DO 1430 I = 1,NEF
      LGV       = IA(IST+2)
      SAVET     = A(IST+ 6) + SIGDEL(I)
      A(IST+ 6) = SAVET
      SAVETR    = SAVET     - SIGP1 (I)
      IF(SAVETR.NE.0.0)    THEN
                           A(IST+ 8) = 0.3333333333/SAVETR
                           A(IST+ 9) = 0.3333333333/SAVETR
                           ENDIF
      A(IST+11) = A(IST+11) + SIGDEL(I)
      IST       = IST + 10 + LGV
 1430 CONTINUE
      CALL  DELETE(MEMBE2)
      CALL  WRITE (MEMBE2,A,LENG)
  440 CONTINUE
C     **************************************************************
C     * EDITTING OF EFFECTIVE MICRO X-SECTION  (PRINT-OUT,DISK-OUT)*
C     *          <  ONLY RESONANT NUCLIDE >                        *
C     **************************************************************
      VOLTOT = 0.0
      CALL CLEA( FLXTOT , 74 , 0.0 )
      DO 2000 J = 1,KSREG
      VOLTOT    = VOLTOT    + VOLR  (J)
      DO 2000 I = 1 , NG
      FLXTOT(I) = FLXTOT(I) + PHIRR(J,I)
 2000 CONTINUE
C ----- MICRO PRINT OUT
      IFLSW     = 1
CKSK  NFILE(1)  = 4HMICR
      NFILE(1)  = 'MICR'
CKSK  NFILE(2)  = 4HEF
      NFILE(2)  = 'EF  '
C-----LOOP OF MATERIAL
      DO 600  K = 1,KCOMP
      IF(K.LE.LCOMP.AND.ISWF(K).NE.3.AND.IUSE(K).EQ.0)  GO TO 600
      IF(K.EQ.MICMOD)                                   GO TO 600
      MMK       = NISO(K)
      IF(MMK.LE.0)      GO TO 600
C
      MICOUT    = 0
      MEMBER    = MTNAME(K)(1:4)// 'BMIC'
      LENG      = MAXNG*MAXMT3*MMK
      CALL  READ( MEMBER , EFFMIC , LENG )
C-----LOOP OF NUCLIDE
      DO 500  M = 1,MMK
      IF(IRES(M,K).NE.2)  GO TO 500
      IF(DN(M,K).LE.0.0)  GO TO 500
      MK        = MCODE(M,K)
      IF(MK.LE.0)         GO TO 500
      IF(MK.GT.KRES)      GO TO 500
      MICOUT    = MICOUT + 1
      IF(IMAA(19).EQ.0)   GO TO 451
C
      CALL CLEA( TMPSUM , 10 , 0.0)
      WRITE(NOUT2,1020) MTNAME(K),IDENT(M,K)
      DO 450  I = 1,NG
      DELUNG    = ALOG(EE(I)/EE(I+1))
      WRITE(NOUT2,1021) I,EE(I),EE(I+1),RIAR(MK,K,I),
     +      RIFR(MK,K,I),RISR(MK,K,I),RIER(MK,K,I)
      TMPFLX    = PHIXR(K,I)*VOLTOT*DELUNG / ( FLXTOT(I)*VOLM(K) )
      TMPSUM(1) = TMPSUM(1) + RIAR(MK,K,I)*TMPFLX
      TMPSUM(2) = TMPSUM(2) + RIFR(MK,K,I)*TMPFLX
      TMPSUM(3) = TMPSUM(3) + RISR(MK,K,I)*TMPFLX
  450 CONTINUE
      WRITE(NOUT2,1022)
      WRITE(NOUT2,1023) TMPSUM(1),TMPSUM(2),TMPSUM(3)
C
  451 IOUT = 0
      IST  = ISTART - 1
      DO 455 I = 1 , NG
      EFFMIC(I+IST,1,M) = RIAR(MK,K,I)
      SAVOLD            = EFFMIC(I+IST,2,M)
      SAVFNU            = EFFMIC(I+IST,6,M)
      SAVNEW            = RIFR(MK,K,I)
      EFFMIC(I+IST,2,M) = SAVNEW
      EFFMIC(I+IST,4,M) = RISR(MK,K,I)
      EFFMIC(I+IST,5,M) = RIER(MK,K,I)
      IF(SAVOLD.GT.0.0) THEN
             EFFMIC(I+IST,6,M) =  SAVFNU*SAVNEW/SAVOLD
             ENDIF
  455 CONTINUE
C
      IF(LXMICR(M,K).EQ.1) IOUT = 1
      IF(LXMICR(M,K).EQ.3) IOUT = 1
      IF(IOUT.EQ.0)        GO TO 500
C ---- DISK-OUT TO PDS-FILE (EFFECTIVE MICROSCOPIC X-SECTION)
      MEMBER       = IDENT(M,K)
      MEMBER (1:1) = 'M'
      MEMBER (5:5) = 'F'
      MEMBER (6:7) = MTNAME(K) (6:7)
      CALL SEARCH(MEMBER,LENG,ISW)
      IF(ISW.EQ.1)          GO TO 500
      IF(LENG.GT.9000)      THEN
                            WRITE(NOUT2,1071) LENG,MEMBER
                            GO TO 500
                            ENDIF
C
      CALL PCOCON(AMASS,SIG0,ICAPT,IFISS,NEF,IIRES,LTOT,MEMBER)
      ISTC   = LOCAM(1)-2+ISTART
      ISTF   = LOCAM(2)-2+ISTART
      ISTT   = LOCAM(5)-2+ISTART
CADD
      ISTTR  = LOCAM(6)-2+ISTART
      ISTE   = LOCAM(7)-2+ISTART
CEND
      MEMBER (1:1) = 'M'
      CALL CLEA(A     ,9000,0.0 )
      CALL READ(MEMBER,A   ,LTOT)
C
      DO 460 I = 1,NG
      SAVEAB   = 0.0
      IF(ICAPT.EQ.1) THEN
                     SAVEAB     = A(ISTC+I)
                     A(ISTC+I)  = RIAR(MK,K,I)
                     A(ISTT+I)  = A(ISTT +I) + RIAR(MK,K,I)
                     A(ISTTR+I) = A(ISTTR+I) + RIAR(MK,K,I)
                     ENDIF
      IF(IFISS.EQ.1) THEN
                     SAVEAB     = SAVEAB     + A(ISTF+I)
                     A(ISTF+I)  = RIFR(MK,K,I)
                     A(ISTT+I)  = A(ISTT +I) + RIFR(MK,K,I)
                     A(ISTTR+I) = A(ISTTR+I) + RIFR(MK,K,I)
                     ENDIF
C
                     SAVEAB     = SAVEAB     + A(ISTE+I)
                     A(ISTE+I)  = RISR(MK,K,I)
                     A(ISTT+I)  = A(ISTT +I) + RISR(MK,K,I)
                     A(ISTTR+I) = A(ISTTR+I) + RISR(MK,K,I)
C
      A(ISTT +I) = A(ISTT +I) - SAVEAB
      A(ISTTR+I) = A(ISTTR+I) - SAVEAB
  460 CONTINUE
C
      CALL  DELETE(MEMBER)
      CALL  WRITE (MEMBER,A,LTOT)
      IF(IMAA(19).EQ.0)   GO TO 500
C
            SUM      = 0.0
            CALL CLEA( RIANS , 74 , 0.0 )
            CALL CLEA( RIFLX , 74 , 0.0 )
            ISTC     = LOCAM(1)-1
            DO 470 I = 1, NEF
            FLX      = 0.0
            FLXSUM   = 0.0
                      DO 465 J = 1, LSREG
                      FLXSUM = FLXSUM + PDSFLX(J,I)
                      IF(MAR0(J).NE.K) GO TO 465
                      FLX    = FLX    + PDSFLX(J,I)
  465                 CONTINUE
            IF(FLX.EQ.0.0) GO TO 470
            DLUNG    = ALOG(ENERGY(I)/ENERGY(I+1))
            FLX      = FLX*VOLTOT*DLUNG / (FLXSUM*VOLM(K))
            RATE     = A(ISTC+I)*FLX
            RIANS(I) = RATE
            RIFLX(I) = FLX
            SUM      = SUM + RATE
  470       CONTINUE
            IF(SUM.EQ.0.0) GO TO 500
C
      CALL CLEA( TMPSUM, 10 , 0.0 )
      WRITE(NOUT2,471) MEMBER
      WRITE(NOUT2,472)
C
      DO 475  I = 1 , NEF
      RIWEIT    = RIANS(I)*100.0/SUM
      DELUNG    = ALOG(ENERGY(I)/ENERGY(I+1))
      WRITE(NOUT2,473) I,ENERGY(I),ENERGY(I+1),DELUNG,
     +                 RIFLX(I),A(ISTC+I),RIANS(I),RIWEIT
      TMPSUM(1) = TMPSUM(1) + DELUNG
      TMPSUM(2) = TMPSUM(2) + RIFLX(I)
      TMPSUM(3) = TMPSUM(3) + DELUNG*A(ISTC+I)
      TMPSUM(4) = TMPSUM(4) + RIANS(I)
      TMPSUM(5) = TMPSUM(5) + RIWEIT
  475 CONTINUE
      WRITE(NOUT2,472)
      WRITE(NOUT2,474) (TMPSUM(I),I=1,5)
CM    WRITE(NOUT2,476) VOLTOT
CM    WRITE(NOUT2,477) (FLXTOT(I),I=1,NG)
  500 CONTINUE
C
      IF(MICOUT.GT.0) THEN
              MEMBER    = MTNAME(K)(1:4)// 'BMIC'
              LENG      = MAXNG*MAXMT3*MMK
              CALL  OVRWRT ( MEMBER , EFFMIC , LENG )
              ENDIF
  600 CONTINUE
C
      IF(IPLOT.LE.0)     RETURN
      IF(IMAA(19).LE.2)  GO TO 801
C     **************************************************************
C     * EDITTING OF CALCULATED INTERMEDIATE GROUP FLUX             *
C     *          <  ONLY PRINT-OUT >                               *
C     **************************************************************
      MCUT     = FLOAT(KSREG)/10.0 + 0.99
      NCUT     = FLOAT(NGMAX)/50.0 + 0.99
      NLAST    = MCUT*NCUT
      ISW      = 0
C
      DO 800 N = 1,NCUT
      ING1     =(N-1)*50+1
      ING2     =  N  *50
      IF(ING2.GT.NGMAX)  ING2=NGMAX
      DO 800 M = 1,MCUT
      ISW      = ISW+1
      IST1     = (M-1)*10 + 1
      IST2     =  M  *10
      IF(IST2.GT.KSREG)  IST2=KSREG
      WRITE(NOUT2,1030) ISW,NLAST,(I,I=IST1,IST2)
      WRITE(NOUT2,1031)
      DO 700 I=ING1,ING2
      WRITE(NOUT2,1032) I,ENGD(I),(FLUXS(NGMAX-I+1,J),J=IST1,IST2)
  700 CONTINUE
      WRITE(NOUT2,1007)
  800 CONTINUE
C-----PRINT RESONANCE ESCAPE PROBABILITY
  801 CONTINUE
      ESCAPA = ESCAPA/SUMRAT
      ESCAPF = ESCAPF/SUMRAT
      ESCAPT = ESCAPA + ESCAPF
CM    ESCAPA = 1.0 - ESCAPA
CM    ESCAPF = 1.0 - ESCAPF
CM    ESCAPT = 1.0 - ESCAPT
      ESCAPA = EXP(- ESCAPA)
      ESCAPF = EXP(- ESCAPF)
      ESCAPT = EXP(- ESCAPT)
C
      WRITE(NOUT2,1051) EE(1),EE(NG+1),GUZAI,ESCAPA,ESCAPF,ESCAPT
C
C     END OF PROCESS
C
      RETURN
C
  238 FORMAT(1H ,' ## NG FLX ## ',I6,1P10E11.4/,(1H ,20X,1P10E11.4))
  436 FORMAT(1H ,' ## MEMBER(PCOOUT) ## ',A8)
  437 FORMAT(1H ,' ## SIG-T ## ',1P10E11.4)
  438 FORMAT(1H ,' ## SIGDEL## ',1P10E11.4)
  471 FORMAT(//1H ,30X,'## EFFECTIVE CAPTURE RESONACE INTEGRAL ##',
     +10X,'(ALL FAST ENERGY RANGE)',
     +/1H ,40X,'NUCLIDE NAME : ',A8,//,
     + 1H ,25X,' GROUP UPPER-E(EV) LOWER-E(EV) DELTA-U      FLUX
     +X-SECTION   R.I.        RATIO(%)')
  472 FORMAT(1H ,26X,15(6H------))
  473 FORMAT(1H ,25X,I6,1P7E12.5)
  474 FORMAT(1H ,25X,'  SUM ',24X,1P5E12.5)
  476 FORMAT(1H ,' ## VOLTOT ## ',1P5E12.5)
  477 FORMAT(1H ,' ## FLXTOT ## ',1P10E11.4)
C
 1000 FORMAT(1H1,10X,'CALCULATION RESULTS ( PEACO CODE )  ',
     +     //1H ,10X,'TITLE ----- ',18A4,'CASEID ----- ',2A4/)
 1001 FORMAT(//1H ,30X,'INTEGRATED FLUX (M-REGION)',30X,'PART ',I2,
     +     //1H ,'    MATERIAL NAME ',10(A8,3X))
 1008 FORMAT(1H ,' GROUP UPPER-E(EV)',30X,'FLUX')
 1009 FORMAT(1H ,' SUM(DEL-U=',F5.3,1H),1P11E11.4)
 1002 FORMAT(1H ,I6,1P11E11.4)
 1007 FORMAT(1H ,1X,26(5H-----))
C
 1004 FORMAT(//1H ,30X,'INTEGRATED FLUX (R-REGION)',30X,'PART ',I3,
     +     //1H ,'    R-REGION NO.  ',10(I3,8X))
 1005 FORMAT(1H ,'    MATERIAL NAME ',10(A8,3X))
 1006 FORMAT(1H ,'    VOLUME       ',1P10E11.4)
C
 1010 FORMAT(//1H ,30X,
     +'MACROSCOPIC CROSS SECTION',10X,'MATERIAL NAME = ',A8//
     + 1H ,25X,' GROUP UPPER-E(EV) LOWER-E(EV) CAPTURE     FISSION
     +NU*FISSION  ELASTIC    EL REMOVAL'/1H ,26X,15(6H------))
 1011 FORMAT(1H ,25X,I6,1P7E12.5)
 1012 FORMAT(1H ,26X,15(6H------))
C
 1020 FORMAT(//1H ,30X,
     +'MICROSCOPIC CROSS SECTION',10X,'MATERIAL NAME = ',
     +A8,'  NUCLIDE NAME = ',A8//1H ,25X,
     +' GROUP UPPER-E(EV) LOWER-E(EV) CAPTURE     FISSION     ELASTIC',
     +'    EL REMOVAL'/1H ,26X,13(6H------))
 1021 FORMAT(1H ,25X,I6,1P6E12.5)
 1022 FORMAT(1H ,26X,13(6H------))
 1023 FORMAT(1H ,25X,' EFFECTIVE RESONANCE INTEGRAL ',1P3E12.5)
C
 1030 FORMAT(1H1,30X,'NEUTRON SPECTRA (INERMEDIATE,R-REGION)',20X,
     +'PAGE ',I3,' OF ',I3//1H ,' R-REGION NO.      ',10(I3,8X))
 1031 FORMAT(1H ,' GROUP ENERGY(EV)  ',30X,'FLUX'/1H ,1X,26(5H-----))
 1032 FORMAT(1H ,I6,1P11E11.4)
C
 1041 FORMAT(/1H ,10X,'PROGRAM ERROR -- WORK DIMENSION SHORTAGE(SUBR. PC
     +OOUT)',/1H ,10X,'NEED LENTGTH  -- ',I6,'(FIXED LENGTH = 9000) ',
     +       /1H ,10X,'MACRO MODIFICATION (',A8,') IS SKIPED'/)
 1051 FORMAT(///1H ,20X,'RESONANCE ESCAPE PROBABILITY',
     +//1H ,10X,'ENERGY RANGE ----------- ',
     +'FROM ',F8.4,' TO ',F8.4,' EV  ( GUZAI = ',E12.5,')',
     +//1H ,10X,'CAPTURE  --------------- ',F10.6,
     + /1H ,10X,'FISSION  --------------- ',F10.6,
     + /1H ,10X,'ABSORPTION ------------- ',F10.6/)
C
 1061 FORMAT(/1H ,10X,'PROGRAM ERROR -- WORK DIMENSION SHORTAGE(SUBR. PC
     +OOUT)',/1H ,10X,'NEED LENTGTH  -- ',I6,'(FIXED LENGTH = ',I6,') ',
     +       /1H ,10X,'FLUX MODIFICATION (',2A4,') IS SKIPED'/)
 1071 FORMAT(/1H ,10X,'PROGRAM ERROR -- WORK DIMENSION SHORTAGE(SUBR. PC
     +OOUT)',/1H ,10X,'NEED LENTGTH  -- ',I6,'(FIXED LENGTH = 9000) ',
     +       /1H ,10X,'MICRO. MODIFICATION (',2A4,') IS SKIPED'/)
C
C
      END

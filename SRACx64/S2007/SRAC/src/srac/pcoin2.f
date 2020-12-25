      SUBROUTINE PCOIN2(VOLM  ,VOLR  ,MAR   ,MTNAME,BL    ,TEMP  ,
     1                  NISO  ,IDENT ,IRES  ,DN    ,NCOR  ,ISWF  ,
     2                  MCODE ,NCODEL,IREST ,AMU   ,SIGF  ,SIGNU ,
CMOD 3                  SIGS  ,SIGA  ,VCAP  ,IDRREG,IUSE  ,DEN   ,SIGS0)
     3                  SIGS  ,SIGA  ,VCAP  ,IDRREG,IUSE  ,DEN   ,SIGS0,
     4                  NMAT  ,MATD  )
C
CMOD  PARAMETER ( MXLISO = 2000  , MAXMAT = 100 )
CMOD  PARAMETER ( MXNISO = 110 , MAXNG = 107  , MAXMT3 = 6 )
CDEL  PARAMETER ( MXDBHT = 450 )
      INCLUDE  'MATDTINC'
      INCLUDE  'BMICRINC'
      INCLUDE  'DBHETINC'
CEND
      CHARACTER*4     TIL,ID,NFILE,MTNAME
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK3/ A(14500),LOCAM(8)
CMOD  COMMON /PCOWK4/ NUCPOS(MXLISO),KUCPOS(MXLISO)
      CHARACTER*4     IDUM1
      COMMON  /PCOWK4/ IDUM1(2,MXLISO),IDUM2(MXLISO),KUCPOS(MXLISO),
     +                 NUCPOS(MXLISO)
CEND
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB,KCOREK(MAXMAT)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
CKSK  COMMON /DOUBLE/ LDOUBL(50),ISWRES(450)
      COMMON /DOUBLE/ LDOUBL(50),ISWRES(MXDBHT)
C
      COMMON   /MICCOM/ EFFMIC(MAXNG,MAXMT3,MXNISO),LENEFF
C
C@DEL DIMENSION       IDUM2(MXLISO)
C@MOD CHARACTER*4     IDUM1(2,MXLISO),NAMEP(2)
      CHARACTER*4     NAMEP(2)
C
C@DEL EQUIVALENCE    (IDUM1(1,1),A(1))
C@DEL EQUIVALENCE    (IDUM2(1),A(2001))
C
      DIMENSION
     1  NCOR(KCOMP),ISWF(KCOMP),MAR(KSREG),MTNAME(2,KCOMP),
     2  BL(KCOMP),TEMP(KCOMP),NISO(KCOMP),IREST(KMAT),AMU(KMAT),
     3  IRES(KNMAX,KCOMP),DN(KNMAX,KCOMP),MCODE(KNMAX,KCOMP),
     5  SIGF(KMAT,NG),SIGNU(KMAT,NG),SIGS(KMAT,NG),SIGS0(KMAT),
     6  SIGA(KMAT,NG),VCAP(KMAT),VOLR(KSREG),VOLM(KCOMP)
C
      CHARACTER*4  IDENT(2,KNMAX,KCOMP),NCODEL(2,KMAT)
      DIMENSION    IDRREG(KSREG),IUSE(KCOMP),DEN(KRES,KCOMP)
C@ADD
      DIMENSION    MATD(NMAT)
C
C ----- RESONANT NUCLIDE STORE
C
      ISET=0
      DO 100 M=1,KMAT
      IF(IDUM2(M).NE.2)  GO TO 100
      ISET=ISET+1
      NCODEL(1,ISET)=IDUM1(1,M)
      NCODEL(2,ISET)=IDUM1(2,M)
      IREST(ISET)   = 2
      KUCPOS(ISET)  = NUCPOS(M)
  100 CONTINUE
C ----- 1./V  NUCLIDE STORE
      DO 110 M=1,KMAT
      IF(IDUM2(M).NE.4)  GO TO 110
      ISET=ISET+1
      NCODEL(1,ISET)=IDUM1(1,M)
      NCODEL(2,ISET)=IDUM1(2,M)
      IREST(ISET)   = 4
      KUCPOS(ISET)  = NUCPOS(M)
  110 CONTINUE
C ----- STEP  NUCLIDE STORE
      DO 120 M=1,KMAT
      IF(IDUM2(M).NE.3)  GO TO 120
      ISET=ISET+1
      NCODEL(1,ISET)=IDUM1(1,M)
      NCODEL(2,ISET)=IDUM1(2,M)
      IREST(ISET)   = 3
      KUCPOS(ISET)  = NUCPOS(M)
  120 CONTINUE
C ----- OTHER NUCLIDE STORE
      DO 130 M=1,KMAT
      IF(IDUM2(M).NE.0)  GO TO 130
      ISET=ISET+1
      NCODEL(1,ISET)=IDUM1(1,M)
      NCODEL(2,ISET)=IDUM1(2,M)
      IREST(ISET)   = 0
      KUCPOS(ISET)  = NUCPOS(M)
  130 CONTINUE
C ----- AMU,X-SECTION SET
      IFLSW=1
CKSK  NFILE(1)=4HFAST
      NFILE(1)='FAST'
CKSK  NFILE(2)=4HU
      NFILE(2)='U   '
C
      CALL  CLEA(AMU   ,KMAT    ,0.0)
      CALL  CLEA(VCAP  ,KMAT    ,0.0)
      CALL  CLEA(SIGF  ,KMAT*NG ,0.0)
      CALL  CLEA(SIGNU ,KMAT*NG ,0.0)
      CALL  CLEA(SIGS  ,KMAT*NG ,0.0)
      CALL  CLEA(SIGA  ,KMAT*NG ,0.0)
      CALL  CLEA(SIGS0 ,KMAT    ,0.0)
C
      DO 200 K = 1,KMAT
      NAMEP(1) = NCODEL(1,K)
CKSK  NAMEP(2) = 4H0000
      NAMEP(2) = '0000'
      CALL  PCOCON(AMASS,SIGC0,ICAPT,IFISS,NEF,IIRES,LTOT,NAMEP)
      AMU(K)   = AMASS
      VCAP(K)  = SIGC0
      NAMEP(1) (1:1) = 'M'
      LENG     = LOCAM(8)-1
      CALL READ(NAMEP(1),A,LENG)
      ISTC     = LOCAM(1)+ISTART-1
      ISTS     = LOCAM(7)+ISTART-1
      ISTF     = LOCAM(2)+ISTART-1
      ISTN     = LOCAM(3)+ISTART-1
      SIGS0(K) = A(ISTS-1)
C
      DO 210 I=1,NG
      IF(ICAPT.EQ.1) SIGA(K,I) =A(ISTC)
      IF(IFISS.EQ.1) SIGF(K,I) =A(ISTF)
      IF(IFISS.EQ.1) SIGNU(K,I)=A(ISTN)
                     SIGS(K,I) =A(ISTS)
      ISTS=ISTS+1
      ISTC=ISTC+1
      ISTF=ISTF+1
      ISTN=ISTN+1
  210 CONTINUE
  200 CONTINUE
C ---- GET EFFECTIVE MICROSCOPIC X-SECTION IF EXIST
      IFLSW   = 1
CKSK  NFILE(1)=4HMICR
      NFILE(1)='MICR'
CKSK  NFILE(2)=4HEF
      NFILE(2)='EF  '
      IST     = ISTART-1
C
      DO 240 NN = 1 , KCOMP
      NAMEP(1) = MTNAME(1,NN)
      NAMEP(2) = 'BMIC'
      LENG     = MAXNG*MAXMT3*NISO(NN)
C@ADD
      KANS     = 0
      CALL SEARCH ( NAMEP , LENG0 , KANS )
      IF(KANS.EQ.1) GO TO 240
C@END
      CALL  READ ( NAMEP , EFFMIC , LENG )
C
      DO 230 K=KRES+1,KMAT
      NAMEP(1)=NCODEL(1,K)
      NAMEP(2)=NCODEL(2,K)
      IF(NAMEP(2).EQ.'0000') GO TO 230
      MPOS    = KUCPOS(K)/10000
      IF(MPOS.NE.NN)         GO TO 230
      KPOS    = KUCPOS(K) - MPOS*10000
      IF(KPOS.LE. 0)         GO TO 230
CDEL  IF(DN(KPOS,MPOS).LE.0.0)  GO TO 230
      SIGS0(K)= EFFMIC(IST,4,KPOS)
C
      DO 220 I=1,NG
      SIGA(K,I) = EFFMIC(I+IST,1,KPOS)
      SIGS(K,I) = EFFMIC(I+IST,4,KPOS)
      IF(SIGF(K,I).GT.0.0) THEN
CM                SAVENU =  SIGNU(K,I) / SIGF(K,I)
                  SIGF(K,I) = EFFMIC(I+IST,2,KPOS)
CM                SIGNU(K,I)= EFFMIC(I+IST,2,KPOS)*SAVENU
                  SIGNU(K,I)= EFFMIC(I+IST,6,KPOS)/SIGF(K,I)
                  ELSE
                  SIGF(K,I) = 0.0
                  SIGNU(K,I)= 0.0
                  ENDIF
  220 CONTINUE
  230 CONTINUE
  240 CONTINUE
C
C-----SET NBH
C
  301 CONTINUE
      SAVE=100000.
      NBH=0
      IF(KRES.LE.0)  GO TO 311
      DO 310 K=1,KRES
      IF(AMU(K).GT.SAVE) GO TO 310
      SAVE=AMU(K)
      NBH =K
  310 CONTINUE
C
C-----SET NBB
C
  311 CONTINUE
      SAVE=100000.
      NBB=0
      IF(KMAT-KRES.LE.0)  GO TO 321
      DO 320 K=KRES+1,KMAT
      IF(AMU(K).GT.SAVE.OR.AMU(K).LE.1.1)  GO TO 320
      SAVE=AMU(K)
      NBB =K
  320 CONTINUE
C
  321 CONTINUE
      CALL ICLEA(MCODE,KNMAX*KCOMP,0)
      CALL ICLEA(NCOR ,KCOMP      ,0)
      CALL ICLEA(ISWF ,KCOMP      ,0)
      CALL  CLEA(DEN  ,KRES*KCOMP ,0.0)
      CALL ICLEA(MAIN ,2          ,0)
C
      DO 350 K=1,KCOMP
      MMK=NISO(K)
      IF(MMK.LE.0)  GO TO 350
C
              DO 340 M=1,MMK
              NAMEP(1)=IDENT(1,M,K)
              NAMEP(2)=IDENT(2,M,K)
              IRESS   =IRES(M,K)
              IF(IUSE(K).EQ.1)             GO TO 329
C
              IF(IRESS  .EQ.2) ISWF(K)=3
              IF(ISWF(K).EQ.3)             GO TO 329
              IF(IRESS  .EQ.3) ISWF(K)=1
              IF(ISWF(K).EQ.1)             GO TO 329
              IF(IRESS  .EQ.4) ISWF(K)=2
C
  329                    CONTINUE
                         DO 330 J=1,KMAT
                         IF(NAMEP(1).NE.NCODEL(1,J)) GO TO 330
                         IF(NAMEP(2).NE.NCODEL(2,J)) GO TO 330
                         IF(IRESS.NE.IREST(J))       GO TO 330
                         MCODE(M,K)=J
CM                       IF(J.LE.KRES) DEN(J,K)=DN(M,K)
  330                    CONTINUE
  340         CONTINUE
  350 CONTINUE
C-----CHECK DUPLICATED NUCLIDE------ADDED BY JAIS K.KANEKO 6/18/1985----
      DO 1350 K=1,KCOMP
      MMK=NISO(K)
      IF(MMK.LE.1)   GO TO 1350
      DO 1300 M=1,MMK-1
      MOLD = MCODE(M,K)
      IF(MOLD.LE.0)  GO TO 1300
             DO 1250 J = M+1,MMK
             MNEW = MCODE(J,K)
             IF(MNEW.NE.MOLD) GO TO 1250
             DN(M,K) = DN(M,K)+DN(J,K)
             MCODE(J,K) = 0
             DN(J,K)    = 0.0
 1250 CONTINUE
 1300 CONTINUE
 1350 CONTINUE
C-----DEFINE DEN ARRAY--------------ADDED BY JAIS K.KANEKO 6/18/1985----
      DO 2350 K=1,KCOMP
      MMK=NISO(K)
      IF(MMK.LE.0)  GO TO 2350
      DO 2340 M=1,MMK
      J  =MCODE(M,K)
      IF(J.LE.0)    GO TO 2340
      IF(J.LE.KRES) DEN(J,K)=DN(M,K)
 2340 CONTINUE
 2350 CONTINUE
C-----------------------------------------------------------------------
      NVCAP = 0
      NSTEP = 0
      KCOMPF= 0
            DO 360 K=1,KCOMP
            IF(ISWF(K).EQ.2)  NVCAP=NVCAP+1
            IF(ISWF(K).EQ.1)  NSTEP=NSTEP+1
            IF(ISWF(K).LE.2)  GO TO 360
            KCOMPF  = KCOMPF + 1
            NCOR(K) = KCOMPF
  360       CONTINUE
C
      KEEP  = KCOMPF
      KCOMPF= 0
      IF(IPATH.EQ.1)   GO TO 800
C ***********************************************************
C *   AUTOMATICALLY SETTING OF KCOMPF,NCOR,MAIN PARAMETER   *
C ***********************************************************
      DO 500 K=1,KEEP
      DO 370 J=1,KCOMP
      JJ=J
      IF(NCOR(J).EQ.K)  GO TO 371
  370 CONTINUE
      GO TO 500
C
  371 KCOMPF=KCOMPF+1
      IF(KCOMPF.GT.2)      GO TO 1000
      IF(JJ.EQ.KCOMP)      GO TO 450
C
             CALL  CLEA(A,KRES,0.0)
             DO 400 J=JJ+1,KCOMP
             IF(NCOR(J).EQ.0)  GO TO  400
             DO 380 I=1,KRES
             IF(DEN(I,JJ).EQ.0.0.AND.DEN(I,J).EQ.0.0) GO TO 380
             IF(DEN(I,JJ).EQ.0.0)  GO TO 400
             IF(DEN(I,J).EQ.0.0)   GO TO 400
             A(I)=DEN(I,JJ)/DEN(I,J)
             RATIO=A(I)
  380        CONTINUE
                   DO 390 I=1,KRES
                   IF(A(I).EQ.0.0)               GO TO 390
                   IF(ABS(A(I)-RATIO).GT.0.001)  GO TO 400
  390              CONTINUE
             NCOR(J)=KCOMPF
  400        CONTINUE
C
  450 SAVE=0.0
      DO 460 I=1,KRES
      IF(DEN(I,JJ).LT.SAVE)         GO TO 460
      MAIN(KCOMPF)=I
      SAVE=DEN(I,JJ)
  460 CONTINUE
      NCOR(JJ)=KCOMPF
  500 CONTINUE
      GO TO 501
C **************************************************************
C *   KCOMPF,NCOR,MAIN PARAMETER ARE SETTED BY INPUT NCOR DATA *
C *              ( IPATH = 1 CASE )                            *
C **************************************************************
  800 CONTINUE
C
CDELETED BY K.KANEKO FOR SPECIAL RESONANCE MATERIAL TREATMENT
CDEL  IF(IPCNT.LE.1) THEN
CDEL                 WRITE(NOUT1,809)
CDEL                 CALL REAI( KCOREK , NMAT , ' NC ','OR  '  )
CDEL                 DO 803  I = 1 , KCOMP
CDEL                 MPOS      = MATD(I)
CDEL                 IF(KCOREK(MPOS).GT.0)  NCOR(I) = KCOREK(MPOS)
CM803                CONTINUE
CDEL                 CALL ICLEA( KCOREK , NMAT , 0 )
CDEL                 DO 804  I = 1 , KCOMP
CDEL                 KCOREK(I) = NCOR(I)
CM804                CONTINUE
CDEL 
CDEL                 ELSE
                     DO 805  I = 1 , KCOMP
                     NCOR(I)   = KCOREK(I)
  805                CONTINUE
CDEL                 ENDIF
C
CM809 FORMAT(1H ,' ENTER NCOR IN FREE FORMAT. ( AT SUBR. PCOIN2) ')
C
      DO 810  K=1,KCOMP
      IF(NCOR(K).GT.0.AND.ISWF(K).NE.3)  NCOR(K)=0
      IF(NCOR(K).LT.0.OR .NCOR(K).GT.2)  GO TO 3000
  810 CONTINUE
C----------- REPRESENTIVE MATERIAL SET
             K1 = 0
             K2 = 0
             SAVE = 0.0
             DO 820 K=1,KCOMP
             IF(NCOR(K).NE.1)    GO TO 820
             IF(VOLM(K).LT.SAVE) GO TO 820
             K1 = K
             SAVE = VOLM(K)
  820        CONTINUE
C
             SAVE = 0.0
             DO 830 K=1,KCOMP
             IF(NCOR(K).NE.2)    GO TO 830
             IF(VOLM(K).LT.SAVE) GO TO 830
             K2 = K
             SAVE = VOLM(K)
  830        CONTINUE
C
      IF(K1.EQ.0.AND.K2.EQ.0)    GO TO 3000
      IF(K1.GT.0)                GO TO 841
C
             K1 = K2
             K2 = 0
             DO 840 K=1,KCOMP
             IF(NCOR(K).EQ.2)  NCOR(K) = 1
  840        CONTINUE
C
  841 KCOMPF = 2
      IF(K2.EQ.0) KCOMPF = 1
C
      DO 860 K=1,KCOMPF
      ISW = K1
      IF(K.EQ.2)  ISW = K2
      SAVE = 0.0
                   DO 850 J = 1 , KRES
                   IF(DEN(J,ISW).LT.SAVE) GO TO 850
                   MAIN(K) = J
                   SAVE    = DEN(J,ISW)
  850              CONTINUE
  860 CONTINUE
C **************************************************************
C *   DEFINITION OF THE KIND OF PROBLEM TO BE SOLVED.(NPROB)   *
C **************************************************************
  501 CONTINUE
      NPROB =KCOMPF
      IF(NSTEP.GT.0) NPROB=NPROB+2
      IF(NVCAP.LE.0) GO TO 600
C
      IF(KCOMPF+NVCAP.GT.2)  GO TO 509
      NPROB=5
      DO 504 K=1,KCOMP
      IF(ISWF(K).NE.2) GO TO 504
      KCOMPF=KCOMPF+1
      NCOR(K)=KCOMPF
      MMK    =NISO(K)
C
                SAVE = 0.0
                DO 503 M=1,MMK
                J = MCODE(M,K)
                IF(J.LE.0)           GO TO 503
                IF(IRES(M,K).NE.4)   GO TO 503
                IF(DN(M,K).LT.SAVE)  GO TO 503
                MAIN(KCOMPF) = J
                SAVE = DN(M,K)
  503           CONTINUE
  504 CONTINUE
      GO TO 600
C
  509 CONTINUE
      DO 510 J=KRES+1,KMAT
  510 IF(IREST(J).EQ.4) IREST(J)=3
      DO 530 K=1,KCOMP
      IF(ISWF(K).NE.2)   GO TO 530
      MMK    =NISO(K)
      IF(MMK.LE.0)       GO TO 530
      ISWF(K)=1
      DO 520 M=1,MMK
      IF(IRES(M,K).NE.4) GO TO 520
      IRES(M,K)=3
  520 CONTINUE
  530 CONTINUE
      IF(NPROB.LE.2) NPROB=NPROB+2
C
C
  600 CONTINUE
      IF(IPLOT.EQ.0) RETURN
C ----- PRINT OUT
      WRITE(NOUT2,670) TIL,ID
  670 FORMAT(1H1/1H ,5X,'*** INPUT DATA LIST FOR PEACO CODE ***',
     1       18A4,4X,2A4)
      WRITE(NOUT2,671) KSREG,KMAT,KCOMP,KCOMPF,KRES,NPROB,NDOUBL,IPLOT
C
  671 FORMAT(1H0,11X,
     1'KSREG : NO. OF R-REGION -------------------------- ',I5/1H ,11X,
     2'KMAT  : NO. OF NUCLIDES -------------------------- ',I5/1H ,11X,
     3'KCOMP : NO. OF MATERIALS ------------------------- ',I5/1H ,11X,
     4'KCOMPF: NO. OF FUEL MATERIALS -------------------- ',I5/1H ,11X,
     5'KRES  : NO. OF RESONANT NUCLIDE ------------------ ',I5/1H ,11X,
     6'NPROB : PROBLEM  OPTION -------------------------- ',I5/1H ,11X,
     7'NDOUBL: HETERO   OPTION -------------------------- ',I5/1H ,11X,
     8'IPLOT : PLOT OPTION (0/1/2:NO/YES/YES) ----------- ',I5)
C
      WRITE(NOUT2,672) (NCOR(I),I=1,KCOMP)
  672 FORMAT(1H0,10X,' RESONANCE-MATERIAL IDENTIFICATION  '//,
     + (1H ,10X,(10I6)/))
      WRITE(NOUT2,673) (MAIN(I),I=1,KCOMPF)
  673 FORMAT(1H0,10X,' THE MAIN RESONANCE NUCLIDE IN FUEL MATERIALS',
     +      /1H0,10X,2I5)
      WRITE(NOUT2,680) (I,VOLR(I),I=1,KSREG)
  680 FORMAT(1H0,10X,' THE VOLUME OF EACH R-REGION'//,
     +  (1H ,10X,5(I3,2X,E12.5,3X)/))
      WRITE(NOUT2,681) (I,VOLM(I),I=1,KCOMP)
  681 FORMAT(1H0,10X,' THE VOLUME OF EACH M-REGION'//,
     +  (1H ,10X,5(I3,2X,E12.5,3X)/))
C
      WRITE(NOUT2,675) ISTART
      DO 550 K=1,KMAT
      WRITE(NOUT2,676) K,(NCODEL(I,K),I=1,2),AMU(K),SIGF(K,1),SIGNU(K,1)
     +               ,SIGS(K,1),SIGA(K,1),VCAP(K),IREST(K)
CM    DO 550 N=2,NG
CM    WRITE(NOUT2,674) N,SIGF(K,N),SIGNU(K,N),SIGS(K,N),SIGA(K,N)
  550 CONTINUE
      WRITE(NOUT2,677)
C
  675 FORMAT(//1H ,20X,'<< CROSS SECTION OF EACH NUCLIDE AT ',I2,
     + '-TH BROAD ENERGY GROUP >>'//1H ,10X,'NO. ',
     +'NUCLIDE',5X,'MASS(AMU)',3X,'SIG.F',7X,'NU      ',4X,'SIG.S',
     +7X,'SIG.C',7X,'SIG.VCAP',3X,'IRES'/1H ,5X,17(6H------))
  676 FORMAT(1H ,10X,I3,1X,2A4,3X,1P6E12.5,4X,I2)
  677 FORMAT(1H ,5X,17(6H------))
  674 FORMAT(1H ,30X,3HNG=,I2,2H: ,1P6E12.5)
C
      WRITE(NOUT2,701) (ISWF(I),I=1,KCOMP)
      WRITE(NOUT2,702) (MAR(I),I=1,KSREG)
C
  701 FORMAT(1H0,10X,'MATERIAL IDENTIFICATION ----- (0/1/2/3;NON-RESO/ST
     +EP/VCAP/RESO)'//,(1H ,10X,10I6/))
  702 FORMAT(1H0,10X,'R-REGION IDENTIFICATION ----- (N;MATERIAL NO.)'//,
     +(1H ,10X,10I6/))
C
      DO 710 K=1,KCOMP
      MMK=NISO(K)
      WRITE(NOUT2,711) MTNAME(1,K),MTNAME(2,K),MMK,TEMP(K),BL(K)
      IF(MMK.LE.0)  GO TO 710
      WRITE(NOUT2,712) ((IDENT(I,M,K),I=1,2),M=1,MMK)
      WRITE(NOUT2,713)  (DN(M,K),M=1,MMK)
      WRITE(NOUT2,714)  (IRES(M,K),M=1,MMK)
      WRITE(NOUT2,715)  (MCODE(M,K),M=1,MMK)
      IF(ISWF(K).GT.1.AND.BL(K).EQ.0.0)  GO TO 2000
  710 CONTINUE
C
  711 FORMAT(//1H ,30X,'MATERIAL NAME = ',2A4,
     1     //1H ,10X,'NUMBER OF NUCLIDE --------------- ',I6,
     2      /1H ,10X,'TEMPERATURE (K) ----------------- ',F8.2,
     3      /1H ,10X,'MEAN CHORD LENGTH --------------- ',1PE12.5)
  712 FORMAT(1H ,10X,'IDENTIFICATION OF NUCLIODE ------ ',5(2X,2A4,2X),
     +    /,(1H ,44X,5(2X,2A4,2X)))
  713 FORMAT(1H ,10X,'NUMBER DENSITY ------------------ ',1P5E12.5,
     +    /,(1H ,44X,1P5E12.5))
  714 FORMAT(1H ,10X,'RESONANT INDICATOR -------------- ',5(I6,6X),
     +    /,(1H ,44X,5(I6,6X)))
  715 FORMAT(1H ,10X,'MCODE --------------------------- ',5(I6,6X),
     +    /,(1H ,44X,5(I6,6X)))
C
      IF(IPATH.NE.2)  RETURN
C
      WRITE(NOUT2,721) LCOMP,LSREG,IPATH,MICFL,
     +                 MICMOD,MTREPL,IGEOM,METHOD,
     +                 GAMMA,RF,RM,XLL
  721 FORMAT(1H0,10X
     1 ,' ## INFORMATION OF DOUBLE HETEROGENEITY PROBLEM ## ',
     2     //1H ,10X,'LCOMP --------------------------- ',I6,
     3      /1H ,10X,'LSREG --------------------------- ',I6,
     4      /1H ,10X,'IPATH --------------------------- ',I6,
     5      /1H ,10X,'MICFL --------------------------- ',I6,
     6      /1H ,10X,'MICMOD--------------------------- ',I6,
     7      /1H ,10X,'MTREPL--------------------------- ',I6,
     8      /1H ,10X,'IGEOM --------------------------- ',I6,
     9      /1H ,10X,'METHOD--------------------------- ',I6,
     A      /1H ,10X,'GAMMA --------------------------- ',E12.5,
     B      /1H ,10X,'RF ------------------------------ ',E12.5,
     C      /1H ,10X,'RM ------------------------------ ',E12.5,
     D      /1H ,10X,'XLL ----------------------------- ',E12.5)
      WRITE(NOUT2,722) (IUSE(I),I=1,KCOMP)
  722 FORMAT(1H0,10X,' DOUBLE HETERO. MATERIAL IDENTIFICATION '
     +,' (0/1/2/3:NONE/MACRO-FUEL/MICRO-MODERATOR/MICRO-FUEL)'//,
     + (1H ,10X,(20I6)/))
      WRITE(NOUT2,723) (ISWRES(I),I=1,LSREG)
  723 FORMAT(1H0,10X,' R-REGION IDENTIFICATION IN MACROSCOPIC '
     +,'GEOMETRY  (0/1:NON-RESONANT/RESONANT)'//,
     + (1H ,10X,(20I6)/))
      WRITE(NOUT2,724) (IDRREG(I),I=1,KSREG)
  724 FORMAT(1H0,10X,' R-REGION CROSS REFERRENCE BETWEEN MACRO. '
     +,'AND MIXED GEOMETRY (IDRREG)',/1H ,10X,
     +'(IDRREG(I):R-REGION NO IN MACRO. GEOMETRY FOR I-TH MIXED ',
     +'R-REGION (MINUS NEANS EXPANDED R-REGION) )'//,
     + (1H ,10X,(20I6)/))
C
      RETURN
C
 1000 CONTINUE
      WRITE(NOUT1,799)
  799 FORMAT(//1H ,10X,'EEROR STOP AT SUBR.(PCOIN2)',
     +       /1H ,10X,'NO. OF FUEL COMPSITION OVER (MAXIMUM=2)'/)
      STOP
C
 2000 CONTINUE
      WRITE(NOUT1,1799)
 1799 FORMAT(//1H ,10X,'EEROR STOP AT SUBR.(PCOIN2)',
     +       /1H ,10X,'MEAN CHORD LENGTH OF FUEL MATERIAL IS ZERO. ]]')
      STOP
C
 3000 CONTINUE
      WRITE(NOUT1,2799)
 2799 FORMAT(//1H ,10X,'EEROR STOP AT SUBR.(PCOIN2)',
     +       /1H ,10X,'NCOR(INPUT DATA) IS INCONSISTENT ]]] ')
      STOP
      END

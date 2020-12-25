      SUBROUTINE  PCOIN1(VOLM  ,VOLR  ,MMR   ,MTNAME,XL    ,TEMP  ,
     +                   NISO  ,IDENT ,IRES  ,DN    ,LXMICR,IDCASE,
     +                   TIL   ,KCOMP ,KNMAX ,KSREG ,KMAT  ,NDOUBL,
     +                   NEF   ,KRES  ,NMAT  ,IDRREG,IUSE  ,VOLR0 ,
     +            MAR0  ,KCODEL,KREST ,KUCPOS,NCOR  ,MXKMAT,MATD )
C
      CHARACTER*4      NFILE
      CHARACTER*4      CHACHK
      CHARACTER*4      NCODEL,IDTMP1,IDTMP2
C
CDEL  PARAMETER ( MXLISO = 2000 , MAXMAT=100)
CDEL  PARAMETER ( MXDBHT =   450 )
      INCLUDE  'MATDTINC'
      INCLUDE  'DBHETINC'
C
      COMMON  /MAINC / NN(100),ID(2),TITLE(18),AA(380)
      COMMON  /PIJ2C/  LL(50),BB(950)
      COMMON  /TMPSET/ STND(35),IDTEMP(61),NTDUMY
C
      COMMON  /PCOWK2/ DMUUM1(9),NOUT1,NOUT2
C@MOD COMMON  /PCOWK3/ NCODEL(2,MXLISO),IREST(MXLISO),LISO(MXLISO),
C@MOD+                 MATD(500),JDUM(MXLISO),LOCAM(8),IFS
      COMMON  /PCOWK3/ JDUM(14500),LOCAM(8),IFS
      COMMON  /PCOWK4/ NCODEL(2,MXLISO),IREST(MXLISO),LISO(MXLISO),
     +                 NUCPOS(MXLISO)
CKK98
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB,KCOREK(MAXMAT)
CEND                                                              
C
      COMMON  /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                 IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
CKSK  COMMON  /DOUBLE/ LDOUBL(50),ISWRES(450)
      COMMON  /DOUBLE/ LDOUBL(50),ISWRES(MXDBHT)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
CMOD  DIMENSION  IA(380),IDPQ(3),IBB(950),DDOUBL(50),NAMEP(2)
      DIMENSION  IA(380),IBB(950),DDOUBL(50)
      CHARACTER*4  IDPQ(3),NAMEP(2)
C
      EQUIVALENCE (IA(1),AA(1)),(IBB(1),BB(1))
      EQUIVALENCE (LDOUBL(1),DDOUBL(1))
C
      DIMENSION  TIL(18),IDCASE(2),LXMICR(KNMAX,KCOMP)
      DIMENSION  MTNAME(2,KCOMP),NISO(KCOMP),TEMP(KCOMP),XL(KCOMP),
     +           IRES(KNMAX,KCOMP),VOLM(KCOMP),VOLR(KSREG),
CM   +           IDENT(2,KNMAX,KCOMP),DN(KNMAX,KCOMP),MMR(KSREG)
     +           DN(KNMAX,KCOMP),MMR(KSREG)
      CHARACTER*4 IDENT(2,KNMAX,KCOMP)
      CHARACTER*4 CA(330)
C
      DIMENSION  IDRREG(KSREG),IUSE(KCOMP),VOLR0(LSREG),MAR0(LSREG)
C@MOD DIMENSION  KCODEL(2,MXKMAT),KREST(MXKMAT),KUCPOS(MXKMAT)
      DIMENSION  KREST(MXKMAT),KUCPOS(MXKMAT)
      CHARACTER*4  KCODEL(2,MXKMAT)
C@ADD
      DIMENSION  MATD(NMAT),NCOR(NMAT)
      DIMENSION  IFUEL(MAXMAT)
      EQUIVALENCE (AA(1),CA(1))
C@END
CM    DATA  IDPQ /4H   0,4H   2,4H   F/
      DATA  IDPQ /'   0','   2','   F'/
C
C *** START OF PROCESS
C
      CALL ICLEA( IDRREG ,KSREG, 0 )
      CALL ICLEA( IUSE   ,KCOMP, 0 )
      CALL ICLEA( MAR0   ,LSREG, 0 )
      CALL  CLEA( VOLR0  ,LSREG,0.0)
C@MOD CALL ICLEA( NUCPOS ,1000 , 0 )
      CALL ICLEA( NUCPOS ,MXLISO , 0 )
      CALL ICLEA( KUCPOS ,MXKMAT,0 )
      CALL ICLEA( IFUEL  ,MAXMAT,0 )
      CALL ICLEA( NCOR   ,NMAT  ,0 )
C
      NDOUBL=0
      MTREPL=0
      MICFL =0
      MICMOD=0
C
      IFLSW=1
      NFILE(1)= 'FAST'
      NFILE(2)= 'U  '
C
      DO 1 I=1,18
    1 TIL(I)=TITLE(I)
      IDCASE(1)=ID(1)
      IDCASE(2)=ID(2)
C
      LCMMR=LL(40)
      DO 24 I=1,LSREG
      MAR0(I)=IBB(LCMMR)
      LCMMR=LCMMR+1
   24 CONTINUE
C@DELETE
C     LCMATD=LL(50)
C     DO 10 K=1,LCOMP
C     MATD(K)=IBB(LCMATD)
C  10 LCMATD=LCMATD+1
C@END
      IF(IPATH.NE.2) GO TO 8
                 DO 6 J = 1 , LSREG
                 IF(ISWRES(J).EQ.1)  KK = MAR0(J)
    6            CONTINUE
                 MTREPL = KK
                 IUSE(MTREPL)  = 1
                 ISWPOS        = MATD(KK)
                 MATD(LCOMP+1) = ISWPOS - 2
                 MATD(LCOMP+2) = ISWPOS - 1
C
    8 CONTINUE
C@MOD
CM    WRITE(6,9) (MATD(I),I=1,KCOMP)
CM  9 FORMAT(1H ,' ### MATD ### ',10I6)
C
      LCMTNM=NN(85)
      DO 11 J=1,KCOMP
      ISW   =LCMTNM + (MATD(J)-1)*2 -1
      DO 11 I=1,2
      ISW   =ISW + 1
      IAIA  =IA(ISW)
      IF(I.EQ.2) CALL PACK(IAIA,1,IDPQ(3))
      IF(I.EQ.2) CALL PACK(IAIA,4,IDPQ(2))
      MTNAME(I,J)=IAIA
   11 CONTINUE
C
      LCNISO=NN(86)
      DO 12 J=1,KCOMP
      ISW   =LCNISO + MATD(J) - 1
      NISO(J)=IA(ISW)
      IF( NISO(J).EQ.0) THEN
                        WRITE(NOUT1,112) J
                        WRITE(NOUT2,112) J
                        STOP
                        ENDIF
   12 CONTINUE
C
  112 FORMAT(' *** NULL COMPOSITION ENCOUNTERED FOR',I2,' TH MIXTURE,',
     *'GIVE COMPOSITION EVEN IF MACRO X-SECTION IS ALREADY COMPOSED')
C
      LCTEMP=NN(87)
      DO 13 J=1,KCOMP
      ISW   =LCTEMP + MATD(J) - 1
      TEMP(J)=AA(ISW)
   13 CONTINUE
C
      LCXL=NN(88)
      DO 14 J=1,KCOMP
      ISW = LCXL + MATD(J) -1
      XL(J)=AA(ISW)
   14 CONTINUE
C
      LCLISO=NN(90)
      DO 16 J=1,NMAT
      LISO(J)=IA(LCLISO)
      LCLISO=LCLISO+1
   16 CONTINUE
C
      LCIDNT=NN(91)
      DO 17 J=1,KCOMP
      IPOS=MATD(J)
      ISW=LCIDNT+(LISO(IPOS)-1)*2
      MM=NISO(J)
      IF(MM.LE.0)  GO TO 17
      DO 217 M=1,MM
      DO 216 I=1,2
C@MOD IDENT(I,M,J)=IA(ISW)
      IDENT(I,M,J)=CA(ISW)
      ISW=ISW+1
  216 CONTINUE
      CALL PACK(IDENT(1,M,J),1,IDPQ(1))
      CALL PACK(IDENT(2,M,J),1,IDPQ(1))
      IF(IDENT(1,M,J)(2:3).EQ.'TH')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'PA')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'U0')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'NP')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'PU')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'AM')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'CM')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'BK')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'CF')  IFUEL(J) = 1
      IF(IDENT(1,M,J)(2:3).EQ.'FM')  IFUEL(J) = 1
  217 CONTINUE
   17 CONTINUE
C
C------------- LXMICR SET FROM INPUT-DATA (MAINC)
C
               LCIXMC=NN(94)
               DO 5056 J=1,KCOMP
               IPOS=MATD(J)
               ISW =LCIXMC+LISO(IPOS)-1
               MM  =NISO(J)
               IF(MM.LE.0)   GO TO 5056
                    DO 5006 M=1,MM
                    LXMIC=IA(ISW)
                    LXMICR(M,J)=LXMIC
                    ISW=ISW+1
 5006               CONTINUE
 5056          CONTINUE
C
C _______ SPECIAL REASONANCE MATERIAL SPECIFICATION
C
      IF(IPATH.EQ.1) THEN
      IF(IPCNT.LE.1) THEN                                                       
                     WRITE(NOUT1,809)                                           
                     CALL REAI( KCOREK , NMAT , ' NC ','OR  '  )                
                     DO 803  I = 1 , KCOMP                                      
                     MPOS      = MATD(I)                                        
                     IF(KCOREK(MPOS).GT.0)  NCOR(I) = KCOREK(MPOS)              
  803                CONTINUE                                                   
                     CALL ICLEA( KCOREK , NMAT , 0 )                            
                     DO 804  I = 1 , KCOMP                                      
                     KCOREK(I) = NCOR(I)                                        
  804                CONTINUE      
                     ENDIF
                     ENDIF
C
  809 FORMAT(1H ,' ENTER NCOR IN FREE FORMAT. ( AT SUBR. PCOIN1) ') 
C
C-------------- IRES SET
C
               LCIRES=NN(93)
               DO 5055 J=1,KCOMP
               IPOS= MATD(J)
               ISW = LCIRES+LISO(IPOS)-1
               MM  = NISO(J)
               IF(MM.LE.0)    GO TO 5055
C
               RTEMP=TEMP(J)
               TDIFF=100000.
               ISET = 1
               DO 117 K=1,NTDUMY
               IF(ABS(STND(K)-RTEMP).GT.TDIFF) GO TO 118
               ISET = K
  117          TDIFF = ABS(STND(K)-RTEMP)
  118          IDTMP=IDTEMP(ISET)
C
               DO 5005 M=1,MM
               ISAVE   =IA(ISW)
               IF(ISAVE.EQ.1) THEN
                              IRES(M,J) = 0
                              GO TO 5004
                              ENDIF
CKK9
               IF(ISAVE.LE.-1) THEN
                               IRES(M,J) = -1
                               GO TO 5004
                              ENDIF
CEND
C
               NAMEP(1)=IDENT(1,M,J)
               NAMEP(2)= '0000'
               CALL PCOCON(DUM1,DUM2,IDUM1,IDUM2,NEF,IIRES,LTOT,NAMEP)
CKK98
      IF(IPATH.EQ.1) THEN
                     IF(KCOREK(J).LE.0) IIRES = 0
                     IF(KCOREK(J).LE.0) ISAVE = 0
                     ENDIF
CEND
               IF(IIRES.EQ.1) THEN
                              IF(ISAVE.NE.2) WRITE(NOUT1,1020)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                              IF(ISAVE.NE.2) WRITE(NOUT2,1020)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                              IRES(M,J)    = 2
CM                            IDENT(2,M,J) =  '0000'
CMOD                          CALL PACK(IDENT(2,M,J),4,IDTMP  )
                              CHACHK =  '0000'
                              CALL PACKX(CHACHK,1,IDENT(2,M,J),2)
                              CALL PACKX(CHACHK,2,IDENT(2,M,J),3)
               IF(CHACHK.EQ.'0000')
     *                        CALL PACK(IDENT(2,M,J),4,IDTMP  )
                              ENDIF
C
               IF(IIRES.EQ.0.AND.IFS.EQ.1) THEN
                              IF(ISAVE.EQ.2) THEN
                                             ISAVE = 0
                                             WRITE(NOUT1,1021)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                                             WRITE(NOUT2,1021)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                                             ENDIF
                              CALL PACKX(IDENT(2,M,J),2,MTNAME(2,J),2)
                              CALL PACKX(IDENT(2,M,J),3,MTNAME(2,J),3)
                              IRES(M,J) = ISAVE
                              CALL PACK(IDENT(2,M,J),4,IDTMP  )
                              ENDIF
C
               IF(IIRES.EQ.0.AND.IFS.EQ.0) THEN
                              IF(ISAVE.EQ.2) THEN
                                             ISAVE = 0
                                             WRITE(NOUT1,1021)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                                             WRITE(NOUT2,1021)
     *                 IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                                             ENDIF
                              IDENT(2,M,J) =  '0000'
                              IRES(M,J) = ISAVE
                              ENDIF
 5004          ISW = ISW + 1
 5005          CONTINUE
 5055          CONTINUE
C
C-------------- DENSITY SET FROM INPUT-DATA(MAINC)
C
               LCDN=NN(92)
               DO 5057 J=1,KCOMP
               IPOS=MATD(J)
               ISW =LCDN+LISO(IPOS)-1
               MM  =NISO(J)
               IF(MM.LE.0)   GO TO  5057
                    DO 5007 M=1,MM
                    DNTMP  =AA(ISW)
                    DN(M,J)=DNTMP
                    ISW=ISW+1
 5007               CONTINUE
 5057          CONTINUE
C
C----- DENSITY , IRES , LXMICR RESETTING
C
           DO 5550 J = 1 , KCOMP
           MMK = NISO(J)
           IF(MMK.LE.0)   GO TO 5550
                   SUM = 0.0
                   DO 5400 M = 1,MMK
                   DNTMP = DN(M,J)
CTFREE             IF(IRES(M,J).EQ.2.AND.LXMICR(M,J).GT.0) GO TO 5350
CKSK               IF(IRES(M,J).EQ.2.AND.LXMICR(M,J).GT.0) THEN
CKSK                     IF(DNTMP.LE.0.0) DN(M,J) = 1.0000E-30
CKSK                     IF(DNTMP.LT.1.0000E-30) DN(M,J) = 1.0000E-30
CKSK                     GO TO 5350
C                        IF(DNTMP.LT.1.0000E-30) THEN
C                          WRITE(NOUT1,1022) IRES(M,J),LXMICR(M,J),
C    &                     IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),
C    &                     MTNAME(2,J),DN(M,J)
C                          DN(M,J)     = 1.0000E-30
C                          DNTMP       = 1.0000E-30
C                          IRES(M,J)   = 0
C                          LXMICR(M,J) = 0
CKSK                       GO TO 5350
C1022 FORMAT(1H ,5X,'WARNING ---> IRES=',I2,' AND LXMICR=',I2,' OF ',
C    &       2A4,1H(,2A4,1H),' IS RESET TO 0 BECAUSE OF LOW DENSITY',
C    &       1H(,1PE12.5,1H),'=>1.0E-30')
CKSK
                   IF(IRES(M,J).EQ.2.AND.LXMICR(M,J).GT.0) THEN
                         IF(DNTMP.LT.1.0000E-30.AND.IFUEL(J).EQ.1) THEN
                           DN(M,J) = 1.0000E-30
                           GO TO 5350
                           ENDIF
                       ENDIF
CKK9803ADD CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                   IF(IFUEL(J).EQ.0.AND.IRES(M,J).EQ.2) THEN
                     IF(DNTMP.LT.1.0000E-12) THEN
                       IRES(M,J)   = 0
                       LXMICR(M,J) = 0
                       DN(M,J)     = 0.0
                       DNTMP       = 0.0
                     ENDIF
                   ENDIF
CKK9803END CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CKK9712ADD CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                   IF(IRES(M,J).EQ.2.AND.LXMICR(M,J).EQ.0) THEN
                     IF(DNTMP.LT.1.0000E-12.AND.IFUEL(J).EQ.0) THEN
                   WRITE(NOUT1,1018)
     *             IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
                   IRES(M,J)   = 0
                   LXMICR(M,J) = 0
                   DN(M,J)     = 0.0
                   DNTMP       = 0.0
                   ENDIF
                   ENDIF
CKK9712END CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CKK9712CELCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                  IF(IRES(M,J).LE.-1)  GO TO 5350
C
CM                 IF(DNTMP.GT.1.0E -6) GO TO 5350
C                  IF(DNTMP.GT.1.0E-12) GO TO 5350
C
C                  WRITE(NOUT1,1018)
C    *             IDENT(1,M,J),IDENT(2,M,J),MTNAME(1,J),MTNAME(2,J)
C                  IRES(M,J)   = 0
C                  LXMICR(M,J) = 0
C                  DN(M,J)     = 0.0
C                  DNTMP       = 0.0
CEND9712 CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
 5350              CONTINUE
                   SUM = SUM + DNTMP
 5400              CONTINUE
                   IF(SUM.GT.0.0)  GO TO 5550
                                IPOS=MATD(J)
                                ISW =LCDN+LISO(IPOS)-1
                                DO 5500 M=1,MMK
                                DNTMP  =AA(ISW)
                                DN(M,J)=DNTMP
                                ISW=ISW+1
 5500                           CONTINUE
 5550      CONTINUE
C
 1017 FORMAT(1H ,' *** INPUT TEMPERATURE OF ',2A4,' IS RESET (',F8.2,
     *'K) BECAUSE OF MCROSS-LIBRARY DEFINITION. (AT PEACO) ***')
 1018 FORMAT(1H ,5X,'CAUTION ---> NUMBER DENSITY OF ',2A4,1H(,2A4,1H),
     +' IS ZERO SET BECUASE OF IT''S LOW DENSITY(LESS THAN 1.0E-12)]]]')
 1019 FORMAT(1H ,5X,'CAUTION ---> NUMBER DENSITY OF ',2A4,1H(,2A4,1H),
     +' IS SET TO 1.0E-11 BECAUSE OF EFFECTIVE MICRO. X-SECTION CAL.')
 1020 FORMAT(1H ,5X,'WARNING ---> IRES OF ',2A4,1H(,2A4,1H),
     +' IS RESET TO 2 BECAUSE THIS NUCLIDE HAS MCROSS LIBRARY ]] ')
 1021 FORMAT(1H ,5X,'WARNING ---> IRES OF ',2A4,1H(,2A4,1H),
     +' IS RESET TO 0 BECAUSE THIS NUCLIDE HAS NO MCROSS LIBRARY ]] ')
C
C
C
      DO 120 J=1,KCOMP
      MM=NISO(J)
      IF(MM.LE.0)   GO TO 120
      ISWDBL = 0
      DO 20 M=1,MM
      LXMIC=LXMICR(M,J)
      IF(LXMIC.GE.2.AND.IRES(M,J).EQ.2)  NDOUBL=1
      IF(IRES(M,J).EQ.2)  ISWDBL = 1
   20 CONTINUE
                IF(IPATH.NE.2) GO TO 120
                IF(J.LE.LCOMP) GO TO 120
                IF(ISWDBL.EQ.0)  MICMOD  = J
                IF(ISWDBL.EQ.1)  MICFL   = J
  120 CONTINUE
C
      LCVOLR  = LL(47)
      DO 21 I=1,LSREG
      VOLR0(I)= BB(LCVOLR)
      LCVOLR  = LCVOLR+1
      VOLR(I)   = VOLR0(I)
      IDRREG(I) = I
      MMR(I)    = MAR0(I)
   21 CONTINUE
C
      LCVOLM = LL(49)
      DO 22 I=1,LCOMP
      VOLM(I)= BB(LCVOLM)
      LCVOLM = LCVOLM+1
   22 CONTINUE
C
       IF(IPATH.NE.2)  GO TO 19
C
C ----- MICROSCOPIC GEOMETRY DATA SET
C
       IGEOM =  LDOUBL(02)
       METHOD=  LDOUBL(03)
       RF    =  DDOUBL(08)
       RM    =  DDOUBL(09)
       XLL   =  DDOUBL(10)
       VF    =  DDOUBL(11)
       VM    =  DDOUBL(12)
       VCELL =  DDOUBL(13)
       RHO   =  DDOUBL(14)
C      GAMMA =  DDOUBL(15)
                LCXDC = NN(89)
                ISW   = LCXDC + MATD(MICFL) - 1
                DANCOF= AA(ISW)
                GAMMA = 1.0 - DANCOF
                XL(MICFL) = XLL
C
CM     WRITE(6,2) VF,VM,VCELL,RHO,DANCOF,GAMMA
C
CM  2 FORMAT(1H0,
CM   *          '  VF    = ',E12.4,
CM   *           ' VM    = ',E12.4,
CM   *           ' VCELL = ',E12.4,
CM   *           ' RHO   = ',E12.4,
CM   *           ' DANCOF= ',E12.4,
CM   *           ' GAMMA = ',E12.4)
C
      IUSE(MICFL)  = 3
      IUSE(MICMOD) = 2
      VOLM(MICFL ) =  VF  / VCELL  * VOLM(MTREPL)
      VOLM(MICMOD) =  VM  / VCELL  * VOLM(MTREPL)
C
      ISWDBL = 0
      DO 28 L = 1 , LSREG
      IF(ISWRES(L).EQ.1)  GO TO 27
      ISWDBL         = ISWDBL + 1
      VOLR(ISWDBL)   = VOLR0(L)
      IDRREG(ISWDBL) = L
      MMR(ISWDBL)    = MAR0(L)
      GO TO 28
   27            CONTINUE
                 VOLR(ISWDBL+1) = VOLR0(L)*VF/VCELL
                 VOLR(ISWDBL+2) = VOLR0(L)*VM/VCELL
                 IDRREG(ISWDBL+1) = -L
                 IDRREG(ISWDBL+2) = -L
                 MMR   (ISWDBL+1) =  MICFL
                 MMR   (ISWDBL+2) =  MICMOD
                 ISWDBL = ISWDBL + 2
   28 CONTINUE
C
C     COMPOSITION DATA STORAGE END
C
   19 CONTINUE
      KMAT=0
      DO 1111 K=1,KCOMP
      MMK=NISO(K)
      IF(MMK.LE.0)  GO TO 1111
      DO 111  M=1,MMK
CKK9 ** EXCLUDE PSUEDO NUCLIDE
      IF(IRES(M,K).LE.-1) GO TO 111
CEND
      KMAT=KMAT+1
      KCODEL(1,KMAT)=IDENT(1,M,K)
      KCODEL(2,KMAT)=IDENT(2,M,K)
      KREST(KMAT)   =IRES(M,K)
      KUCPOS(KMAT)  = K*10000 +  M
  111 CONTINUE
 1111 CONTINUE
C
      DO 2222 K=1,KMAT-1
      IDTMP1=KCODEL(1,K)
      IDTMP2=KCODEL(2,K)
      IDTMP3=KREST(K)
      IF(IDTMP3.EQ.-10)        GO TO 2222
      DO  222 J=K+1,KMAT
      IF(IDTMP1.NE.KCODEL(1,J)) GO TO 222
      IF(IDTMP2.NE.KCODEL(2,J)) GO TO 222
      IF(IDTMP3.NE.KREST(J))    GO TO 222
      KREST(J)=-10
  222 CONTINUE
 2222 CONTINUE
C
      ISW  = 0
      KRES = 0
      DO 3333 K=1,KMAT
      IDTMP3=KREST(K)
      IF(IDTMP3.EQ.-10)        GO TO 3333
      ISW = ISW + 1
C@MOD IF(ISW.GT.1000) GO TO 4444
      IF(ISW.GT.MXLISO) GO TO 4444
      NCODEL(1,ISW) = KCODEL(1,K)
      NCODEL(2,ISW) = KCODEL(2,K)
      IREST (ISW)   = KREST (K)
      NUCPOS(ISW)   = KUCPOS(K)
      IF(IREST(ISW).EQ.2)  KRES = KRES + 1
 3333 CONTINUE
C
      KMAT = ISW
      RETURN
C
 4444 WRITE(NOUT1,5555) MXLISO
      WRITE(NOUT2,5555) MXLISO
      STOP
C
 5555 FORMAT(//1H ,' FATAL PROGRAM ERROR STOP AT SUB(PCOIN1) ]]]] ',
     *        /1H ,' WORK DIMENSION SHORTAGE. (NCODEL,IREST) ',
     *        /1H ,' RESERVED SIZE IS ',I6,' WORDS ]]]       '/)
C
      END

C
      SUBROUTINE FNCALC(
     *    RAMDA    ,NCH      ,NBIC     ,PBIC     ,NOL      ,KSTP     ,
     *    LONG     ,LL       ,IP       ,KP       ,NPN      ,
     *    GAMMA    ,GAMX     ,SIGA     ,SIGC     ,SIGN2N   ,PHIC     ,
     *    PHIL     ,PHAI     ,P        ,R        ,G        ,RAM      ,
     *    ANO      ,AN       ,DEX      ,DEXX     ,DCOEF    ,MAP      ,
     *    FLX      ,T        ,XM       ,
     *    NMAX     ,NPAR     ,NCHA     ,LCHA     ,MAPLEN   )
C
      REAL*8     DEX(NMAX),XX,T,DEXX(NMAX),DCOEF(LCHA)
      DIMENSION
     *    RAMDA(NMAX)        ,NCH(NMAX)          ,
     *    NBIC(NPAR,NMAX)    ,PBIC(NPAR,NMAX)    ,NOL(NMAX)          ,
     *    KSTP(NCHA,NMAX)    ,LONG(NCHA,NMAX)    ,LL(NCHA,NMAX)      ,
     *    IP(LCHA,NCHA,NMAX) ,KP(LCHA,NCHA,NMAX) ,NPN(NPAR,NMAX)     ,
     *    GAMMA(NMAX)        ,GAMX(NMAX)         ,SIGA(NMAX)         ,
     *    SIGC(NMAX)         ,SIGN2N(NMAX)       ,
     *    PHIC(NMAX)         ,PHIL(NMAX)         ,PHAI(NPAR,NMAX)    ,
     *    R(LCHA)            ,G(LCHA)            ,P(LCHA)            ,
     *    RAM(NMAX)          ,ANO(NMAX)          ,AN(NMAX)           ,
     *    MAP(MAPLEN)
C
      DATA ERR   ,EPS   /1.0E-20  ,1.0E-10   /
C
C
      IF ( MAPLEN .LE. 0 ) GO TO 1000
C
      DO 200 NN=1,NMAX
         PHIL(NN) = 0.0
         PHIC(NN) = 0.0
         DEX (NN) = 1.0
         DEXX(NN) = T
         RAM(NN)  = RAMDA(NN)  + FLX*SIGA(NN)
         IF(RAM(NN).EQ.0.0) GO TO 200
            PHIL(NN) =RAMDA(NN)/RAM(NN)
            XX       =RAM(NN)*T
                              DEX (NN) = 0.0
            IF(XX.LE.174.673) DEX (NN) = DEXP(-XX)
            IF(XX.LE.EPS    ) DEX (NN) = 1.0D0 - XX
                              DEXX(NN) = (1.0D0 - DEX(NN))/RAM(NN)
            IF(XX.LE.EPS    ) DEXX(NN) = T - XX*T/2.0D0
CM          IF(FLX.GT.ERR) PHIC(NN) = FLX*SIGC(NN)/RAM(NN)
            IF(FLX.GT.ERR) PHIC(NN) = FLX/RAM(NN)
C        END IF
  200 CONTINUE
C     WRITE(6,660) RAM
C 660       FORMAT(1H ,'RAM-->',1P10E12.5)
C     WRITE(6,670) SIGA
C 670       FORMAT(1H ,'SIGA->',1P10E12.5)
C     WRITE(6,680) SIGF
C 680       FORMAT(1H ,'SIGF->',1P10E12.5)
C     WRITE(6,690) FLX ,T
C 690       FORMAT(1H ,'FLX,T:',1P10E12.5)
C
C
C
      DO 230 NN=1,NMAX
         NCHN    =NCH(NN)
         IF(NCHN.EQ.0) GO TO 230
            IF(NCHN.GT.NPAR) NCHN=NPAR
            DO 220 M=1,NCHN
               NCL     =NPN(M,NN)
               IF(NCL.EQ.0) GO TO 220
                  NB      =NBIC(M,NN)
                     PHAI(M,NN)=PBIC(M,NN)*PHIL(NCL)
CM                IF(NB.LE.6.AND.NB.NE.3) GO TO 210
        IF(NB.EQ.3)  PHAI(M,NN)=PBIC(M,NN)*PHIC(NCL)*SIGC(NCL)
        IF(NB.EQ.8)  PHAI(M,NN)=PBIC(M,NN)*PHIC(NCL)*SIGN2N(NCL)
CM                   GO TO 220
CM210                CONTINUE
CM                   PHAI(M,NN)=PBIC(M,NN)*PHIL(NCL)
C                 END IF
C              END IF
  220       CONTINUE
C        END IF
  230 CONTINUE
C
C
C
      DO 320 MP = 1 , MAPLEN
         NN       = MAP(MP)
         AN(NN)   = 0.0
         NOS      = NOL(NN)
         DO 310 I=1,NOS
            LL2 = LONG(I,NN) - 1
            IF (LL2 .LE. 0 ) GO TO 300
               DO 260 K=1,LL2
                     IP1  = IP(K+1,I,NN)
                     KP1  = KP(K+1,I,NN)
                     P(K) = PHAI(KP1,IP1)
  260          CONTINUE
  300       CONTINUE
            P(LL2+1) = 1.0
C           LLLL=LONG(I,NN)
C           WRITE(6,600) LL(I,NN),LONG(I,NN),KSTP(I,NN)
C           WRITE(6,610) (IP(J,I,NN),J=1,LLLL)
C           WRITE(6,620) (KP(J,I,NN),J=1,LLLL)
C           WRITE(6,630) (P(J),J=1,LLLL)
C 600       FORMAT(1H0,'LL,LONG,KSTP--->',3I8)
C 610       FORMAT(1H ,'IP------------->',10I8)
C 620       FORMAT(1H ,'KP------------->',10I8)
C 630       FORMAT(1H ,'P-------------->',10F8.3)
C 640       FORMAT(1H ,'ANO------------>',5 E12.5)
C 650       FORMAT(1H ,'AN ------------>',5 E12.5)
            AN(NN)   = AN(NN)
     *               + FNBATE  (LL(I,NN)  ,LONG(I,NN)  ,
     *                          RAM       ,GAMMA       ,ANO         ,
     *                          R         ,G           ,P           ,
     *                          GAMX      ,IP(1,I,NN)  ,KSTP(I,NN)  ,
     *                          T         ,XM          ,NMAX        ,
     *                          DEX       ,DEXX        ,DCOEF       )
C
C        WRITE(6,640) ANO
C        WRITE(6,650) AN
  310    CONTINUE
  320 CONTINUE
C
C
C
 1000 CONTINUE
      RETURN
      END

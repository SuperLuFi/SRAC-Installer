      SUBROUTINE PCOQIN(P     ,QQ    ,DANX  ,DANY  ,DAN   ,X     ,
     +                  NX    ,SIG   ,NCOR  ,MAR   ,VOLR  )
C
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX,
     +                XL(20)
C
      DIMENSION    P(KSREG,KSREG),QQ(KPIJ1,MAXN,MAXP),MAR(KSREG),
     1             DANX(MAXN,KDAN),DANY(MAXN,KDAN),X(KCOMPF),DAN(MAXP),
     2             NX(KCOMPF),SIG(KCOMP),VOLR(KSREG),NCOR(KCOMP)
C
C     DATA ICOUNT / 0 /
C
C     ICOUNT = ICOUNT + 1
C     IWRT   = 0
C     IF(MOD(ICOUNT,400).EQ.1)  IWRT = 1
C
      XXX=X(1)
      YYY=XXX/(1.0-XXX)
      ZZZ=YYY/XLL
C
      IF(XXX.GT.XL(19)) GO TO 50
      IF(XXX.LT.XL(1))   XXX = XL(1)
C
      IST = 1
C
               DO 1 I = 2 , 18
               IF(XXX.LT.XL(I))  GO TO 2
               IST = I
    1          CONTINUE
C
    2 CONTINUE
      IST  = IST -1
      IF(IST.LE.0)  IST = 1
      IJ   = 0
C
      DO 10 I=1,KSREG
      DO 10 J=1,KSREG
      IJ = IJ + 1
CM    CALL  LAGS(XL(IST),QQ(IST,1,IJ),3,XXX,ANS,ILL)
      CALL  INTRPL(6,4,XL(IST),QQ(IST,1,IJ),1,XXX,ANS)
C     IF(ILL.NE.0)  GO TO 999
      P(I,J)=ANS
   10 CONTINUE
      GO TO 70
C
   50 CONTINUE
      IJ = 0
      DO 60 I=1,KSREG
      DO 60 J=1,KSREG
      IJ = IJ + 1
      P(I,J)=QQ(20,1,IJ)-DAN(IJ)/YYY
   60 CONTINUE
C
   70 CONTINUE
C
CM           DO 100 I=1,KSREG
CM           SUM = 0.0
CM           DO  80 J=1,KSREG
CM           SUM = SUM + P(I,J)
CM 80        CONTINUE
CM           IF(IWRT.EQ.1)  WRITE(6,101) ICOUNT,I,IST,SUM,XXX,YYY,ZZZ
CM           IF(IWRT.EQ.1)  WRITE(6,102) (P(I,J),J=1,KSREG)
CM                 DO 90 J=1,KSREG
CM                 P(I,J) = P(I,J)/SUM
CM 90              CONTINUE
CM                 IF(IWRT.EQ.1)  WRITE(6,103) (P(I,J),J=1,KSREG)
CM100        CONTINUE
C
      DO 6 I = 1,KSREG
      MI     = MAR(I)
      JF     = I+1
      SUM    = 0.0
      SUMP   = 0.0
      DO 9 J = 1,KSREG
      IF(J.LE.I) SUMP = SUMP + P(I,J)
    9 SUM    = SUM +P(I,J)
C
CM    IF(IWRT.EQ.1)  WRITE(6,101) ICOUNT,I,IST,SUM,XXX,YYY,ZZZ
CM    IF(IWRT.EQ.1)  WRITE(6,102) (P(I,J),J=1,KSREG)
C
               SUMA  = SUMP - P(I,I)
               IF(I.EQ.KSREG) P(I,I) = 1.-SUMA
               IF(JF.GT.KSREG) GO TO 6
                       DO 8 J = JF,KSREG
                       MJ     = MAR(J)
C                      P(I,J) = W(J)*SIG(MJ)*P(J,I)/(W(I)*SIG(MI))
                       P(I,J) =VOLR(J)*SIG(MJ)*P(J,I)/(VOLR(I)*SIG(MI))
    8                  SUMP   = SUMP + P(I,J)
               TMPB   = (1.-SUMA)/(SUMP-SUMA)
               DO 7 J = I,KSREG
               P(I,J) = P(I,J)*TMPB
               IF(I.EQ.J) GO TO 7
               P(J,I) = P(J,I)*TMPB
    7          CONTINUE
C              IF(IWRT.EQ.1)  WRITE(6,103) (P(I,J),J=1,KSREG)
    6 CONTINUE
C
      RETURN
C
  101 FORMAT(1H ,' ## ICOUNT I IST SUM Z XL SIGT ## ',3I6,1P5E12.5)
  102 FORMAT(1H ,' ## PIJ (BEFORE) ## ',1P9E12.5)
  103 FORMAT(1H ,' ## PIJ (AFTER)  ## ',1P9E12.5)
C
      END

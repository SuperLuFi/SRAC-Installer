      SUBROUTINE PCOINT(P     ,QQ    ,DANX  ,DANY  ,DAN   ,X     ,
     +                  NX    ,SIG   ,NCOR  ,MAR   ,VOLR  )
C
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
C
      DIMENSION    P(KSREG,KSREG),QQ(KPIJ1,MAXN,MAXP),MAR(KSREG),
     1             DANX(MAXN,KDAN),DANY(MAXN,KDAN),X(KCOMPF),DAN(MAXP),
     2             NX(KCOMPF),SIG(KCOMP),VOLR(KSREG),NCOR(KCOMP)
      DIMENSION    XL(11)
C
      DATA XL / 0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99999/
C
C     START OF PROCESS
C
      DO 10 K=1,KCOMPF
      DELX   = 100.0
      IPOS   = 0
      DO 11 N= 2,10
      TEMP   = ABS(XL(N)-X(K))
      IF(TEMP.LT.DELX) THEN
                       DELX = TEMP
                       IPOS = N
                       ENDIF
   11 CONTINUE
      NX(K) = IPOS
      IF(IPOS.EQ.0) STOP 900
   10 CONTINUE
C
      IF(KCOMPF.EQ.2) GO TO 1
C
C     CASE FOR KCOMPF=1
C
      NU     = NX(1)
      U      = (X(1)-XL(NU))/0.1
      UM     = U - 1.0
      UP     = U + 1.0
C
      DO 14 I=1,KSREG
      DO 14 J=1,I
      IJ     =(I*(I-1))/2+J
   14 P(I,J)=0.5*U*(UM*QQ(NU-1,1,IJ)+UP*QQ(NU+1,1,IJ))-UP*UM*QQ(NU,1,IJ)
      GO TO 2
C
C     CASE FOR KCOMPF=2
C
    1 CONTINUE
      NU = NX(1)
      U  = (X(1)-XL(NU))/0.1
      UM = U-1.
      UP = U+1.
      NV = NX(2)
      V  = (X(2)-XL(NV))/0.1
      VM = V-1.
      VP = V+1.
C
      A1     = 0.5*U*UM
      A2     = 1.0-U*U
      A3     = 0.5*U*UP
      B1     = 0.5*V*VM
      B2     = 1.0-V*V
      B3     = 0.5*V*VP
      DO 40 I=1,KSREG
      DO 40 J=1,I
      IJ     =(I*(I-1))/2+J
      F1     =A1*QQ(NU-1,NV-1,IJ)+A2*QQ(NU,NV-1,IJ)+A3*QQ(NU+1,NV-1,IJ)
      F2     =A1*QQ(NU-1,NV  ,IJ)+A2*QQ(NU,NV  ,IJ)+A3*QQ(NU+1,NV  ,IJ)
      F3     =A1*QQ(NU-1,NV+1,IJ)+A2*QQ(NU,NV+1,IJ)+A3*QQ(NU+1,NV+1,IJ)
      P(I,J) =B1*F1              +B2*F2            +B3*F3
  40  CONTINUE
C
C
    2 CONTINUE
      DO 6 I=1,KSREG
      MI    = MAR(I)
      JF    = I+1
      SUMP  = 0.0
      DO 9 J=1,I
    9 SUMP  = SUMP + P(I,J)
      SUMA  = SUMP - P(I,I)
      IF(I.EQ.KSREG) P(I,I)=1.-SUMA
C
      IF(JF.GT.KSREG) GO TO 6
C
      DO 8 J=JF,KSREG
      MJ    = MAR(J)
      P(I,J)= VOLR(J)*SIG(MJ)*P(J,I)/(VOLR(I)*SIG(MI))
    8 SUMP  = SUMP + P(I,J)
      TMPB  = (1.-SUMA)/(SUMP-SUMA)
      DO 7 J=I,KSREG
      P(I,J)=P(I,J)*TMPB
      IF(I.EQ.J) GO TO 7
      P(J,I)=P(J,I)*TMPB
    7 CONTINUE
    6 CONTINUE
C
C     END OF PROCESS
C
CMOD  DELETED BY JAIS K.KANEKO 1/17/1989 AFTER THIS STATEMENT CCCCCCCCCC
CM    DO 10 K=1,KCOMPF
CM    DO 11 N=1,10
CM    IF(X(K).LT.XL(N)) GO TO 12
CM 11 CONTINUE
CM    NX(K)=11
CM    GO TO 10
CM 12 NX(K)=N-1
CM    IF(KCOMPF.EQ.1.AND.N.EQ.2) NX(K)=2
CM 10 CONTINUE
CM    NU=NX(1)
CM    U=(X(1)-XL(NU))/0.1
CM    UM=U-1.
CM    UP=U+1.
CM    IF(KCOMPF.EQ.1) GO TO 1
CM    NV=NX(2)
CM    V=(X(2)-XL(NV))/0.1
CM    VM=V-1.
CM    VP=V+1.
CM    IF(NU.GT.10.OR.NV.GT.10) GO TO 50
CM    IF(NU.LT.2.OR.NV.LT.2) GO TO 20
CM    A1=0.5*U*UM
CM
CM    A2=0.5*V*VM
CM    A3=2.*(A1+A2)-UM*VM
CM    A4=A1-U*VM
CM    A5=A2-V*UM
CM    A6=U*V
CM    DO 40 I=1,KSREG
CM    DO 40 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 40 P(I,J)=A1*QQ(NU-1,NV,IJ)+A2*QQ(NU,NV-1,IJ)-A3*QQ(NU,NV,IJ)
CM   X+A4*QQ(NU+1,NV,IJ)+A5*QQ(NU,NV+1,IJ)+A6*QQ(NU+1,NV+1,IJ)
CM    GO TO 2
CM 20 IF(NX(1).EQ.9.OR.NX(2).EQ.9) GO TO 30
CM    A1=0.5*U*UM
CM    A2=0.5*V*VM
CM    A3=2.*A1+U*VM
CM    A4=2.*A2+V*UM
CM    A5=0.5*(A3+A4-UM-VM)
CM    A6=U*V
CM    DO 21 I=1,KSREG
CM    DO 21 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 21 P(I,J)=A5*QQ(NU,NV,IJ)-A3*QQ(NU+1,NV,IJ)-A4*QQ(NU,NV+1,IJ)
CM   X+A1*QQ(NU+2,NV,IJ)+A2*QQ(NU,NV+2,IJ)+A6*QQ(NU+1,NV+1,IJ)
CM    GO TO 2
CM 30 DO 31 I=1,KSREG
CM    DO 31 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 31 P(I,J)=UM*VM*QQ(NU,NV,IJ)-U*VM*QQ(NU+1,NV,IJ)
CM   X-V*UM*QQ(NU,NV+1,IJ)+U*V*QQ(NU+1,NV+1,IJ)
CM    GO TO 2
CM 50 IF(NU.GT.10.AND.NV.GT.10) GO TO 70
CM    IF(NU.GT.10) GO TO 60
CM    IF(NU.NE.1) GO TO 52
CM    NU=2
CM    U=(X(1)-XL(NU))/0.1
CM    UM=U-1.
CM    UP=U+1.
CM 52 Y=X(2)/(1.-X(2))
CM    DO 51 I=1,KSREG
CM    DO 51 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 51 P(I,J)=0.5*U*(UM*(QQ(NU-1,11,IJ)-DANY(NU-1,IJ)/Y)
CM   1+UP*(QQ(NU+1,11,IJ)-DANY(NU+1,IJ)/Y))
CM   2-UP*UM*(QQ(NU,11,IJ)-DANY(NU,IJ)/Y)
CM    GO TO 2
CM 60 IF(NV.NE.1) GO TO 62
CM    NV=2
CM    V=(X(2)-XL(NV))/0.1
CM    VM=V-1.
CM    VP=V+1.
CM 62 Y=X(1)/(1.-X(1))
CM    DO 61 I=1,KSREG
CM    DO 61 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 61 P(I,J)=0.5*V*(VM*(QQ(11,NV-1,IJ)-DANX(NV-1,IJ)/Y)
CM   1+VP*(QQ(11,NV+1,IJ)-DANX(NV+1,IJ)/Y))
CM   2-VP*VM*(QQ(11,NV,IJ)-DANX(NV,IJ)/Y)
CM    GO TO 2
CM 70 DO 71 I=1,KSREG
CM    Y=1.
CM    M=MAR(I)
CM    M=NCOR(M)
CM    IF(M.GT.2.OR.M.EQ.0) GO TO 72
CM    Y=X(M)/(1.-X(M))
CM 72 DO 71 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 71 P(I,J)=QQ(11,11,IJ)-DAN(IJ)/Y
CM    GO TO 2
CM  1 IF(NU.GT.10) GO TO 13
CM    DO 14 I=1,KSREG
CM    DO 14 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 14 P(I,J)=0.5*U*(UM*QQ(NU-1,1,IJ)+UP*QQ(NU+1,1,IJ))-UP*UM*QQ(NU,1,IJ)
CM    GO TO 2
CM 13 Y=X(1)/(1.-X(1))
CM    DO 15 I=1,KSREG
CM    DO 15 J=1,I
CM    IJ=(I*(I-1))/2+J
CM 15 P(I,J)=QQ(11,1,IJ)-DAN(IJ)/Y
CM  2 DO 6 I=1,KSREG
CM    MI=MAR(I)
CM    JF=I+1
CM    SUMP=0.
CM    DO 9 J=1,I
CM  9 SUMP=SUMP+P(I,J)
CM    SUMA=SUMP-P(I,I)
CM    IF(I.EQ.KSREG) P(I,I)=1.-SUMA
CM    IF(JF.GT.KSREG) GO TO 6
CM    DO 8 J=JF,KSREG
CM    MJ=MAR(J)
CM    P(I,J)=W(J)*SIG(MJ)*P(J,I)/(W(I)*SIG(MI))
CM    P(I,J)=VOLR(J)*SIG(MJ)*P(J,I)/(VOLR(I)*SIG(MI))
CM  8 SUMP=SUMP+P(I,J)
CM    TMPB=(1.-SUMA)/(SUMP-SUMA)
CM    DO 7 J=I,KSREG
CM    P(I,J)=P(I,J)*TMPB
CM    IF(I.EQ.J) GO TO 7
CM    P(J,I)=P(J,I)*TMPB
CM  7 CONTINUE
CM  6 CONTINUE
CEND  DELETED BY JAIS K.KANEKO 1/17/1989 CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      RETURN
      END
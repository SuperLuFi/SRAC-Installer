      SUBROUTINE LNCHAI(
     1    NUCL     ,RAMDA    ,NCH      ,NUCLP    ,NBIC     ,PBIC     ,
     2    NOL      ,KSTP     ,LONG     ,LL       ,IP       ,KP       ,
     3    NPN      ,NMAX     ,NPAR     ,NCHA     ,LCHA     ,ITYP     )
C
      DIMENSION
     *    NUCL(NMAX)        ,RAMDA(NMAX)       ,NCH(NMAX)        ,
     *    NUCLP(NPAR,NMAX)  ,NBIC(NPAR,NMAX)   ,PBIC(NPAR,NMAX)  ,
     *    NOL(NMAX)         ,KSTP(NCHA,NMAX)   ,LONG(NCHA,NMAX)  ,
     *    LL(NCHA,NMAX)     ,
     *    IP(LCHA,NCHA,NMAX),KP(LCHA,NCHA,NMAX),NPN(NPAR,NMAX)
C
C           LINEAR CHAIN CALCULATION
C
C
C     NUCL     CODE NO. OF NUCLIDE
C     RAMDA    DECAY CONSTANT
C     NCH      THE NUMBER OF PARENTS
C     NUCLP    CODE NO. OF PARENTS
C     NBIC     REACTION TYPE INDICATOR
C                 1  DECAY
C                 3  N-GAMMA
C     PBIC     BRANCHING RATIO
C     NOL      THE NUMBER OF LINEAR CHAINS
C     KSTP     INDICATOR TO SPECIFY THE TOP NUCLIDE OF LINEAR CHAIN
C     LONG     LENGTH OF LINEAR CHAIN
C     LL       INDICATOR TO POINT STARTING NUCLIDE
C     IP       NUCLIDE SEQUENCE NUMBER OF LINEAR CHAIN
C     KP       BRANCHING INDICATOR IN LINEAR CHAIN
C     NPN      NUCLIDE SEQUENCE NUMBER OF PARENT
C     NMAX     THE NUMBER OF NULIDES
C     NPAR     MAXIMUM NUMBER OF PARENTS
C     NCHA     MAXIMUM NUMBER OF LINEAR CHAINS
C     LCHA     MAXIMUM LENGTH OF LINEAR CHAIN
C     ITYP     CHAIN TYPE
C                1 : HOT  (DECAYS AND NEUTRON REACTIONS)
C                2 : COLD (DECAYS ONLY)
C
C     PROLOGUE
C
      DO  90 N=1,NMAX
      NCHN    =NCH(N)
      IF(NCHN.EQ.0) GO TO 80
      DO  70 M=1,NCHN
      IF(NBIC(M,N).EQ.3.OR.NBIC(M,N).GE.7) NBIC(M,N)=-NBIC(M,N)
      DO 60 NN=1,NMAX
      IF(NUCLP(M,N).NE.NUCL(NN)) GO TO 60
      NPN(M,N)=NN
      IF(PBIC(M,N).EQ.0.0.OR.RAMDA(NN).EQ.0.0) NPN(M,N)=-NPN(M,N)
      GO TO 70
   60 CONTINUE
      NPN(M,N)=0
   70 CONTINUE
      GO TO 90
   80 CONTINUE
      NPN(1,N)=0
   90 CONTINUE
C
      DO 120 NN = 1 , NMAX
         DO 110 J=1,NCHA
            LL(J,NN)   = 0
            KSTP(J,NN) = 0
            DO 100 L=1,LCHA
               IP(L,J,NN) = 0
               KP(L,J,NN) = 0
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
C
C     CHAIN CALCULATION
C
      DO 400 NN=1,NMAX
         I       =1
         K       =1
         NOSN    =1
         IP(1,1,NN)  = NN
C
         DO 250 K = 1, LCHA
            NOS  = NOSN
            MSTP = 0
            DO 240 I = 1, NOS
               IF (IABS(KSTP(I,NN)) .EQ. 1 ) GO TO 240
               MSTP = 1
               NCL     =IP(K,I,NN)
               IF (K.EQ.1) GO TO 140
                  KM1     =K-1
                  DO 130 KX=1,KM1
                     IF(NCL.EQ.IP(KX,I,NN)) GO TO 150
  130             CONTINUE
  140          CONTINUE
               IF(NCH(NCL).GT.0.AND.K.LT.LCHA) GO TO 160
  150             CONTINUE
                  KSTP(I,NN) = 1
                  LONG(I,NN) = K
                  GO TO 240
C
C
C
  160          CONTINUE
               J1      =NOSN
               NOSN    =NOSN+NCH(NCL)-1
               IF(NOSN.GT.NCHA) GO TO 91000
               J2      =NOSN
               IF(J1.GT.J2) GO TO 240
               JZ = 0
               DO 235 JJ=J1,J2
                  JZ      =JZ+1
                  J       =I
                  IF ( JZ.EQ.  1 ) GO TO 205
                     J       =JJ
                     LL(J,NN)=K
                     DO 201 L=1,K
                        IP(L,J,NN) =IP(L,I,NN)
                        IF(L.EQ.K) GO TO 201
                           KP(L,J,NN) =KP(L,I,NN)
C                       END IF
  201                CONTINUE
C                 END IF
  205             CONTINUE
                  NB      =NBIC(JZ,NCL)
C
                  IF(NPN(JZ,NCL).EQ.0)                GO TO 210
                     IF(NB.GT.0.AND.NPN(JZ,NCL).LT.0) GO TO 210
                        IF(NB.GT.0)                   GO TO 220
                           IF(ITYP.EQ.1)              GO TO 220
  210             CONTINUE
                  KSTP(J,NN) = 1
                  IF(NB.LT.0) KSTP(J,NN) =-1
                  LONG(J,NN) = K
                  GO TO 235
C
  220             CONTINUE
                  IP(K+1,J,NN)=IABS(NPN(JZ,NCL))
                  KP(K,J,NN) =JZ
  235          CONTINUE
  240       CONTINUE
            IF ( MSTP .EQ. 0 ) GO TO 300
  250    CONTINUE
         IF ( MSTP .EQ. 0 ) GO TO 300
CDEL     GO TO 92000
C
  300    CONTINUE
         NOL(NN) = NOS
C
  400 CONTINUE
C
C     EPILOGUE
C
      DO 520 NN  = 1, NMAX
         NOS = NOL(NN)
         DO 510 I=1,NOS
            LL(I,NN) = LONG(I,NN) - LL(I,NN)
            IF ( LONG(I,NN) .LE. 0 ) GO TO 510
               LLL2 = (LONG(I,NN)-1)/2 + 1
               LLL1 = LONG(I,NN) + 1
               DO 500 L = 1 , LLL2
                  LLL1          = LLL1-1
                  IWORK         = IP(L,I,NN)
                  IP(L,I,NN)    = IP(LLL1,I,NN)
                  IP(LLL1,I,NN) = IWORK
                  IWORK         = KP(L,I,NN)
                  KP(L,I,NN)    = KP(LLL1,I,NN)
                  KP(LLL1,I,NN) = IWORK
  500         CONTINUE
  510    CONTINUE
  520 CONTINUE
C
      DO 560 I=1,NMAX
         DO 550 J=1,NPAR
            NBIC(J,I)=IABS(NBIC(J,I))
            NPN(J,I) =IABS(NPN(J,I))
  550    CONTINUE
  560 CONTINUE
      RETURN
C
C     ERROR MESSAGE SECTION
C
91000 CONTINUE
      WRITE(6,91001) NUCL(NN) , NCHA
91001 FORMAT(1H ,'*** ERROR IN LNCHAI ********* NUCL:',I8/
     *       1H ,' THE NUMBER OF LINEAR CHAINS OVER NCHA : NCHA = ',I5)
      STOP 16
92000 CONTINUE
      WRITE(6,92001) NUCL(NN) , LCHA
92001 FORMAT(1H ,'*** ERROR IN LNCHAI ********* NUCL:',I8/
     *       1H ,' CHAIN LENGTH IS OVER LCHA. :LCHA= ',I5)
      STOP 16
      END

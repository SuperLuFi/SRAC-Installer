      FUNCTION  FNBATE(
     1    LL       ,LONG     ,RAMDA    ,GAMMA    ,ANO      ,R        ,
     2    G        ,P        ,GAMX     ,JX       ,KSTP     ,T        ,
     3    XM       ,NMAX     ,DEX      ,DEXX     ,DCOEF    )
C
      REAL*8     DEX(NMAX),DEXX(NMAX),DCOEF(LONG),T
C
      DIMENSION
     *    RAMDA(NMAX),GAMMA(NMAX),ANO(NMAX),R(LONG),G(LONG),P(LONG),
     *    GAMX(NMAX)  ,JX(LONG)
C
      REAL*8   DA,DGXM,DMS,QG,QF,RL,RM,RI,RLPL,FN,EPS0
      DATA   EPS0   /1.0000D-10   /
C
C
      FN      = 0.0
      IF ( LL   .LE. 0 ) GO TO 1000
      IF ( LONG .LE. 0 ) GO TO 1000
C
C
      LONGT = LONG
C
C
      JXL  = JX(LONGT)
      IF ( LL .LT. LONGT) GO TO 10
         DA   = ANO  (JXL)
         DGXM = GAMMA(JXL)*XM
         IF ( LONGT.EQ.1 .AND. KSTP.EQ.1 ) DGXM = GAMX(JXL)*XM
         FN = FN + DA*DEX(JXL) + DGXM*DEXX(JXL)
   10 CONTINUE
C
C
      IF ( LONGT .LE. 1 ) GO TO 1000
C
C
      LONGM = LONGT - 1
      RL   = RAMDA(JXL)
      DO 100 M = 1 , LONGM
         JXM  = JX(M)
         RM   = RAMDA(JXM)
         DCOEF(M) = 1.0D0
         IF ( DABS((RL - RM)/RM) .LE. EPS0 ) GO TO 100
            DCOEF(M) = DCOEF(M)/(RL-RM)
  100 CONTINUE
      DCOEF(LONGT) = 1.0D0
C
      KSM    = 0
      KSL    = 0
      ICOUNT = 0
C
      DO 800 N = 2 , LONGT
         L     = LONGT - N + 1
         LONGM = L + 1
         JXL   = JX(L)
         RL    = RAMDA(JXL)
         RLPL  = RAMDA(JXL)*P(L)
         DA    = ANO  (JXL)
         DGXM  = GAMMA(JXL)*XM
         IF ( L.EQ.1 .AND. KSTP.EQ.1 ) DGXM = GAMX(JXL)*XM
         IF ( RLPL .LE. 0.0 ) GO TO 9000
C
         DO 300 M = LONGM , LONGT
            JXM      = JX(M)
            RM       = RAMDA(JXM)
            DCOEF(M) = DCOEF(M)*RLPL
C
            IF ( DABS((RL-RM)/RL) .LE. EPS0 ) GO TO 200
               DCOEF(M) = DCOEF(M)/(RL-RM)
               GO TO 300
C
  200          CONTINUE
               KSM    = M
               KSL    = L
               ICOUNT = ICOUNT + 1
               IF ( ICOUNT .GT. 1 ) GO TO 9100
  300    CONTINUE
C
         DO 310 M = 1 , L
            JXM      = JX(M)
            RM       = RAMDA(JXM)
            DCOEF(M) = DCOEF(M)*RLPL
            IF ( DABS((RL-RM)/RL) .LE. EPS0 ) GO TO 310
               DCOEF(M) = DCOEF(M)/(RL-RM)
  310    CONTINUE
C
         IF ( LL .LT. L ) GO TO 800
            IF ( DA.EQ.0.0 .AND. DGXM.EQ.0.0 ) GO TO  800
C
               IF ( KSM .GT. 0 ) GO TO 600
C
                  DMS   = 0.0
                  QG    = 0.0
                  QF    = 0.0
C
                  DO 400 M = LONGM , LONGT
                     JXM  = JX(M)
                     DMS  = DMS  + DCOEF(M)
                     QG   = QG   + DCOEF(M)*DEX (JXM)
                     QF   = QF   + DCOEF(M)*DEXX(JXM)
  400             CONTINUE
                  QG = QG - DMS*DEX (JXL)
                  QF = QF - DMS*DEXX(JXL)
                  GO TO 700
C
  600             CONTINUE
                  DMS   = 0.0
                  QG    = 0.0
                  QF    = 0.0
                  DO 500 M = L , LONGT
                     IF ( M .EQ. KSL ) GO TO 500
                     IF ( M .EQ. KSM ) GO TO 500
                        JXM  = JX(M)
                        DMS  = DMS  + DCOEF(M)
                        QG   = QG   + DCOEF(M)*DEX (JXM)
                        QF   = QF   + DCOEF(M)*DEXX(JXM)
  500             CONTINUE
                  JXKSM = JX(KSM)
                  RI    = RAMDA(JXKSM)
                  QG    = QG + DCOEF(KSM) *DEX (JXKSM)*T
     *                       - DMS*DEX (JXKSM)
                  QF    = QF + DCOEF(KSM)*(DEXX(JXKSM)-T*DEX(JXKSM))/RI
     *                       - DMS*DEXX(JXKSM)
  700       CONTINUE
C
            FN = FN + DA*QG + DGXM*QF
            IF(FN  .LE.0.0) FN  =0.0
  800 CONTINUE
C
C
C
 1000 CONTINUE
      FNBATE = FN
      RETURN
C
C
C
 9000 CONTINUE
      WRITE(6,9001)
      WRITE(6,9002) JXL,L,RL,P(L),DA,XM,DGXM
 9001 FORMAT(1H ,'ERROR STOP AT FNBATE     '/
     *       1H ,'RAMDA LESS THAN EQUAL 0.0')
 9002 FORMAT(1H ,' ## JXL L RL P(L) ANO XM DGXM ## ',2I6,1P5E12.5)
      STOP 200
C
 9100 CONTINUE
      WRITE(6,9101) ICOUNT,JX(LONG),LL
 9101 FORMAT(1H ,'WARNIGN AT FNBATE ** ICOUNT GREATER THAN 1  ]]]',
     *   /1H ,'ICOUNT = ',I6,' FOR ',I4,'-TH NUCLIDE. ** LL = ',I6)
C
      GO TO 1000
C
CM    STOP 200
C
      END

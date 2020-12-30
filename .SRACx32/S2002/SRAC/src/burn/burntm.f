      SUBROUTINE  BURNTM ( NOWSTP , IBC2   , IBEDIT , IDU235 ,NMAT   ,
     1                     NTNUC  , NTDEPZ , MATDPL , DNOLD  ,SSA1B  ,
     2                     MXSTEP , POWERL , TWTHVY , ST235  ,PHIP   ,
     3                     PERIOD , DAYS   , U235F  , EXPST  ,CUMMWD ,
     4                     DELDAY , VOLM   , MATXRG , MXNUC  ,NOUT2  )
C
      INTEGER*4   MATDPL(NMAT)  ,MATXRG(NMAT)
      REAL*4      VOLM  (NMAT)  ,DNOLD(MXNUC,NTDEPZ)
      REAL*4      SSA1B (NTNUC,NTDEPZ),PHIP(NTDEPZ)
      REAL*4      PERIOD(MXSTEP),DAYS(MXSTEP),U235F(MXSTEP)
      REAL*4      EXPST (MXSTEP),CUMMWD(MXSTEP)
C
C
C
      IF(IBEDIT.GT.1) THEN
         WRITE(6,*) ' ** NOWSTP IDU235 ST235 : ',NOWSTP,IDU235,ST235
         ENDIF
C
      IF(NOWSTP.LE.1) THEN
             CALL  CLEA( DAYS     , MXSTEP , 0.0 )
             CALL  CLEA( U235F    , MXSTEP , 0.0 )
             CALL  CLEA( EXPST    , MXSTEP , 0.0 )
             CALL  CLEA( CUMMWD   , MXSTEP , 0.0 )
C
             ST235    = 0.000000
             DO 100 M = 1 , NMAT
             MPOS     = MATDPL(M)
             IF(MPOS.GT.0.AND.MATXRG(M).GT.0) THEN
                  ST235    = ST235 + DNOLD(IDU235,MPOS)*VOLM(M)*1.00E+24
                  ENDIF
  100        CONTINUE
C
      IF(IBEDIT.GT.1) THEN
             WRITE(6,*) ' ** NOWSTP IDU235 ST235 : ',NOWSTP,IDU235,ST235
             ENDIF
C
             ENDIF
C
C  *** CASE FOR IBC2=1 : PERIOD IS GIVEN  BY CUMULATIVE MWD/T UNIT
C
      IF(IBC2.EQ.1.AND.POWERL.GT.0.0) THEN
              IF(NOWSTP.EQ.1) THEN
                    DELEXP  = PERIOD(1)
                    ELSE
                    DELEXP  = PERIOD(NOWSTP) -PERIOD(NOWSTP-1)
                    ENDIF
              DELDAY  = DELEXP*TWTHVY/POWERL
              EXPST (NOWSTP+1) = EXPST (NOWSTP) + DELEXP
              CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP) + DELDAY*POWERL
              DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
              ENDIF
C
      IF(IBC2.EQ.1.AND.POWERL.LE.0.0) THEN
                  DELDAY           = -PERIOD(NOWSTP)
                  EXPST (NOWSTP+1) = EXPST (NOWSTP)
                  CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP)
                  DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
                  PERIOD(NOWSTP  ) = EXPST (NOWSTP)
                  ENDIF
C
C  *** CASE FOR IBC2=2 : PERIOD IS GIVEN  BY CUMULATIVE MWD UNIT
C
      IF(IBC2.EQ.2.AND.POWERL.GT.0.0) THEN
              IF(NOWSTP.EQ.1) THEN
                    DELEXP  = PERIOD(1)
                    ELSE
                    DELEXP  = PERIOD(NOWSTP) -PERIOD(NOWSTP-1)
                    ENDIF
              DELDAY  = DELEXP/POWERL
              EXPST (NOWSTP+1) = EXPST (NOWSTP) + DELEXP/TWTHVY
              CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP) + DELEXP
              DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
              ENDIF
C
      IF(IBC2.EQ.2.AND.POWERL.LE.0.0) THEN
                  DELDAY           = -PERIOD(NOWSTP)
                  EXPST (NOWSTP+1) = EXPST (NOWSTP)
                  CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP)
                  DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
                  PERIOD(NOWSTP  ) = CUMMWD(NOWSTP)
                  ENDIF
C
C  *** CASE FOR IBC2=3 : PERIOD IS GIVEN  BY CUMULATIVE DAYS UNIT
C
      IF(IBC2.EQ.3) THEN
              IF(NOWSTP.EQ.1) THEN
                    DELDAY  = PERIOD(1)
                    ELSE
                    DELDAY  = PERIOD(NOWSTP) - PERIOD(NOWSTP-1)
                    ENDIF
              RFACT         = 0.000
              IF(TWTHVY.GT.0.0)   RFACT = 1.00000 / TWTHVY
CMOD          EXPST (NOWSTP+1) = EXPST (NOWSTP) + POWERL*DELDAY/TWTHVY
              EXPST (NOWSTP+1) = EXPST (NOWSTP) + POWERL*DELDAY*RFACT
              CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP) + POWERL*DELDAY
              DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
              ENDIF
C
C  *** CASE FOR IBC2=4 : PERIOD IS GIVEN  BY DELTA DAYS UNIT
C
      IF(IBC2.EQ.4) THEN
              DELDAY  = PERIOD(NOWSTP)
              RFACT   = 0.000
              IF(TWTHVY.GT.0.0)   RFACT = 1.00000 / TWTHVY
CMOD          EXPST (NOWSTP+1) = EXPST (NOWSTP) + POWERL*DELDAY/TWTHVY
              EXPST (NOWSTP+1) = EXPST (NOWSTP) + POWERL*DELDAY*RFACT
              CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP) + POWERL*DELDAY
              DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
              ENDIF
C
C  *** CASE FOR IBC2=5 : PERIOD IS GIVEN  BY U-235 FRACTION
C
      IF(IBC2.EQ.5.AND.POWERL.GT.0.0) THEN
              IF(NOWSTP.EQ.1) THEN
                    DELU5   = PERIOD(1)
                    ELSE
                    DELU5   = PERIOD(NOWSTP) -PERIOD(NOWSTP-1)
                    ENDIF
              U5ABS     = 0.0
              DO 200  M = 1  , NMAT
              MPOS      = MATDPL(M)
              IF(M.GT.0.AND.MATXRG(M).GT.0) THEN
                U5ABS     = U5ABS  +
     1          PHIP(MPOS)*VOLM(M)*DNOLD(IDU235,MPOS)*SSA1B(IDU235,MPOS)
                       ENDIF
  200         CONTINUE
              SEC       = DELU5*1.000E-2*ST235/U5ABS
              DELDAY    = SEC/3600.00/24.0000
C
              EXPST (NOWSTP+1) = EXPST (NOWSTP) + POWERL*DELDAY/TWTHVY
              CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP) + POWERL*DELDAY
              DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
              ENDIF
C
      IF(IBC2.EQ.5.AND.POWERL.LE.0.0) THEN
                  DELDAY           = -PERIOD(NOWSTP)
                  EXPST (NOWSTP+1) = EXPST (NOWSTP)
                  CUMMWD(NOWSTP+1) = CUMMWD(NOWSTP)
                  DAYS  (NOWSTP+1) = DAYS  (NOWSTP) + DELDAY
                  PERIOD(NOWSTP  ) = U235F (NOWSTP)
                  ENDIF
C
      IF(IBEDIT.GT.1.OR.DELDAY.LE.0) THEN
         WRITE(6,*) ' ** IBC2 NOWSTP TWTHVY ST235 POWERL DELDAY : ',
     1                   IBC2,NOWSTP,TWTHVY,ST235,POWERL,DELDAY
         WRITE(6,301) (PERIOD(I),I=1,NOWSTP)
         WRITE(6,302) (DAYS  (I),I=1,NOWSTP+1)
         WRITE(6,303) (EXPST (I),I=1,NOWSTP+1)
         WRITE(6,304) (CUMMWD(I),I=1,NOWSTP+1)
C
         WRITE(NOUT2,*) ' ** IBC2 NOWSTP TWTHVY ST235 POWERL DELDAY : ',
     1                   IBC2,NOWSTP,TWTHVY,ST235,POWERL,DELDAY
         WRITE(NOUT2,301) (PERIOD(I),I=1,NOWSTP)
         WRITE(NOUT2,302) (DAYS (I),I=1,NOWSTP+1)
         WRITE(NOUT2,303) (EXPST (I),I=1,NOWSTP+1)
         WRITE(NOUT2,304) (CUMMWD(I),I=1,NOWSTP+1)
         ENDIF
C
      IF(DELDAY.LE.0.0) THEN
          WRITE(    6,*) ' ** FATAL INPUT ERROR STOP ST BURNTM !! '
          WRITE(    6,*) ' ** NEGATIVE BUTNUP TIME WAS CALCULATED !! '
          WRITE(    6,*) ' ** PLEASE CHECK INPUT PERIOD OR POWERL DATA.'
          WRITE(    6,*) ' ** FATAL INPUT ERROR STOP ST BURNTM !! '
          WRITE(    6,*) ' ** NEGATIVE BUTNUP TIME WAS CALCULATED !! '
          WRITE(    6,*) ' ** PLEASE CHECK INPUT PERIOD OR POWERL DATA.'
          STOP 980
          ENDIF
C
  301 FORMAT(1H ,' >>PERIOD : ',1P10E11.4)
  302 FORMAT(1H ,' >>DAYS   : ',1P10E11.4)
  303 FORMAT(1H ,' >>EXPST  : ',1P10E11.4)
  304 FORMAT(1H ,' >>CUMMWD : ',1P10E11.4)
C
      RETURN
      END

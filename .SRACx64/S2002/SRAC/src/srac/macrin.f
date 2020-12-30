      SUBROUTINE MACRIN(ANSWER,LENG  ,RTEMP ,ITLOOP,NET  ,
     +                  AIDENT,BIDENT,CIDENT,ATEMP ,BTEMP,CTEMP ,
     +                  ADATA ,BDATA ,CDATA                       )
C
      CHARACTER*8       AIDENT,BIDENT,CIDENT
      REAL*4            ANSWER(NET,1)
      REAL*4            ADATA (NET,1),BDATA(NET,1),CDATA(NET,1)
      REAL*4            SIGS  (50)  , SIGSS(50)
C
C-----READ DATA
C
       IF(ITLOOP.LE.1) STOP  987
       IF(ITLOOP.GT.3) STOP  988
C
CKK9  CALL  READ ( AIDENT , ADATA , LENG )
CKK9  CALL  READ ( BIDENT , BDATA , LENG )
CKK9  IF(ITLOOP.GT.2)  CALL READ ( CIDENT , CDATA , LENG )
C
C-----INTERPOLATE THERMAL DATA
C
       IF(ITLOOP.EQ.3) GO TO 101
C
       GRAD     = ( RTEMP - ATEMP) / ( BTEMP - ATEMP )
       DO 100 I = 1 , LENG
       ANSWER(I,1) = ADATA(I,1) + GRAD * ( BDATA(I,1) - ADATA(I,1) )
  100  CONTINUE
       GO TO 251
C
  101  CONTINUE
       ACOEF = (RTEMP-BTEMP)*(RTEMP-CTEMP)/(ATEMP-BTEMP)/(ATEMP-CTEMP)
       BCOEF = (RTEMP-ATEMP)*(RTEMP-CTEMP)/(BTEMP-ATEMP)/(BTEMP-CTEMP)
       CCOEF = (RTEMP-ATEMP)*(RTEMP-BTEMP)/(CTEMP-ATEMP)/(CTEMP-BTEMP)
C
CM     WRITE(6,102) RTEMP,ATEMP,BTEMP,CTEMP
CM102  FORMAT(1H ,' ## RTEMP ATEMP BTEMP CTEMP (MACRIN) ## ',4F10.3)
C
       ITYPE    = 1
       GRAD     = ( RTEMP - ATEMP) / ( BTEMP - ATEMP )
       IF(RTEMP.GT.BTEMP) THEN
                          ITYPE  = 2
                          GRAD   = (RTEMP - BTEMP)/(CTEMP-BTEMP)
                          ENDIF
C
CM     WRITE(6,104) ITYPE,GRAD
CM104  FORMAT(1H ,' ## ITYPE GRAD ## ',I6,1PE12.5)
C
       DO 200 I = 1 , LENG
       ANSWER(I,1) = ACOEF*ADATA(I,1)+BCOEF*BDATA(I,1)+CCOEF*CDATA(I,1)
  200  CONTINUE
C
       IF(AIDENT(1:1).EQ.'K') THEN
       DO 205 I = 1 , LENG
       ASAVE    = ADATA(I,1)
       BSAVE    = ASAVE*BDATA(I,1)*CDATA(I,1)
       IF(BSAVE.LE.0.0)     GO TO 205
       IF(ASAVE.GE.5.0E-3)  GO TO 205
       BSAVE       = ACOEF*ALOG(ASAVE)
     +             + BCOEF*ALOG(BDATA(I,1))  +  CCOEF*ALOG(CDATA(I,1))
       ANSWER(I,1) = EXP(BSAVE)
  205  CONTINUE
                              ENDIF
C
       IF(ITYPE.EQ.1) THEN
       DO 210 I = 1 , LENG
       IF(ABS(ADATA(I,1)).LT.1.0E-30)
     + ANSWER(I,1) = ADATA(I,1) + GRAD * ( BDATA(I,1) - ADATA(I,1) )
  210  CONTINUE
                      ENDIF
       IF(ITYPE.EQ.2) THEN
       DO 220 I = 1 , LENG
       IF(ABS(BDATA(I,1)).LT.1.0E-30)
     + ANSWER(I,1) = BDATA(I,1) + GRAD * ( CDATA(I,1) - BDATA(I,1) )
  220  CONTINUE
                      ENDIF
C------CHECK BALANCE
  251  CONTINUE
CMOD   IF(LENG.LE.4*NET)  GO TO 401
       IF(LENG.LE.5*NET)  GO TO 401
C
       DO 300 N = 1 , NET
       SIGS (N) = ANSWER(N,NET+1)
       DO 250 J = 1 , NET
       SIGS (N) = SIGS (N) + ANSWER(J,N)
  250  CONTINUE
       SIGSS(N) = ANSWER(N,NET+3) - ANSWER(N,NET+2) - ANSWER(N,NET+4)
  300  CONTINUE
C
CM     WRITE(6,601) (SIGS(I),I=1,NET)
CM     WRITE(6,602) (SIGSS(I),I=1,NET)
C
       IF(AIDENT(1:1).NE.'K') THEN
                              DO 310 N = 1 , NET
                              ANSWER(N,NET+3) = SIGS(N) +
     @                        ANSWER(N,NET+2) + ANSWER(N,NET+4)
  310                         CONTINUE
*                             WRITE(6,*) ' AIDENT = ',AIDENT
*                             WRITE(6,605) (SIGS(I),I=1,NET)
*                             WRITE(6,606) (ANSWER(I,NET+3),I=1,NET)
                              GO TO 401
                              ENDIF
C------ONLY 'K'-MATRIX IS MODIFYED --------------< FROM 2/1/1990 >------
       DO 350 N = 1 , NET
       IF(SIGS(N).EQ.0.0) THEN
                          SIGSS(N) = 1.0
                          ELSE
                          SIGSS(N) = SIGSS(N)/SIGS(N)
                          ENDIF
  350  CONTINUE
CM     WRITE(6,603) (SIGSS(I),I=1,NET)
C------MODIFY THERMAL KERNEL
       DO 400 N = 1 , NET
       RATIO    = SIGSS(N)
       IF(RATIO.EQ.1.0) GO TO 400
       ANSWER(N,NET+1) = ANSWER(N,NET+1)*RATIO
       DO 370       J  = 1 , NET
       ANSWER(J,N)     = ANSWER(J,N)*RATIO
  370  CONTINUE
  400  CONTINUE
C
CM601  FORMAT(1H ,' ## SIGS  ## ',1P10E11.4)
CM602  FORMAT(1H ,' ## SIGSS ## ',1P10E11.4)
CM603  FORMAT(1H ,' ## RATIO ## ',1P10E11.4)
* 605  FORMAT(1H ,' ## SIGS  ## ',1P10E11.4)
* 606  FORMAT(1H ,' ## SIG-T ## ',1P10E11.4)
C
  401  NCOL = (LENG+1)/NET
       CALL UTLWOT( BDATA , NCOL , NET , 1 , 'GRP.' , 'TRAN' , '    ',
     +              '  THERMAL BDATA     '      )
       CALL UTLWOT(ANSWER , NCOL , NET , 1 , 'GRP.' , 'TRAN' , '    ',
     +              ' INTERPOLATED DATA  '      )
C
C       END FO PROCESS
C
        RETURN
        END

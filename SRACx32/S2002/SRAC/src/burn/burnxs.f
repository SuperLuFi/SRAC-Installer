      SUBROUTINE BURNXS( IMAX  ,LAPSE ,NMAT  ,NXR   ,MAXMT3,MXNUC ,
     1                   NREG  ,NISO  ,MATXRG,VOLM  ,VOLX  ,
     2                   FLUXMF,FLUXXF,FLUXMB,FLUXXB,FLUXM1,FLUXX1,
     3                   CC    ,CCB   ,CCX   ,IPBURN,NTDEPZ,MATDPL,
     4                   SSC1G ,SSF1G ,SSA1G ,S2N1G ,SSP1G ,NTNUC )
C
C
C
       REAL*4   VOLM  (NMAT)      ,VOLX  (NXR)
       REAL*4   FLUXMF(IMAX ,NMAT),FLUXXF(IMAX ,NXR)
       REAL*4   FLUXMB(LAPSE,NMAT),FLUXXB(LAPSE,NXR)
       REAL*4   FLUXM1(NMAT)      ,FLUXX1(NXR)
       REAL*4   CC (IMAX ,MAXMT3,MXNUC,NMAT)
       REAL*4   CCB(LAPSE,MAXMT3,MXNUC,NMAT)
       REAL*4   CCX(LAPSE,MAXMT3,MXNUC,NMAT)
       REAL*4   SSC1G(MXNUC,NMAT),SSF1G(MXNUC,NMAT),S2N1G(MXNUC,NMAT)
       REAL*4   SSA1G(MXNUC,NMAT),SSP1G(MXNUC,NMAT)
C
       INTEGER*4   NISO(NMAT),MATXRG(NMAT),NREG(IMAX)
       INTEGER*4   IPBURN(MXNUC,NTDEPZ),MATDPL(NMAT)
C
C  *** START OF PROCESS
C
       CALL  CLEA ( FLUXMB , LAPSE*NMAT        , 0.0 )
       CALL  CLEA ( FLUXXB , LAPSE*NXR         , 0.0 )
       CALL  CLEA ( FLUXM1 ,       NMAT        , 0.0 )
       CALL  CLEA ( FLUXX1 ,       NXR         , 0.0 )
       LENG  = LAPSE*MAXMT3*MXNUC*NMAT
       CALL  CLEA ( CCB    ,       LENG        , 0.0 )
       CALL  CLEA ( SSC1G  , MXNUC*NMAT        , 0.0 )
       CALL  CLEA ( SSF1G  , MXNUC*NMAT        , 0.0 )
       CALL  CLEA ( S2N1G  , MXNUC*NMAT        , 0.0 )
       CALL  CLEA ( SSA1G  , MXNUC*NMAT        , 0.0 )
       CALL  CLEA ( SSP1G  , MXNUC*NMAT        , 0.0 )
C
C *** CALCULATE BROAD GROUP FLUX
C
       DO 100          M = 1 , NMAT
CKKADD 05/1997
       IF(MATXRG(M).LE.0) GO TO 100
       DO  90          I = 1 , IMAX
       SAVE              = FLUXMF(I,M)/VOLM(M)
       FLUXMB(NREG(I),M) = FLUXMB(NREG(I),M) + SAVE
       FLUXM1(M)         = FLUXM1(M)         + SAVE
       FLUXMF(I,M)       = SAVE
   90  CONTINUE
  100  CONTINUE
C
       DO 110          M = 1 , NXR
       DO 110          I = 1 , IMAX
       SAVE              = FLUXXF(I,M)/VOLX(M)
       FLUXXB(NREG(I),M) = FLUXXB(NREG(I),M) + SAVE
       FLUXX1(M)         = FLUXX1(M)         + SAVE
       FLUXXF(I,M)       = SAVE
  110  CONTINUE
C
C **** CALCULATE BROAD GROUP EFFECTIVE MICROSCOPIC X-SECTION
C
       DO 210 M   = 1 , NMAT
       IF(MATXRG(M).LE.0) GO TO 210
       IF(NISO  (M).LE.0) GO TO 210
       DO 200 ISO = 1 , NISO(M)
       DO 200 MT  = 1 , MAXMT3
       DO 200 NG  = 1 , IMAX
       CCB(NREG(NG),MT,ISO,M) = CCB(NREG(NG),MT,ISO,M) +
     *                          CC (NG,MT,ISO,M)*FLUXMF(NG,M)
  200  CONTINUE
  210  CONTINUE
C
       DO 310 M   = 1 , NMAT
       IF(MATXRG(M).LE.0) GO TO 310
       IF(NISO  (M).LE.0) GO TO 310
       DO 300 ISO = 1 , NISO(M)
       DO 300 MT  = 1 , MAXMT3
       DO 300 NG  = 1 , LAPSE
       CCB(NG,MT,ISO,M) = CCB(NG,MT,ISO,M) / FLUXMB(NG,M)
  300  CONTINUE
  310  CONTINUE
C
       DO 420 M   = 1 , NMAT
       IF(MATXRG(M).LE.0) GO TO 420
       IF(NISO  (M).LE.0) GO TO 420
       DO 410 ISO = 1 , NISO(M)
       DO 400 NG  = 1 , IMAX
       SSC1G(ISO,M) = SSC1G(ISO,M) + CC(NG,1,ISO,M)*FLUXMF(NG,M)
       SSF1G(ISO,M) = SSF1G(ISO,M) + CC(NG,2,ISO,M)*FLUXMF(NG,M)
       S2N1G(ISO,M) = S2N1G(ISO,M) + CC(NG,3,ISO,M)*FLUXMF(NG,M)
       SSP1G(ISO,M) = SSP1G(ISO,M) + CC(NG,6,ISO,M)*FLUXMF(NG,M)
  400  CONTINUE
       SSC1G(ISO,M) = SSC1G(ISO,M)/FLUXM1(M)
       SSF1G(ISO,M) = SSF1G(ISO,M)/FLUXM1(M)
       S2N1G(ISO,M) = S2N1G(ISO,M)/FLUXM1(M)
       SSP1G(ISO,M) = SSP1G(ISO,M)/FLUXM1(M)
       SSA1G(ISO,M) = SSC1G(ISO,M) + SSF1G(ISO,M)
  410  CONTINUE
  420  CONTINUE
C
C
       LENG  = LAPSE*MAXMT3*MXNUC*NMAT
       CALL  CLEA ( CCX    ,       LENG        , 0.0 )
C
       DO 450 M   = 1 , NMAT
       MPOS       = MATDPL(M)
       IF(MPOS.LE.0) GO TO 450
       DO 440 I   = 1 , NTNUC
       IPOS       = IPBURN(I,MPOS)
       IF(IPOS.GT.0) THEN
                     DO 430 MT  = 1 , MAXMT3
                     DO 430 NG  = 1 , LAPSE
                     CCX(NG,MT,I,M) = CCB(NG,MT,IPOS,M)
  430                CONTINUE
                     ENDIF
  440  CONTINUE
  450  CONTINUE
C
       RETURN
       END

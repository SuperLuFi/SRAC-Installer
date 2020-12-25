C***********************************************************************
C                             SIGT
C***********************************************************************
C === SUBPROGRAM TO READ SIGT FROM MACRO FILE AND WRITE INTO F04
C***********************************************************************
C
      SUBROUTINE SIGT(NG,MATNM,MATD,NMP)
C
      CHARACTER*4  MATNM(2,*),RANGE(3),NODE(2),FILENM,ICF
C
      COMMON /MAINC / IOPT(100)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /WORK  / SIG(107),AA(7500)
C
      EQUIVALENCE (AA(1),II(1)),(IOPT(55),NEF),(IOPT(57),NERF)
     &           ,(IOPT(64),NOUT1),(IOPT(98),IRANG),(IOPT(99),ICF)
*    *           ,(IOPT(65),NOUT2)
C
      DIMENSION II(1),MATD(*)
C
      DATA RANGE/'FAST','THER','ALL '/
C
C *** START OF PROCESS ****
C
      IFLSW     = 1
      FILENM(1) = 'MACR'
      FILENM(2) = 'O   '
      FILENM(3) = '    '
      IF(ICF.EQ.'0002') FILENM(2)='OWRK'
      REWIND 4
C
      DO 1000 NM = 1,NMP
      NODE(1)    = MATNM(1,MATD(NM))
      NODE(2)    = MATNM(2,MATD(NM))
      CALL PACKX( NODE(2),1,RANGE(IRANG+1),1)
      CALL PACK ( NODE(2),4,ICF)
CMOD
CM    IF(ICF.EQ.'0002')    NODE(2)(4:4) = '4'
CEND
      IFLAG      = 0
      CALL SEARCH(NODE,LTH,ISW)
      ISW        = 0
      JSW        = 0
      IF(ISW.EQ.1) THEN
                   NODE(2)(1:1)='A'
                   CALL SEARCH(NODE,LTH,JSW)
                   IF(JSW  .EQ.1) GO TO 1111
                   IF(IRANG.EQ.1) IFLAG=1
                   ENDIF
C
      CALL READ(NODE,AA,LTH)
      K  = 0
      IF(IFLAG.EQ.1) THEN
                     IREAD = NERF
                     IF(ICF.EQ.'0002') IREAD=NEF
                     DO 800 N = 1,IREAD
                     K        = K + II(K+2) + 10
  800                CONTINUE
                     ENDIF
C
      DO  900 N = 1,NG
      SIG(N)    = AA(K+6)
      K         = K+II(K+2)+10
  900 CONTINUE
C
      WRITE(4) (SIG(K),K=1,NG)
C
*     WRITE(NOUT2,150) NM,(SIG(N),N=1,NG)
 1000 CONTINUE
C
C  *** END OF PROCESS
C
      END FILE 4
      RETURN
C
C  *** ERROR CASE WHEN MACROSCOPIC X-SECTION WAS NOT FOUND ]]]]
C
 1111 CONTINUE
      WRITE(NOUT1,1112) NODE(1),NODE(2),RANGE(IRANG+1)
      STOP
C
* 150 FORMAT(10X,'TOTAL CROSS SECTION FOR PIJ2  MAT=',I2/(10X,10E12.5))
 1112 FORMAT(1H0,'*** MATERIAL *',2A4,'* NOT FOUND IN ', A4,' RANGE OF',
     &' MACRO X-SECTION FILE *** SIGT STEP ***')
C
      END

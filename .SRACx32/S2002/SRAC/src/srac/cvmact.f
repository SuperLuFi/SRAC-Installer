      SUBROUTINE CVMACT(NM,ID,MATR,MTNAME)
C
      CHARACTER *4    ICF,FILELB
C
      COMMON /MAINC / III(500)
CITA  COMMON /SRACIT/ ID2,IXKI,IDELAY,IXYZ(ZNEX)
      COMMON /SRACIT/ ID2,IXKI,IDELAY,IXYZ(500)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILELB(3),ECODE,TEMPRY
C
      COMMON /WORK  / B(8500),BN(8500),XX(107),SCATT(107),
     *                IA(20),XXX(107,150),DD2(107,150)
C
      INTEGER*4       MATR(1)
      INTEGER*4       OTFILE
      CHARACTER*4     RANGE(3),MTNAME(2,1),IIDENT(2)
      DIMENSION       IB(8500),IBN(8500)
C
      EQUIVALENCE (III(55),NEF  ),(III(56),NET),(III(57),NERF)
      EQUIVALENCE (III(58),NERT),(III(59),NMAT)
      EQUIVALENCE (III(98),IRANG),(III(99),ICF)
      EQUIVALENCE (B(1),IB(1)),(BN(1),IBN(1))
C
C  FT31  PS LRECL=80 BLKSIZE=3200 USED BY CITATION
C
      DATA           OTFILE/31/
      DATA           RANGE/'FAST','THER','ALL '/
      DATA           IPRINT /6/
C
C     START OF PROCESS
C
      REWIND OTFILE
      NGRMAX  = 107
      IFLAG   =   0
C
      IF(ID.NE.1.AND.ID.NE.2.AND.ID.NE.3)       GO TO 5010
      IF((ID.EQ.3.OR.IXKI.NE.0).AND.NM.GT.150)  GO TO 5020
C
      FILELB(1) = 'MACR'
      FILELB(2) = 'O '
      NGR       = NERF + NERT
      IF(ICF.EQ.'0002') THEN
                        FILELB(2) = 'OWRK'
                        NGR       = III(55+IRANG)
                        IF(IRANG.EQ.2) NGR = NEF+NET
                        ENDIF
C
      IX28       = NGR - 1
      IX29       = 1
      WRITE(OTFILE,7000)
      WRITE(OTFILE,7010) NGR,IX28,IX29,ID2,IXKI,IDELAY
      LTH        = 0
C
C     LOOP OF MATERIAL
C
      DO 6100  M = 1,NM
      MM         = MATR(M)
C     WRITE(6,*) '   IN CVMACT MM = ',MM
      IIDENT(1)  = MTNAME(1,MM)
      IIDENT(2)  = MTNAME(2,MM)
      IIDENT(2) (1:1)  = RANGE(IRANG+1) (1:1)
      IIDENT(2) (4:4)  = ICF(4:4)
      CALL GETLEN(IIDENT,LTH)
      IF(LTH.GT.8500) GO TO 5030
      CALL READ(IIDENT,B,LTH)
      IF (ICF.EQ.'0000')  IIDENT(2) (4:4) = 'N'
      IF (ICF.EQ.'0002')  IIDENT(2) (4:4) = 'M'
C === N2N DATA
      CALL SEARCH(IIDENT,LTHN,ISW)
      IFN2N      = 0
      IF(ISW.EQ.0) THEN
                   CALL READ(IIDENT,BN,LTHN)
                   IFN2N = 1
                   ENDIF
      IBP        = 0
      LOCN       = 0
C === LOOP OF ENERGY GROUP
      DO 6070  K = 1,NGR
      CALL CLEA( SCATT , NGR , 0.0 )
      LSS        = IB(IBP+1)
      LGV        = IB(IBP+2)
      SIGACT     =  B(IBP+3)
      SIGF       =  B(IBP+4)
CM    SIGWAT     =  SIGF * 3.108E-11
CM    ENERGY/FISSION IS TAKEN FROM U-235
CM    ENERGY/FISSION FOR PU-239 IS 3.392
      SIGWAT     =  SIGF * 3.246E-11
CEND
      VSIGF      =  B(IBP+5)
      SIGT       =  B(IBP+6)
      X          =  B(IBP+7)
      D1         =  B(IBP+8)
      D2         =  B(IBP+9)
      SIGA       =  B(IBP+10)
      J1         =  K  - LSS + 1
      J2         =  K  + LGV - LSS
      IBP        = IBP + 10
      IF(IFLAG.EQ.0) XX(K)      =  X
C === SCATTERING TRANSFER
      DO 6030  J = J1,J2
      IBP        = IBP + 1
      SCATT(J)   = B(IBP)
      IF(J.EQ.K) SCATT(J) = 0.0
 6030 CONTINUE
C ===  CORRECT ABSORBTION X-SECTION DUE TO (N,2N) REACTION
      IF( IFN2N.NE.0 .AND. LOCN.LT.LTHN) THEN
                          J2        = IBN(LOCN+2)
                          LOCN      = LOCN + 10
                          DO 6035 J = 1,J2
                          LOCN      = LOCN +  1
                          SIGA      = SIGA - BN(LOCN)
 6035                     CONTINUE
                                         ENDIF
C
      IF(ID.EQ.2.AND.ID2.EQ.0) D1 = D2
      XXX(K,M)   = X
      DD2(K,M)   = D2
      WRITE(OTFILE,7020) M,K,D1,SIGA,VSIGF,SIGACT,SIGWAT
      WRITE(OTFILE,7030) (SCATT(J),J=1,NGR)
 6070 CONTINUE
CM    IX29=NGR-IX29
      IF(XX(1).NE.0.0) IFLAG = 1
 6100 CONTINUE
C
C     END OF MATERIAL LOOP
C
      WRITE(OTFILE,7040)
      WRITE(OTFILE,7030) (XX(K),K=1,NGR)
C === OUTPUT MATERI-WISE ANISOTROPIC DIFFUSION COEFFICENT
      IF(ID2.EQ.0) GO TO 3000
      DO 100 M = 1,NM
  100 WRITE(OTFILE,7030) (DD2(I,M),I=1,NGR)
C === OUTPUT MATERI-WISE FISSION SPECTRUM
 3000 IF(IXKI.EQ.0) GO TO 3100
      DO 110 M = 1,NM
  110 WRITE(OTFILE,7030) (XXX(I,M),I=1,NGR)
 3100 IF(IDELAY.EQ.0) GO TO 6300
C
      LTMAX     = 0
      NFAMLY    = 6
      DO 115  M = 1,NM
      MM        = MATR(M)
      IIDENT(1) = MTNAME(1,MM)
      IIDENT(2) = MTNAME(2,MM)
      IIDENT(2) (1:1)  = RANGE(IRANG+1) (1:1)
      IF (ICF.EQ.'0000') IIDENT(2) (4:4) = 'Z'
      IF (ICF.EQ.'0002') IIDENT(2) (4:4) = 'Y'
      CALL SEARCH(IIDENT,LTHD,ISW)
      IF(ISW.EQ.1)      GO TO 115
      IF(LTHD.GT.LTMAX)     LTMAX  =LTHD
  115 CONTINUE
CMOD  IF(LTMAX.EQ.2*15*NGR) NFAMLY = 15
      IF(LTMAX.EQ.3*15*NGR) NFAMLY = 15
      IDELAY    = NFAMLY
C
      DO 6200 M = 1,NM
      MM        = MATR(M)
      IIDENT(1) = MTNAME(1,MM)
      IIDENT(2) = MTNAME(2,MM)
      IIDENT(2) (1:1)  = RANGE(IRANG+1) (1:1)
      IF (ICF.EQ.'0000') IIDENT(2) (4:4) = 'Z'
      IF (ICF.EQ.'0002') IIDENT(2) (4:4) = 'Y'
      CALL CLEA ( B ,8500 , 0.0 )
      CALL SEARCH(IIDENT,LTHD,ISW)
CM    IF(ISW .EQ.   1)  GO TO 6200
      IF(ISW .EQ.   1)  THEN
CM                      LTHD      = 2*NFAMLY*NGR
                        LTHD      = 3*NFAMLY*NGR
                        GO TO 6200
                        ENDIF
      LTH       = LTHD
      IF(LTHD.GT.8500)  GO TO 5030
C
      LTHD1     = LTHD
CM    LTHD      = 2*NFAMLY*NGR
      LTHD      = 3*NFAMLY*NGR
      CALL READ  (IIDENT,B,LTHD1)
C
CM    IF(NFAMLY.EQ.6.OR.LTHD1.EQ.2*15*NGR) GO TO 6200
      IF(NFAMLY.EQ.6.OR.LTHD1.EQ.3*15*NGR) GO TO 6200
CM    JJF      = 12*NGR
CM    DO 1   I = 1,2*NGR
CM    JJT      = 15*(2*NGR-I)+6
CM    DO 1   J = 1,6
CM    B(JJT)   = B(JJF)
CM    JJF      = JJF - 1
CM    JJT      = JJT - 1
CM  1 CONTINUE
CM    DO 2 I   = 1,2*NGR
CM    DO 2 J   = 7,15
CM    JJ       = 15*(I-1) + J
CM    B(JJ)    = 0.0
CM  2 CONTINUE
      JJF      = 18*NGR
      DO 1   I = 1,3*NGR
      JJT      = 15*(3*NGR-I)+6
      DO 1   J = 1,6
      B(JJT)   = B(JJF)
      JJF      = JJF - 1
      JJT      = JJT - 1
    1 CONTINUE
      DO 2 I   = 1,3*NGR
      DO 2 J   = 7,15
      JJ       = 15*(I-1) + J
      B(JJ)    = 0.0
    2 CONTINUE
 6200 WRITE(OTFILE,7030) (B(K),K=1,LTHD)
C
C     END OF DELAYED NEUTRON DATA OUTPUT
C
 6300 REWIND OTFILE
C
C     END OF PROCESS
C
      RETURN
C
 5010 WRITE(IPRINT,8010)
      STOP
C
 5020 WRITE(IPRINT,8020)
     1  '*** ERROR IN CVMACT *** NM (',NM,') IS OVER 150. AND ID=3 ',
     2  'OR/AND IXKI .NE. 0 '
      STOP
C
 5030 WRITE(IPRINT,8030) IIDENT,LTH
      STOP
C
 7000 FORMAT('008')
 7010 FORMAT(6I3)
C7020 FORMAT(2I6,5E12.5)
C7030 FORMAT(6E12.5)
 7020 FORMAT(2I6,1P5E12.5)
 7030 FORMAT(1P6E12.5)
 7040 FORMAT(80X)
 7050 FORMAT(20A4)
 8010 FORMAT(1H0,'ID ERROR, ID TAKE 1 OR 2,1 TAKE D1(K),2 TAKE D2(K)')
 8020 FORMAT(1H0,A,I5,A)
 8030 FORMAT(1H0,' ERROR STOP IN READING ',2A4,' OF MACRO FILE ]] ',
     +      /1H ,I10,' WORD''S WORK DIMENSION IS NEEDED (CVMACT) .')
C
      END

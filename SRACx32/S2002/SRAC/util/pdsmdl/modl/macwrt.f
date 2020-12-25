C***********************************************************************
C  PROGRAM TO REWRITE MACRO OR MACROWRK PDS FILE DATA                  *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE MACWRT(DIRNAM, IOUT, IPRN, MEMNAM, NG, XSEC,
     &                  MINSG, MAXSG, SCAT, DELAY)
C
C============================== FOR MAIN ===============================
CDEL  PARAMETER (MAXNG=107, MAXXS=10, MAXUP=48)
      INCLUDE  'PARAMINC'
      DIMENSION  XSEC(MAXNG,MAXXS)
      DIMENSION  SCAT(MAXNG,-MAXUP:MAXNG)
      DIMENSION  DELAY(15,MAXNG,3)
      CHARACTER*72  DIRNAM
      CHARACTER* 8  MEMNAM
C=======================================================================
CDEL  PARAMETER  (MAXWRK=10000)
      COMMON /WKPDS/ WORK(MAXWRK)  
      DIMENSION      IWORK(1)
      EQUIVALENCE ( WORK(1),IWORK(1) )
      CHARACTER*1   PTAG
      CHARACTER* 8  MEMBER
C------------------------------ INPUT ----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/MACRO01
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO REWRITE(A8)
C     NG         : NUMBER OF ENERGY GROUPS CORRESPONDING TO INPUT
C                  MACRO MEMBER NAME
C     XSEC(G,I)  : MACROSCOPIC XS OF XS-TYPE:I, ENERGY GROUP:G
C     * NEED ONLY WHEN THE 8TH CHARACTER OF MEMNAM .NE. 'Y' OR 'Z'
C                  I=1 : PRODUCTION
C                  I=2 : FISSION
C                  I=3 : CAPTURE
C                  I=4 : ABSORPTION
C                  I=5 : FISSION SPECTRUM (X)
C                  I=6 : DIFFUSION COEFFICIENT (D1)
C                  I=7 : DIFFUSION COEFFICIENT (D2)
C                  I=8 : TOTAL (ABSORPTION + SCATTERING + N,2N) OR
C                        TRANSPORT XS (DEPEND ON MEMBER NAME)
C                  I=9 : ACTIVATION XS
C     MINSG      : MIN. GROUP OF UP-SCATTERING (USUALLY MINSG=1)
C     MAXSG      : MAX. GROUP OF DOWN-SCATTERING (USUALLY MAXSG=NG)
C     SCAT(G,G') : SCATTERING MATRIX (G => G')
C     * NEED ONLY WHEN THE 8TH CHARACTER OF MEMNAM .NE. 'Y' OR 'Z'
C        PTAG=0,2,4 ; P0 SCATTERING MATRIX + 2*(N,2N MATRIX) ---*
C        PTAG=1,3   : P1 SCATTERING MATRIX * 3.0                !
C        PTAG=N,M   ; (N,2N) SCATTERING MATRIX                  !
C                     XSEC(G,8) = ABS(G) + SUM-G'(SCAT(G,G'))   !
C                               - SUM-G'(SN2N(G,G'))   <--------*
C                               = ABS(G) + SCAT(G) + N2N(G)
C     DELAY(J,G,I) : DELAYED NEUTRON DATA FOR FAMILY J, ENERGY G
C     * NEED ONLY WHEN THE 8TH CHARACTER OF MEMNAM .EQ. 'Y' OR 'Z'
C                  I=1 : DELAYED BETA(J)*DELAYED-NYU.SIGF(G)
C                  I=2 : DELAYED NEUTRON FISSION SPECTRUM
C                  I=3 : DELAYED BETA(J)*DELAYED-NYU.SIGF(G)/DECAY CNST
C-----------------------------------------------------------------------
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(MACWRT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACWRT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        endif
        STOP
      ENDIF
      IGFLG=1
      IXFLG=0
      PTAG=MEMNAM(8:8)
      IF((PTAG.EQ.'0').OR.(PTAG.EQ.'1')) THEN
        IGFLG=0
      ELSEIF(PTAG.EQ.'Y') THEN
        IXFLG=1
      ELSEIF(PTAG.EQ.'Z') THEN
        IGFLG=0
        IXFLG=1
      ELSEIF(PTAG.EQ.'N') THEN
        IGFLG=0
      ENDIF
***************************
*      ZERO SETTING       *---------------------------------------------
***************************
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
*************************************
* LSS AND LGV FOR SCATTERING MATRIX *-----------------------------------
*************************************
      IF(IXFLG.NE.0) GOTO 200
      IC = 0
      IF(MINSG.GT.1) MINSG = 1
      IF(MAXSG.LT.NG) MAXSG = NG
      MFLG= MINSG-1
      DO 130 IG = 1, NG
        ISTAT = MFLG
        ISTOP = MFLG
        DO 110 IGG = MINSG,MAXSG
          IF((SCAT(IG,IGG).NE.0).AND.(ISTAT.EQ.MFLG)) ISTAT = IGG
          IF((IGG.EQ.IG).AND.(ISTAT.EQ.MFLG)) THEN
            ISTAT = IGG
            ISTOP = IGG
          ENDIF
          IF((SCAT(IG,IGG).NE.0).AND.(ISTAT.NE.MFLG)) ISTOP = IGG
  110   CONTINUE
        IF(ISTOP.LT.IG) ISTOP = IG
        LSS = IG - ISTAT + 1
        LGV = ISTOP - ISTAT + 1
************************
* SET MACRO XS IN FILE *------------------------------------------------
************************
        IWORK(IC+1) = LSS
        IWORK(IC+2) = LGV
        WORK(IC+3)  = XSEC(IG,9)
        WORK(IC+4)  = XSEC(IG,2)
        WORK(IC+5)  = XSEC(IG,1)
        WORK(IC+6)  = XSEC(IG,8)
        WORK(IC+7)  = XSEC(IG,5)
        WORK(IC+8)  = XSEC(IG,6)
        WORK(IC+9)  = XSEC(IG,7)
        WORK(IC+10) = XSEC(IG,4)
        IC = IC + 10
        DO 120 IGG = ISTAT, ISTAT+LGV-1
          IC = IC + 1
          WORK(IC) = SCAT(IG,IGG)
  120   CONTINUE
  130 CONTINUE
      MEMBER=MEMNAM
      LENG = IC
      CALL PDSOUT (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MACWRT): PDSOUT ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACWRT): PDSOUT ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
***************************
*  DELAYED NEUTRON DATA   *---------------------------------------------
***************************
  200 IF(IXFLG.NE.1) GOTO 1000
      SUM = 0
      DO 205 IG=1,NG
        DO 205 J=7,15
          SUM = SUM + DELAY(J,IG,1)+DELAY(J,IG,2)+DELAY(J,IG,3)
  205 CONTINUE
      IF(SUM.EQ.0.0) THEN
        NFAM = 6
      ELSE
        NFAM = 15
      ENDIF
      IC = 0
      DO 210 IG=1,NG
        DO 210 J=1,NFAM
          IC = (IG-1)*NFAM+J
          WORK(IC) = DELAY(J,IG,1)
          WORK(NG*NFAM+IC) = DELAY(J,IG,2)
          WORK(2*NG*NFAM+IC) = DELAY(J,IG,3)
  210 CONTINUE
      MEMBER=MEMNAM
      LENG = 3*NG*NFAM
      CALL PDSOUT (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MACWRT): PDSOUT ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACWRT): PDSOUT ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
***************************
*      PRINT OUT          *---------------------------------------------
***************************
 1000 IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) '   ********** MEMBER NAME = ',MEMNAM, ' **********'
      WRITE(IOUT,*)
      WRITE(IOUT,*) '   NG    = ', NG
      WRITE(IOUT,*) '   MINSG = ', MINSG
      WRITE(IOUT,*) '   MAXSG = ', MAXSG
      IF(IXFLG.EQ.0) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- MACROSCPIC CROSS SECTION ---'
        WRITE(IOUT,*)
        WRITE(IOUT,6000)
        DO 1010 IG = 1, NG
          WRITE(IOUT,6010) IG, (XSEC(IG,IR),IR=1,9)
 1010   CONTINUE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- SCATTERING MATRIX(G=>G'') ---'
        WRITE(IOUT,*)
        WRITE(IOUT,6020) (I,I=MINSG,MAXSG)
        DO 1020 IG = 1, NG
          WRITE(IOUT,6030) IG,(SCAT(IG,IGG),IGG=MINSG,MAXSG)
 1020   CONTINUE
        WRITE(IOUT,*)
      ELSE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- DELAYED NEUTRON DATA (D-PRODUCTION) --- '
        WRITE(IOUT,*)
        WRITE(IOUT,6040)  (J,J=1,NFAM)
        DO 1030 IG = 1, NG
          WRITE(IOUT,6010) IG, (DELAY(J,IG,1),J=1,NFAM)
 1030   CONTINUE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- DELAYED NEUTRON DATA (D-FISS.SPECTR) --- '
        WRITE(IOUT,*)
        WRITE(IOUT,6040)  (J,J=1,NFAM)
        DO 1040 IG = 1, NG
          WRITE(IOUT,6010) IG, (DELAY(J,IG,2),J=1,NFAM)
 1040   CONTINUE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- DELAYED NEUTRON DATA (INV.DECAY CST) --- '
        WRITE(IOUT,*)
        WRITE(IOUT,6040)  (J,J=1,NFAM)
        DO 1050 IG = 1, NG
          WRITE(IOUT,6010) IG, (DELAY(J,IG,3),J=1,NFAM)
 1050   CONTINUE
        WRITE(IOUT,*)
      ENDIF
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 6000 FORMAT(1H ,'   G  ',' PRODUCTION ','  FISSION   ',
     &'  CAPTURE   ',' ABSORPTION ',' FISS.SPCTR ',' DIFFUSION1 ',
     &' DIFFUSION2 ','   TOTAL    ',' ACTIVATION ')
 6010 FORMAT(1H ,I4,2X,1P10E12.5,/,7X,1P10E12.5)
 6020 FORMAT(1H ,7H  G/G' ,10(4X,I4,4X),10(/,8X,10(4X,I4,4X)))
 6030 FORMAT(1H ,I4,2X,1P10E12.5,10(/,7X,1P10E12.5))
 6040 FORMAT(1H ,7H  G/J  ,10(4X,I4,4X),10(/,8X,10(4X,I4,4X)))
 7000 FORMAT(1H ,'MACWRT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'MACWRT')
C
 9000 RETURN
      END

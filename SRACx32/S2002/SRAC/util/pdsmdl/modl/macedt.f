C***********************************************************************
C  PROGRAM TO EDIT MACRO OR MACROWRK PDS FILE DATA                     *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE MACEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, XSEC,
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
      DIMENSION  WSCAT(MAXNG,MAXNG)
      DIMENSION  LLSS(MAXNG), LLGV(MAXNG)
      DIMENSION  IWORK(1)
      EQUIVALENCE ( WORK(1),IWORK(1) )
      CHARACTER*1   PTAG
      CHARACTER* 8  MEMBER
C------------------------------ INPUT ----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/MACRO01
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO EDIT(A8)
C     NG         : NUMBER OF ENERGY GROUPS CORRESPONDING TO INPUT
C                  MACRO MEMBER NAME
C------------------------------ OUTPUT ---------------------------------
C     XSEC(G,I)  : MACROSCOPIC XS OF XS-TYPE:I, ENERGY GROUP:G
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
C     MINSG      : MIN. GROUP OF UP-SCATTERING (ALLOW MINSG.LE.0)
C     MAXSG      : MAX. GROUP OF DOWN-SCATTERING (ALLOW MAXSG.GT.NG)
C     SCAT(G,G') : SCATTERING MATRIX (G => G')
C        PTAG=0,2,4 ; P0 SCATTERING MATRIX + 2*(N,2N MATRIX)
C        PTAG=1,3   : P1 SCATTERING MATRIX * 3.0
C        PTAG=N,M   ; (N,2N) SCATTERING MATRIX
C                     XSEC(G,8) = ABS(G) + SUM-G'(SCAT(G,G'))
C                              - SUM-G'(SN2N(G,G'))
C   DELAY(J,G,I) : DELAYED NEUTRON DATA FOR FAMILY J, ENERGY G
C                  I=1 : DELAYED BETA(J)*DELAYED-NYU.SIGF(G)
C                  I=2 : DELAYED NEUTRON FISSION SPECTRUM
C                  I=3 : DELAYED BETA(J)*DELAYED-NYU.SIGF(G)/DECAY CNST
C-----------------------------------------------------------------------
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(MACEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACEDT): NUMBER OF ENERGY GROUPS(=', NG,
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
*     ZERO SET            *---------------------------------------------
***************************
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
      DO 200 IG = 1, MAXNG
        IF(IXFLG.EQ.1) GOTO 115
        LLSS(IG) = 0
        LLGV(IG) = 0
        DO 110 N = 1, 10
          XSEC(IG,N) = 0.0
  110   CONTINUE
        IF(IXFLG.EQ.0) GOTO 125
  115   DO 120 J = 1, 15
          DELAY(J,IG,1) =0.0
          DELAY(J,IG,2) =0.0
          DELAY(J,IG,3) =0.0
  120   CONTINUE
        IF(IXFLG.EQ.1) GOTO 200
  125   DO 130 IGG = 1, MAXNG
          WSCAT (IG,IGG) = 0.0
  130   CONTINUE
        DO 140 IGG = -MAXUP, MAXNG
          SCAT (IG,IGG) = 0.0
  140   CONTINUE
  200 CONTINUE
***************************
*   MACROSCOPIC XS EDIT   *---------------------------------------------
***************************
      IF(IXFLG.NE.0) GOTO 400
      MEMBER=MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MACEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      IC = 0
      DO 300 IG=1,NG
        DO 310 N=1,10
          XSEC(IG,N) = WORK(IC+N)
  310   CONTINUE
        LSS = IWORK(IC+1)
        LGV = IWORK(IC+2)
        IF ( LGV.EQ.0 ) THEN
          WRITE(IOUT,*) ' CAUTION(MACEDT): LGV=0 FOR IG=', IG
          if(iout.ne.6) then 
          WRITE(6,*)    ' CAUTION(MACEDT): LGV=0 FOR IG=', IG
          endif
        ENDIF
        IF ( LGV.GT.0 ) THEN
          LLSS(IG) = LSS
          LLGV(IG) = LGV
          DO 320 IGG = 1, LGV
            WSCAT(IG,IGG) = WORK(IC+10+IGG)
  320     CONTINUE
        END IF
        IC  = IC + 10 + LGV
  300 CONTINUE
      DO 330 IG=1,NG
        XACT       = XSEC(IG, 3)
        FISS       = XSEC(IG, 4)
        PROD       = XSEC(IG, 5)
        TOTL       = XSEC(IG, 6)
        X          = XSEC(IG, 7)
        D1         = XSEC(IG, 8)
        D2         = XSEC(IG, 9)
        ABSP       = XSEC(IG,10)
        XSEC(IG,1) = PROD
        XSEC(IG,2) = FISS
        XSEC(IG,3) = ABSP - FISS
        XSEC(IG,4) = ABSP
        XSEC(IG,5) = X
        XSEC(IG,6) = D1
        XSEC(IG,7) = D2
        XSEC(IG,8) = TOTL
        XSEC(IG,9) = XACT
  330 CONTINUE
C
C----- SCATTERING MATRIX -----------------------------------------------
C
      MINSG = 1
      MAXSG = NG
      DO 340 IG = 1, NG
        IGSTRT = IG - LLSS(IG) + 1
        IGSTOP = IG + LLGV(IG) - LLSS(IG)
        IF(IGSTRT.LT.MINSG) MINSG = IGSTRT
        IF(IGSTOP.GT.MAXSG) MAXSG = IGSTOP
        DO 350 IGG = 1, LLGV(IG)
          SCAT(IG,IGSTRT+IGG-1) = WSCAT(IG,IGG)
  350   CONTINUE
  340 CONTINUE
***************************
*  DELAYED NEUTRON DATA   *---------------------------------------------
***************************
  400 IF(IXFLG.NE.1) GOTO 1000
      MEMBER=MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MACEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NFAM = LENG/NG/3
      IF(NFAM.NE.6.AND.NFAM.NE.15) THEN
        WRITE(IOUT,*) ' ERROR(MACEDT): NUMBER OF DELAYED NEUTRON ',
     &                'FAMILY(=',NFAM,') IS INVALID'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MACEDT): NUMBER OF DELAYED NEUTRON ',
     &                'FAMILY(=',NFAM,') IS INVALID'
        endif
        STOP
      ENDIF
      DO 410 IG=1,NG
        DO 410 J=1,NFAM
          IC=(IG-1)*NFAM+J
          DELAY(J,IG,1) = WORK(IC)
          DELAY(J,IG,2) = WORK(NG*NFAM+IC)
          DELAY(J,IG,3) = WORK(2*NG*NFAM+IC)
  410   CONTINUE
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
        WRITE(IOUT,6040) (J,J=1,NFAM)
        DO 1030 IG = 1, NG
          WRITE(IOUT,6010) IG, (DELAY(J,IG,1),J=1,NFAM)
 1030   CONTINUE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- DELAYED NEUTRON DATA (D-FISS.SPECTR) --- '
        WRITE(IOUT,*)
        WRITE(IOUT,6040) (J,J=1,NFAM)
        DO 1040 IG = 1, NG
          WRITE(IOUT,6010) IG, (DELAY(J,IG,2),J=1,NFAM)
 1040   CONTINUE
        WRITE(IOUT,*)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '   --- DELAYED NEUTRON DATA (INV.DECAY CST) --- '
        WRITE(IOUT,*)
        WRITE(IOUT,6040) (J,J=1,NFAM)
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
 7000 FORMAT(1H ,'MACEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'MACEDT')
C
 9000 RETURN
      END

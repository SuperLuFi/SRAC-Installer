C***********************************************************************
C ANISNXS : MAIN PROGRAM TO FORM ANISN,MORSE,GMVP TYPE BINARY XS
C           LIBRARY FROM SRAC-PDS FILES
C***********************************************************************
C### COPYED FROM ENGEDT/MACEDT
      INCLUDE 'PARAMINC'
      CHARACTER*72  DIRNAM
      CHARACTER*8   MEMNAM
      DIMENSION  EN(MAXNG+1), WT(MAXNG+1)
      DIMENSION  XSEC(MAXNG,MAXXS)
      DIMENSION  SCAT(MAXNG,-MAXUP:MAXNG)
      DIMENSION  DELAY(15,MAXNG,3)
C***********************************************************************
C  XSM(g,1,L,m)  : absorption XS of m-th material (L-1 order)
C  XSM(g,2,L,m)  : production
C  XSM(g,3,L,m)  : total/transport
C  SCM(g,g',L,m) : scattering matrix (g->g') of m-th material
C  XKAI(g,m)     : fission spectrum
C-----------------------------------------------------------------------
      PARAMETER    (MXMT=50, MXPL=1, MXWK=1000000)
      DIMENSION    IDM(MXMT)
      DIMENSION    XSM(MAXNG,3,MXPL+1,MXMT)
      DIMENSION    SCM(MAXNG,MAXNG,MXPL+1,MXMT)
      DIMENSION    XKAI(MAXNG,MXMT), WORK(MXWK), IWORK(MXWK)
      EQUIVALENCE  (WORK(1),IWORK(1))
      CHARACTER*8  MEMBER(MXMT)
      CHARACTER*48 TITLE
      CHARACTER*1  PTAG(6,2), ETAG
C***********************************************************************
C     If you change I/O device number, 
C     Change subroutine (uiount) at the last.
      NIN  = 5
      NOUT1= 6
      NOUT2= 99
      NBIN = 1
      IOUT = NOUT1
C
      IPRN   = 1
      NOTE   = 0
C
      CALL UIOSET
C***********************************************************************
C LOGO PRINT (99)
C***********************************************************************
      WRITE(NOUT1,*) ' ********************************************'
      WRITE(NOUT1,*) ' SRAC UTILITY TO CONVERT MACROSCOPIC XS DATA'
      WRITE(NOUT1,*) ' OF PDS TO ANISN TYPE BINARY LIBRARY DATA'
      WRITE(NOUT1,*) ' ********************************************'
      WRITE(NOUT2,*) ' ********************************************'
      WRITE(NOUT2,*) ' SRAC UTILITY TO CONVERT MACROSCOPIC XS DATA'
      WRITE(NOUT2,*) ' OF PDS TO ANISN TYPE BINARY LIBRARY DATA'
      WRITE(NOUT2,*) ' ********************************************'
      WRITE(NOUT2,*) ' THE BINARY DATA IS AVAILABLE IN ANISN, TWOTRAN',
     &               ' GMVP, MORSE, ETC.'
      WRITE(NOUT2,*) ' NOTE: ANISN FORMAT DOSE NOT INCLUDE MATERIAL-',
     &               ' DEPENDENT FISSION SPECTRA.'
      WRITE(NOUT2,*) ' USE THE PRINTED FISSION SPECTRA IF NECESSARY.'
      WRITE(NOUT2,*)
C
C***********************************************************************
C SET PL-TAG OF SRAC MEMBERS IN MACRO(1) OR MACROWRK(2)
C (SET INITIAL CHARACTER DATA)
C***********************************************************************
      TITLE ='                                                '
      PTAG(1,1) = '0'
      PTAG(2,1) = '1'
      PTAG(3,1) = 'X'
      PTAG(4,1) = 'X'
      PTAG(5,1) = 'X'
      PTAG(6,1) = 'X'
C
      PTAG(1,2) = '4'
      PTAG(2,2) = '3'
      PTAG(3,2) = '5'
      PTAG(4,2) = '6'
      PTAG(5,2) = '7'
      PTAG(6,2) = '8'
C***********************************************************************
C READ DIRECTORY NAME OF MACRO/MACROWRK
C Check MACRO or MACROWRK
C READ ENERGY GROUP STRUCTURE FROM CONTA00[0,2]
C***********************************************************************
C     IMAC=1 : MACRO
C         =2 : MACROWRK
      READ(NIN,'(A72)',END=9999) DIRNAM
      MEMNAM = 'CONTA000'
      CALL PDSIN(DIRNAM,MEMNAM,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.0) THEN
        IMAC=1
        ETAG='A'
        GOTO 100
      ENDIF
C
      MEMNAM = 'CONTA002'
      CALL PDSIN(DIRNAM,MEMNAM,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.0) THEN
        IMAC=2
        ETAG='A'
        GOTO 100
      ENDIF
C
      MEMNAM = 'CONTF000'
      CALL PDSIN(DIRNAM,MEMNAM,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.0) THEN
        IMAC=1
        ETAG='F'
        GOTO 100
      ENDIF
C
      MEMNAM = 'CONTF002'
      CALL PDSIN(DIRNAM,MEMNAM,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.0) THEN
        IMAC=2
        ETAG='F'
        GOTO 100
      ENDIF
C
      WRITE(NOUT1,*) ' ERROR : PDSIN FAILED, IRC=',IRC
      STOP 999
  100 CONTINUE
      CALL ENGEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
      WRITE(NOUT2,*)  ' NUMBER OF ENERGY GROUPS          = ', NG
      WRITE(NOUT2,*)
      WRITE(NOUT2,*) ' << ENERGY BOUNDARY OF MACROSCOPIC XS >>'
      WRITE(NOUT2,*)
      WRITE(NOUT2,6000) (EN(IG),IG=1,NG+1)
      WRITE(NOUT2,*)
C
C***********************************************************************
C READ PL ORDER AND Monte Carlo Option
C MCOPT = 0 : accept negative XS (caused by transport correction)
C       = 1 : not accept negative scattering XS
C             SIGT = SIGT + ABS(SIGS) and SIGS=0
C MSAVE = 0 : down-scattering size is forced to be NG-1 (suggested)
C       = 1 : down-scattering size is searched (additional input NDS1
C             is necessary in GMVP (output library may be not available
C             in some codes (ex. MORSE)
C***********************************************************************
      READ(NIN,*) NPL, MCOPT, IDEBUG, MSAVE
      WRITE(NOUT2,*)  ' INPUT PL ORDER (NPL)             = ', NPL
      WRITE(NOUT2,*)  ' OPTION FOR NEGATIVE XS           = ', MCOPT
      WRITE(NOUT2,*)  ' OPTION FOR DEBUG PRINT           = ', IDEBUG
      WRITE(NOUT2,*)  ' OPTION FOR MEMORY SAVVING        = ', MSAVE
      IF(NPL.LT.0) THEN
        WRITE(NOUT1,*) ' ERROR: INPUT PL-OREDER(=',NPL,') IS INVALID'
      ENDIF
      IF(MCOPT.NE.0) MCOPT=1
      IF(NPL.GT.MXPL) THEN
        WRITE(NOUT1,*) ' ERROR: INPUT PL-OREDER IS GREATER THAN',
     &                ' PROGRAM ARRAY SIZE (=',MXPL,')'
        STOP 777
      ENDIF
      IF(NPL.EQ.0 .AND. IMAC.EQ.2) THEN
        PTAG(1,2) = '2'
      ENDIF
      NPL1 = NPL+1
C
C***********************************************************************
C READ MEMBERS (MATERIALS)
C***********************************************************************
      NMAT = 0
  200 READ(NIN,'(A8,I10)',END=210) MEMNAM, IDUM
      IF(MEMNAM.EQ.'        ') GOTO 210
      NMAT = NMAT + 1
      IF(NMAT.GT.MXMT) THEN
        WRITE(NOUT1,*) ' ERROR: NUMBER OF INPUT MEMBERS IS ',
     &          ' GREATER THAN PROGRAM ARRAY SIZE (=',MXMT,')'
        STOP 777
      ENDIF
      MEMBER(NMAT)=MEMNAM
      IDM(NMAT) = IDUM
      GOTO 200
  210 CONTINUE
C
C***********************************************************************
C SEARCH MAX UP-SCATTERING AND MAX DOWN-SCATTERIG SIZES
C AMONG MEMBERS
C***********************************************************************
      MXUPS = 0
      MXDWS = 0
      DO 300 M=1,NMAT
        DO 310 L=1,NPL1
          MEMNAM = MEMBER(M)
          MEMNAM(5:5)=ETAG
          MEMNAM(8:8)=PTAG(L,IMAC)
          CALL PDSIN(DIRNAM,MEMNAM,WORK,LENG,IRC,IOUT)
          IF(IRC.NE.0) THEN
            WRITE(NOUT1,*) ' ERROR : PDSIN FAILED, IRC=',IRC
            STOP 999
          ENDIF
          IPOS = 0
          DO 320 IG=1,NG
            LSS = IWORK(IPOS+1)
            LGV = IWORK(IPOS+2)
            LUP = LSS-1
            LDW = LGV-LSS
            MXUPS = MAX(LUP,MXUPS)
            MXDWS = MAX(LDW,MXDWS)
            IPOS = IPOS + 10 + LGV
  320     CONTINUE
  310   CONTINUE
  300 CONTINUE
      IF(MSAVE.EQ.0) THEN
        WRITE(NOUT2,*) ' REAL MAX. SIZE OF DOWN-SCATTERING (',MXDWS,
     &                 ') WAS REPLACED BY ',NG-1
        MXDWS = NG - 1
      ENDIF
      WRITE(NOUT2,*) ' MAX. SIZE OF UP-SCATTERING       = ', MXUPS
      WRITE(NOUT2,*) ' MAX. SIZE OF DOWN-SCATTERING     = ', MXDWS
C
C***********************************************************************
C READ MEMBER XS AND SET IT IN ANISN FORMAT
C***********************************************************************
C     IHT : position of total cross section in a group XS data
C     ISGG: position of self-scattering in a group XS data
C     ITBL: length of a group XS data
C     LSCT: length of a scattering data in a group
C     LENG: record length of all group XS data
C     NDS1: size of down-scattering + 1(self-scattering)
C
      IHT  = 3
      ISGG = IHT + MXUPS + 1
      ITBL = ISGG + MXDWS
      LSCT = MXUPS + 1 + MXDWS
      LENG = NG*ITBL
      NDS1 = MXDWS + 1
      WRITE(NOUT2,*) ' SIZE OF SCATTERING VECTOR        = ', LSCT
      WRITE(NOUT2,*) ' IHT : POSITION OF TOTAL XS       = ', IHT
      WRITE(NOUT2,*) ' ISGG: POSITION OF SELF-SCATTERNG = ', ISGG
      WRITE(NOUT2,*) ' ITBL: LENGTH OF A GROUP XS DATA  = ', ITBL
      WRITE(NOUT2,*) ' NDS1: LENGTH OF DOWN+SELF SCAT.  = ', NDS1
C
C----- MACEDT ARRANGEMENT ------------------------------
C     XSEX(g,1): production
C     XSEX(g,2): fission
C     XSEX(g,3): capture defined as (absorption - fission)
C     XSEX(g,4): absorption
C     XSEX(g,5): fission spectrum
C     XSEX(g,6): diffusion coefficient (D1)
C     XSEX(g,7): diffusion coefficient (D2)
C     XSEX(g,8): total or transport
C     XSEX(g,9): velocity cross section
C     SCAT(g,g'): full size of scattering matrix (g=>g')
C----- FOR ANISN FORMAT
C  XSM(g,1,L,m)  : absorption XS of m-th material (L-1 order)
C  XSM(g,2,L,m)  : production
C  XSM(g,3,L,m)  : total/transport
C  SCM(g,g',L,m) : scattering matrix (g->g') of m-th material
C
C----- Sample when NG=9 --------------------------------------
C      1   2  IHT,           [MXUPS]    ISGG    [MXDWS]  ITBL
C g=1  Ag, Pg, Tg,     0 .....3->1 2->1 1->1  0 0 0 ....... 0
C g=2  Ag, Pg, Tg,     0 0 ...4->2 3->2 2->2  1->2  ....... 0
C g=3  Ag, Pg, Tg,     0 0 0 0 ....4->3 3->3  2->3  1->3 .. 0
C  :    :   :   :        :        :       :    :      :     :
C  :    :   :   :        :        :       :    :      :     :
C g=9  Ag, Pg, Tg,     0 0 0 0 ......0  9->9  8->9  7->9 ....
C-------------------------------------------------------------
C
      WRITE(NOUT2,*)
      DO 1000 M=1,NMAT
        WRITE(NOUT2,*)
        WRITE(NOUT2,*) ' ******************************'
        WRITE(NOUT2,*) '  INPUT MEMBER NAME = ',MEMBER(M)
        WRITE(NOUT2,*) '  INPUT MATERIAL ID = ',IDM(M)
        WRITE(NOUT2,*) ' ******************************'
        WRITE(NOUT2,*)
        DO 1100 L=1,NPL1
          MEMNAM = MEMBER(M)
          MEMNAM(5:5)=ETAG
          MEMNAM(8:8)=PTAG(L,IMAC)
          CALL MACEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, XSEC,
     &                MINSG, MAXSG, SCAT, DELAY)
C
          IF(IDEBUG.EQ.1) THEN
            WRITE(NOUT2,*)
            WRITE(NOUT2,*) ' << MEMBER NAME OF THE PL(=',L-1,
     &                          ') COMPONENT : ',MEMNAM,' >>'
            WRITE(NOUT2,*)
          ENDIF
          TITLE(1:8)=MEMNAM
          WRITE(NBIN) NG,ITBL,L-1,IDM(M),TITLE
C
          IF(L.EQ.1) THEN
            DO 400 IG=1,NG
              XKAI(IG,M)= XSEC(IG,5)
  400       CONTINUE
            WRITE(NOUT2,*) ' << MATERIAL DEPENDENT FISSION SPECTRUM >>'
            WRITE(NOUT2,6000) (XKAI(IG,M),IG=1,NG)
            WRITE(NOUT2,*)
          ENDIF
C***********************************************************************
          DO 410 IG=1,NG
            XSM(IG,1,L,M) = XSEC(IG,4)
            XSM(IG,2,L,M) = XSEC(IG,1)
            XSM(IG,3,L,M) = XSEC(IG,8)
C-----------Check Negative XS
            IF(L.EQ.1) THEN
              IF(XSM(IG,1,L,M).LT.0.0) THEN
                WRITE(NOUT2,6110) 'ABSORPTION',IG
                NOTE = NOTE + 1
              ENDIF
              IF(XSM(IG,2,L,M).LT.0.0) THEN
                WRITE(NOUT2,6110) 'PRODUCTION',IG
                NOTE = NOTE + 1
              ENDIF
              IF(XSM(IG,3,L,M).LT.0.0) THEN
                WRITE(NOUT2,6110) 'TOTAL(TRA)',IG
                NOTE = NOTE + 1
              ENDIF
            ENDIF
            SUM = 0.0
            DO 420 IGG=1,NG
              SCM(IG,IGG,L,M) = SCAT(IG,IGG)
C-----------Check Negative Scattering XS
              IF(L.EQ.1) THEN
                IF(SCM(IG,IGG,L,M).LT.0.0) THEN
                  WRITE(NOUT2,6120) 'SCATTERING',IG,IGG
                  NOTE = NOTE + 1
                  IF(MCOPT.NE.0) THEN
                    SUM = SUM + ABS(SCM(IG,IGG,L,M))
                    SCM(IG,IGG,L,M) = 0.0
                  ENDIF
                ENDIF
              ENDIF
  420       CONTINUE
            IF(L.EQ.1 .AND. SUM.NE.0) THEN
              XSM(IG,3,L,M) = XSM(IG,3,L,M) + SUM
              WRITE(NOUT2,6130) SUM, IG
              NOTE = NOTE + 1
            ENDIF
  410     CONTINUE
C--------- set one group XS data(one record) in WORK dimension
C         (Note : loop on sink group)
          IPOS = 0
          DO 430 IGG=1,NG
            WORK(IPOS+1) = XSM(IGG,1,L,M)
            WORK(IPOS+2) = XSM(IGG,2,L,M)
            WORK(IPOS+3) = XSM(IGG,3,L,M)
            IPOS = IPOS + 3
C
C-----------SET SCATTERIG XS (LSCT=MXUPS+1+MXDWS)
            DO 440 K=1,LSCT
              IG = IGG + MXUPS + 1 - K
              IF(IG.LE.NG .AND. IG.GE.1) THEN
                 WORK(IPOS+K)=SCM(IG,IGG,L,M)
              ELSE
                 WORK(IPOS+K)=0.0
              ENDIF
  440       CONTINUE
            IPOS = IPOS + LSCT
            IF(IDEBUG.EQ.1) THEN
              ITMP = IPOS - (LSCT+3) + 1
              WRITE(NOUT2,6200) IGG,(WORK(I),I=ITMP,IPOS)
            ENDIF
  430     CONTINUE
C
          WRITE(NBIN) (WORK(I),I=1,IPOS)
 1100   CONTINUE
 1000 CONTINUE
      IF(NOTE.NE.0) THEN
        WRITE(NOUT2,*)
        WRITE(NOUT2,*) ' THERE ARE ',NOTE,' WARNING MESSAGES MARKED ',
     &                 'BY (!!! WARNING:) '
        WRITE(NOUT2,*)
      ENDIF
      WRITE(NOUT2,*)
      WRITE(NOUT2,*) ' ================ NORMAL END ==================='
C
C***********************************************************************
 6000 FORMAT(3X,1P5E13.5)
 6110 FORMAT(1X,'!!! WARNING: NEGATIVE ',A,'XS WAS DETECTED IN GROUP',
     &       I3 )
 6120 FORMAT(1X,'!!! WARNING: NEGATIVE ',A,'XS WAS DETECTED : ',
     &       'FROM GROUP ',I3,' TO GROUP ',I3 )
 6130 FORMAT(1X,'!!! WARNING: NEGATIVE SCATTERIG XS (=',1PE12.5,
     &       ') WAS SET TO ZERO.',/,
     &       '     IT WAS ADDED TO TOTAL(TRANSPORT) XS OF GROUP ',I3)
 6200 FORMAT(1X,'IGG=',I3,2X,1P220E12.5:/(10X,1P220E12.5:))
 9999 STOP
      END
C***********************************************************************
C
C  uiount   : set unformated(0) or formated(1)
C             for each I/O device
C
C***********************************************************************
C
      subroutine uiount(ioform)
      dimension ioform(100)
      do 100 i=1,100
        ioform(i) = -1
  100 continue
      ioform(1)  = 0
      ioform(5)  = 1
      ioform(6)  = 1
      ioform(99) = 1
      return
      end

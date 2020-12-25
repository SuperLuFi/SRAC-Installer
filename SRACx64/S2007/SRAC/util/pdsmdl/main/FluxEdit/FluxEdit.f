C***********************************************************************
C
C  MAIN PROGRAM FOR PDSMDL
C  FLUXEDIT : PRINT OUT FLUX*VOL AND VOL
C
C***********************************************************************
C### COPYED FROM ENGEDT/FLXEDT
      INCLUDE 'PARAMINC'
      CHARACTER*72  DIRNAM 
      CHARACTER*8   MEMNAM
      DIMENSION  EN(MAXNG+1), WT(MAXNG+1)
      DIMENSION  FLX1(MAXNG,MAXNR), VOLR(MAXNR)
C***********************************************************************
      CHARACTER*8 MEMBER
C
      call uioset
C---- read directory name for FLUX
      read(5,'(a72)',end=9999) dirnam
C---- LOOP ON MEMBER
 1000 READ(5,'(A8)',END=9999 ) MEMBER
      IF(MEMBER.EQ.'        ') GOTO 9999
      IF(MEMBER(6:8).EQ.'VOL') GOTO 2000
C
C---- READ NUMBER OF ENEGY GROUPS
      MEMNAM = 'CONTA000'
      MEMNAM(5:5) = MEMBER(5:5)
      MEMNAM(8:8) = MEMBER(8:8)
      IOUT = 99
      IPRN = 0
      CALL ENGEDT(dirnam, IOUT, IPRN, MEMNAM, NG, WT, EN)
C
C---- READ FLUX*VOL OR VOL AND PRINT OUT
 2000 MEMNAM = MEMBER
      IOUT = 6
      IPRN = 1
      CALL FLXEDT(dirnam, IOUT, IPRN, MEMNAM, NG, NRR, FLX1, VOLR)
      GOTO 1000
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
      ioform(5)  = 1
      ioform(6)  = 1
      ioform(99) = 1
      return
      end

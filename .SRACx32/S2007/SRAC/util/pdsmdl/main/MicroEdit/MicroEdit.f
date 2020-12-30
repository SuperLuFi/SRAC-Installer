C***********************************************************************
C
C  main program for pdsmdl
C  MairoEdit : Print out Micro Xs
C
C***********************************************************************
C### COPYED FROM MICEDT 
      INCLUDE 'PARAMINC'
      DIMENSION  XSEC(MAXNG,MAXXS)
      CHARACTER*72  DIRNAM
      CHARACTER* 8  MICNM
C***********************************************************************
      character *8 member
C
      call uioset
      iout = 6
C---- read directory name for MICREF
      read(5,'(a72)',end=9999) dirnam
C---- loop on member
 1000 read(5,'(a8)',end=9999 ) member
      if(member.eq.'        ') goto 9999
C
C---- read micro xs and print out
      MICNM = member
      iprn = 1
      call MICEDT(DIRNAM, IOUT, IPRN, MICNM, NG, NGF, XSEC)
      goto 1000
 9999 stop
      end
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
      return
      end

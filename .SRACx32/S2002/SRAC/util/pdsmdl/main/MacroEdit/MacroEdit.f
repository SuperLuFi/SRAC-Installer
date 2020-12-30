C***********************************************************************
C
C  main program for pdsmdl
C  MacroEdit : Print out Macro Xs Table
C
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
      character*8 member
C
      call uioset
C---- read directory name for MACRO/MACROWRK
      read(5,'(a72)',end=9999) dirnam
C---- loop on member
 1000 read(5,'(a8)',end=9999 ) member
      if(member.eq.'        ') goto 9999
C
C---- read number of enegy groups
      memnam = 'CONTA000'
      memnam(5:5) = member(5:5)
      if(member(8:8).eq.'2') memnam(8:8)='2'
      if(member(8:8).eq.'3') memnam(8:8)='2'
      if(member(8:8).eq.'4') memnam(8:8)='2'
      if(member(8:8).eq.'Y') memnam(8:8)='2'
      if(member(8:8).eq.'M') memnam(8:8)='2'
      iout = 99
      iprn = 0
      call ENGEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
C
C---- read macro xs and print out
      memnam = member
      iout = 6
      iprn = 1
      call MACEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, XSEC,
     &                  MINSG, MAXSG, SCAT, DELAY)  
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
      ioform(99) = 1
      return
      end

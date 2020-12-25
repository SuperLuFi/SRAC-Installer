C***********************************************************************
C  PROGRAM TO REWRITE ENERGY GROUP STRUCTURE TO MEMBER CONT_00_        *
C  IN FLUX OR MACROWRK OR MACRO OR MICREF FILE                         *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE ENGWRT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
C
C=========================== FOR MAIN ==================================
CDEL  PARAMETER  (MAXNG=107)
      INCLUDE  'PARAMINC'
      CHARACTER*72  DIRNAM
      CHARACTER*8   MEMNAM
      DIMENSION  EN(MAXNG+1), WT(MAXNG+1)
C=======================================================================
CDEL  PARAMETER  (MAXWRK=250)
      COMMON /WKPDS/ WORK(MAXWRK)  
      DIMENSION      IWORK(1)
      EQUIVALENCE  (WORK(1),IWORK(1))
      CHARACTER*8   MEMBER
C-------------------------------INPUT-----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/FLUX01
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO WRITE(A8)
C     NG         : NUMBER OF ENERGY GROUPS
C     WT(G)      : INTEGRATED ASYMPTOTIC NEUTRON SPECTRUM (G=1,NG)
C                  WT(GC)=SUM(WT(GF))
C     EN(G)      : ENERGY BOUNDARIES STARTING FROM THE HIGHEST ENERGY
C                  GROUP (G=1,NG+1)
C-----------------------------------------------------------------------
*****************************
* SIZE CHECK & ZERO SETTING *-------------------------------------------
*****************************
      LENG = 2*NG+2
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(ENGWRT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(ENGWRT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        endif
        STOP
      ENDIF
      IF (LENG.GT.MAXWRK) THEN
        WRITE(IOUT,*) ' ERROR(ENGWRT):REQUIRED WORK SIZE(=', LENG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXWRK, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(ENGWRT):REQUIRED WORK SIZE(=', LENG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXWRK, ')'
        endif
        STOP
      ENDIF
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
***************************
* WRITE ENERGY STRUCTURE  *--------------------------------------------
***************************
      IWORK(1) = NG
      DO 200 I=1,NG
        WORK(I+1) = WT(I)
  200 CONTINUE
      DO 210 I=1,NG+1
        WORK(NG+1+I) = EN(I)
  210 CONTINUE
      MEMBER = MEMNAM
      CALL PDSOUT (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(ENGWRT): PDSOUT ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(ENGWRT): PDSOUT ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
***************************
*     PRINT OUT           *--------------------------------------------
***************************
      IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' NUMBER OF ENERGY GROUPS (MEMBER:',MEMNAM,')= ',NG
      WRITE(IOUT,*)
      WRITE(IOUT,6000)
      WRITE(IOUT,*)
        DO 1000 IG = 1, NG+1
          WRITE(IOUT,6100) IG, WT(IG), EN(IG)
 1000   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 7000 FORMAT(1H ,'ENGWRT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'ENGWRT')
 6000 FORMAT(1H ,4H  G ,2X,14HASYMPTOTIC FLX,2X,16HENERGY LIMIT(EV))
 6100 FORMAT(1H ,I4,3X,1PE12.5,3X,1PE12.5)
C
 9000 RETURN
      END

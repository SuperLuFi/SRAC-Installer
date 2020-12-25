C***********************************************************************
C  PROGRAM TO OUTPUT ENERGY GROUP STRUCTURE FROM MEMBER CONT_00_       *
C  IN FLUX OR MACROWRK OR MACRO OR MICREF FILE                         *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE ENGEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
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
C     MEMNAM     : PDS MEMBER NAME TO EDIT(A8)
C-------------------------------OUTPUT----------------------------------
C     NG         : NUMBER OF ENERGY GROUPS CORRESPONDING TO INPUT
C                  MEMBER NAME FOR ENENRGY STRUCTURE (CONTE00P).
C     WT(G)      : INTEGRATED ASYMPTOTIC NEUTRON SPECTRUM (G=1,NG)
C                  WT(GC)=SUM(WT(GF))
C     EN(G)      : ENERGY BOUNDARIES STARTING FROM THE HIGHEST ENERGY
C                  GROUP (G=1,NG+1)
C-----------------------------------------------------------------------
***************************
*      ZERO SETTING       *---------------------------------------------
***************************
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
      DO 110 I=1,MAXNG+1
        EN(I)=0.0
        WT(I)=0.0
  110 CONTINUE
***************************
* ENERGY GROUP STRUCTURE  *--------------------------------------------
***************************
      MEMBER = MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(ENGEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(ENGEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NG = IWORK(1)
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(ENGEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(ENGEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        endif
        STOP
      ENDIF
      DO 200 I=1,NG
        WT(I) = WORK(I+1)
  200 CONTINUE
      DO 210 I=1,NG+1
        EN(I) = WORK(NG+1+I)
  210 CONTINUE
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
 6000 FORMAT(1H ,4H  G ,2X,14HASYMPTOTIC FLX,2X,16HENERGY LIMIT(EV))
 6100 FORMAT(1H ,I4,3X,1PE12.5,3X,1PE12.5)
 7000 FORMAT(1H ,'ENGEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'ENGEDT')
C
 9000 RETURN
      END

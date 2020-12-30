C***********************************************************************
C  PROGRAM TO OUTPUT ENERGY GROUP STRUCTURE OF FASTP LIB.              *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE FASTLB(DIRNAM, IOUT, IPRN, MEMNAM,
     &                  NGFP, NGFP1, NGFP2, NGFP3, WT, EN)
C
C=========================== FOR MAIN ==================================
CDEL  PARAMETER  (MAXNG=74)
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
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/PFASTJ32
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO EDIT(A8)
C-------------------------------OUTPUT----------------------------------
C     NGFP       : NUMBER OF FAST ENERGY GROUPS IN PUBLIC LIB. =74
C     NGFP1      : THE LOWEST GROUP NUMBER OF FAST RANGE(1MEV)
C     NGFP2      : THE LOWEST GROUP NUMBER OF SMOOTH RANGE(50KEV)
C     NGFP3      : THE LOWEST GROUP NUMBER OF RESONANCE2 RANGE(130.07EV)
C     WT(G)      : INTEGRATED ASYMPTOTIC NEUTRON SPECTRUM (G=1,NGF)
C                  WT(GC)=SUM(WT(GF))
C     EN(G)      : ENERGY BOUNDARIES STARTING FROM THE HIGHEST ENERGY
C                  GROUP (G=1,NGF+1)
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
        WRITE(IOUT,*) ' ERROR(FASTLB): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(FASTLB): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NGFP  = IWORK(1)
      NGFP1 = IWORK(2)
      NGFP2 = IWORK(3)
      NGFP3 = IWORK(4)
      DO 200 I=1,NGFP
        WT(I) = WORK(I+4)
  200 CONTINUE
      DO 210 I=1,NGFP+1
        EN(I) = WORK(NGFP+4+I)
  210 CONTINUE
***************************
*     PRINT OUT           *--------------------------------------------
***************************
      IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' NGFP  = ',NGFP
      WRITE(IOUT,*) ' NGFP1 = ',NGFP1
      WRITE(IOUT,*) ' NGFP2 = ',NGFP2
      WRITE(IOUT,*) ' NGFP3 = ',NGFP3
      WRITE(IOUT,*)
      WRITE(IOUT,6000)
      WRITE(IOUT,*)
        DO 1000 IG = 1, NGFP+1
          WRITE(IOUT,6100) IG, WT(IG), EN(IG)
 1000   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 6000 FORMAT(1H ,4H  G ,2X,14HASYMPTOTIC FLX,2X,16HENERGY LIMIT(EV))
 6100 FORMAT(1H ,I4,3X,1PE12.5,3X,1PE12.5)
 7000 FORMAT(1H ,'FASTLB',114(1H=))
 7010 FORMAT(1H ,114(1H=),'FASTLB')
C
 9000 RETURN
      END

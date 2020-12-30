#ifndef NO_WALLCLOCK_C

/******************************************************************
 *  Fortran called C subroutine :
 *
 *   UNIX venders have their own convention or rule for external
 *  names of FORTRAN coded subprograms or commons.
 *
 *   We prepare equivalent sources for C coded routines
 * 
 *     1. Attach underscore to FORTRAN name. ( SUN & Other BSD UNIX)
 *     2. Same as FORTRAN name (lowercase).   (HP-UX,IBM-AIX etc.)
 *     3. Uppercase string of FORTRAN name.   ( Ncube etc. )
 *     4. "_" + uppercase string of FORTRAN name.   ( HI/OSF )
 *
 *******************************************************************/

/*******************************************************************
 *
 *    Wall clock time since midnight January 1, 1970. ;
 *
 *   Using BSD library function "gettimeofday" supported on most
 *  UNIX.
 *    This clock has a nominal precision of microsecond but
 *  the actual precision depends on settings of users system.
 *
 *    Programmed by M.Sasaki (the Japan Research Institute Co. Ltd.)
 *    
 *    1st version:  6 Oct 1993
 *
 *   * works on HP-UX, SunOS, DEC/OSF, Sysytem_V etc. 
 *
 ******************************************************************/

#include <stdio.h>
#include <sys/types.h>

#include <sys/time.h>

void wallclock( ttt )
     double * ttt ;
{
     struct timeval  tv;
     struct timezone  tz;
     gettimeofday( &tv, &tz ) ;
     *ttt = (double)tv.tv_sec + (double)(tv.tv_usec)/1000000 ;
}

void wallclock_( ttt )
     double * ttt ;
{
     struct timeval  tv;
     struct timezone  tz;
     gettimeofday( &tv, &tz ) ;
     *ttt = (double)tv.tv_sec + (double)(tv.tv_usec)/1000000 ;
}

void WALLCLOCK( ttt )
     double * ttt ;
{
     struct timeval  tv;
     struct timezone  tz;
     gettimeofday( &tv, &tz ) ;
     *ttt = (double)tv.tv_sec + (double)(tv.tv_usec)/1000000 ;
}

void _WALLCLOCK( ttt )
     double * ttt ;
{
     struct timeval  tv;
     struct timezone  tz;
     gettimeofday( &tv, &tz ) ;
     *ttt = (double)tv.tv_sec + (double)(tv.tv_usec)/1000000 ;
}

/******************
main()
{
     double t0, t1 ;
     tokei( &t0 ) ;

     while( getchar() != EOF )
     {
          tokei( &t1 ) ;
          printf("%lf \n",t1-t0) ;
     }
}
 ******************/

#endif

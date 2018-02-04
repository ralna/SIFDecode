! THIS VERSION: SIFDECODE 1.1 - 21/10/2015 AT 12:00 GMT.

!-*-*-*-*-*-*-*-*-  S I F D E C O D E  _ m a i n   P R O G R A M  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as part of CUTE, December 1990
!   Became separate program as part of SifDec, April 2004
!   Updated fortran 2003 version released December 2012

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

      PROGRAM SIFDECODE_main

!  ------------------------------------------------------------------------
!  this is the main program for running the SIF decoder for the GALAHAD and
!  CUTEst optimization packages. It calls the driver routine SDLANC which does
!  all the work. The purpose of this main program is to open and close all
!  files, and to care for the proper filenames when possible
!  ------------------------------------------------------------------------

      USE SIFDECODE
      IMPLICIT NONE

!  local variables

      INTEGER :: i, ad0, auto, print_level, status, algorithm, size, start
      LOGICAL :: single

!  assign the standard output unit numbers

      INTEGER, PARAMETER :: in = 51
      INTEGER, PARAMETER :: out = 6
      CHARACTER ( LEN = 10 ) :: pbname = REPEAT( ' ', 10 )
      CHARACTER ( LEN = 10 ) :: prb = REPEAT( ' ', 10 )
      CHARACTER ( LEN = 80 ) :: prbdat  = REPEAT( ' ', 80 )

!  assign the remaining i/o unit numbers

      INTEGER, PARAMETER :: ingps = 61
      INTEGER, PARAMETER :: infn = ingps
      INTEGER, PARAMETER :: ingr = ingps
      INTEGER, PARAMETER :: inex = ingps

      INTEGER, PARAMETER :: outda = 55
      INTEGER, PARAMETER :: outfn = 52
      INTEGER, PARAMETER :: outra = 53
      INTEGER, PARAMETER :: outgr = 54
      INTEGER, PARAMETER :: outex = 57
      INTEGER, PARAMETER :: outfd = 59
      INTEGER, PARAMETER :: outea = 66
      INTEGER, PARAMETER :: outgd = 63
      INTEGER, PARAMETER :: outem = 67
      INTEGER, PARAMETER :: outff = 0
      INTEGER, PARAMETER :: outgf = 0

!  assign file names

      CHARACTER ( LEN = 24 ), PARAMETER :: prbin  = 'SIFDECODE.CNF          '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbout = 'OUTSDIF.d              '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbfn  = 'ELFUN.f                '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbff  = 'ELFUNF.f               '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbfd  = 'ELFUND.f               '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbra  = 'RANGE.f                '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbgr  = 'GROUP.f                '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbgf  = 'GROUPF.f               '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbgd  = 'GROUPD.f               '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbet  = 'SETTYP.f               '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbex  = 'EXTER.f                '
      CHARACTER ( LEN = 24 ), PARAMETER :: prbea  = 'EXTERA.f               '

      LOGICAL, PARAMETER :: noname = .FALSE.

!  read the problem's name, build default file names and assign
!  the actual values used

      OPEN ( in, FILE = prbin, FORM = 'FORMATTED', STATUS = 'OLD')
      READ ( in, "( A10 )" ) prb
      prbdat = TRIM( prb ) // '.SIF'

!  specify the method to be used (1=SBMIN, 2=AUGLG, 3=BARIA).

      READ( in, "( I2 )" ) algorithm

!  specify whether the problem should be described(<0=DEBUG, 0=NO, >0=YES)

      READ( in, "( I6 )" ) print_level

!  read the actual problem name and use it for initial output

      READ( in, "( A10 )" ) pbname
      WRITE( out, "( /, ' Problem name: ', A )" ) TRIM( pbname )

!  specify whether the derivatives are supplied or are to be computed
!  using automatic differentiation

      READ( in, "( I2 )" ) auto

!  specify whether AD01 or AD02 should be used to perform the
!  automatic differentiation

      READ( in, "( I2 )" ) ad0

!  specify the precision of the output files (single=0,double=1)

      READ( in, "( I2 )" ) i
      single = i == 0

!  specify the "size" of the problem (1=small,2=medium,3=large). This value
!  is simply used to set initial array sizes; incorrect values will be
!  increased, so the parameter is simply a convenience for larger problems

      READ( in, "( I2 )" ) size

!  specify which of the starting points provided is to be used; 
!  if start > # starting vectors, start will be reset to 1

      READ( in, "( I2 )" ) start
      CLOSE( in )

!  open the relevant files - unix systems

      OPEN( ingps, FILE = prbdat, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND ingps
      OPEN( outda, FILE = prbout, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND outda
      OPEN( outra, FILE = prbra,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND outra
      OPEN( outex, FILE = prbex,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND OUTEX
      IF ( auto == 0 ) THEN
        OPEN( outfn, FILE = prbfn,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND OUTFN
        OPEN( outgr, FILE = prbgr,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outgr
      ELSE
        IF ( outff > 0 ) THEN
          OPEN( outff, FILE = prbff,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
          REWIND outff
        END IF
        OPEN( outfd, FILE = prbfd,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outfd
        IF ( outgf > 0 ) THEN
          OPEN( outgf, FILE = prbgf,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
          REWIND outgf
        END IF
        OPEN( outgd, FILE = prbgd,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outgd
        OPEN( outea, FILE = prbea,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outea
      END IF
      OPEN( UNIT = outem )

!  decode the problem

      CALL SIFDECODE_decode( ingps, outda, infn, outfn, outff, outfd, outra,   &
                             ingr, outgr, outgf, outgd, inex, outex, outem,    &
                             outea, print_level, out, noname, algorithm,       &
                             auto, ad0, single, size, start, status )

!  close the opened files

      CLOSE( ingps )
      IF ( status == 0 ) THEN
        WRITE( out, "( ' File successfully decoded' )" )
        CLOSE( outda )
        CLOSE( outra )
        CLOSE( outex )
        IF ( auto == 0 ) THEN
          CLOSE( outfn )
          CLOSE( outgr )
        ELSE
          IF ( outff > 0 ) CLOSE( outff )
          CLOSE( outfd )
          IF ( outgf > 0 ) CLOSE( outgf )
          CLOSE( outgd )
          CLOSE( outea )
        END IF

!  if an error has been discovered, delete the output files

      ELSE
        WRITE( out, "( ' Decoding failure, status = ', I0 )" ) status
        CLOSE( outda, STATUS = 'DELETE' )
        CLOSE( outra, STATUS = 'DELETE' )
        IF ( auto == 0 ) THEN
          CLOSE( outfn, STATUS = 'DELETE' )
          CLOSE( outgr, STATUS = 'DELETE' )
          CLOSE( outex, STATUS = 'DELETE' )
        ELSE
          IF ( outff > 0 ) CLOSE( outff, STATUS = 'DELETE' )
          CLOSE( outfd, STATUS = 'DELETE' )
          IF ( outgf > 0 ) CLOSE( outgf, STATUS = 'DELETE' )
          CLOSE( outgd, STATUS = 'DELETE' )
          CLOSE( outea, STATUS = 'DELETE' )
        END IF
      END IF
      CLOSE( outem, STATUS = 'DELETE' )
      STOP

!  end of program SIFDEC

      END PROGRAM SIFDECODE_main

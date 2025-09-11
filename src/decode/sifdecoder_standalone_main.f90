! THIS VERSION: SIFDECODE 2.6 - 2025-09-11 AT 15:00 GMT.

!-*-*-*-*-*-*-*-*-  S I F D E C O D E R _ m a i n   P R O G R A M  -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   Original version SIFDECODE 2.4, August 11th, 2024

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

      PROGRAM SIFDECODER_standalone_main

!  ------------------------------------------------------------------------
!  this is the main program for running the SIF decoder for the GALAHAD and
!  CUTEst optimization packages. It calls the driver routine SIFDECDE which 
!  does all the work. The purpose of this main program is to open and close
!  all files, and to care for the proper filenames when possible
!  ------------------------------------------------------------------------

      USE SIFDECODER_standalone
      IMPLICIT NONE

!  local variables

      INTEGER :: l, argnum, narg, status
      INTEGER, PARAMETER :: len_param = 200
      LOGICAL :: file_exists
      CHARACTER ( LEN = len_param ) :: argval

!  default values

      INTEGER :: out = 6
      INTEGER :: size = 3
      INTEGER :: realpr = 64
      INTEGER :: start = 1
      INTEGER :: package = 3
      INTEGER :: output_level = 0
      INTEGER :: auto = 0
      INTEGER :: deriv_check = 0
      INTEGER :: add_suffix = 0
      INTEGER :: show_params = 0
      INTEGER :: force = 0
      CHARACTER ( LEN = len_param ) :: param_list = REPEAT( ' ', len_param )
      CHARACTER ( LEN = 1000 ) :: sifname = REPEAT( ' ', 1000 )

!  read and process command-line options

      narg = COMMAND_ARGUMENT_COUNT( )

!  if there are no arguments, print help information and stop

      IF ( narg == 0 ) THEN
        WRITE( 6, 2000 )
        WRITE( 6, 2010 )
        WRITE( 6, 2020 )
        WRITE( 6, "( /, '  ** No arguments provided, stopping **' )" )
        STOP
      END IF

!  loop over arguments

      argnum = 1
      DO WHILE ( argnum <= narg )
        CALL GET_COMMAND_ARGUMENT( argnum, argval )
        argnum = argnum + 1
        SELECT CASE ( argval )
        CASE( '-h', '--help' )
          WRITE( 6, 2000 )
          WRITE( 6, 2010 )
          WRITE( 6, 2020 )
          STOP
        CASE( '-sp' )
           realpr = 32
        CASE( '-dp' )
           realpr = 64
        CASE( '-qp' )
           realpr = 128
        CASE( '-o' )
          CALL GET_COMMAND_ARGUMENT( argnum, argval )
          argnum = argnum + 1
          READ( argval, * ) output_level
        CASE( '-c' )
          deriv_check = 1
        CASE( '-f' )
          auto = 1
        CASE( '-b' )
          auto = 2
        CASE( '-p' )
          CALL GET_COMMAND_ARGUMENT( argnum, argval )
          argnum = argnum + 1
          READ( argval, * ) package
        CASE( '-s' )
          CALL GET_COMMAND_ARGUMENT( argnum, argval )
          argnum = argnum + 1
          READ( argval, * ) size
        CASE( '-st' )
          CALL GET_COMMAND_ARGUMENT( argnum, argval )
          argnum = argnum + 1
          READ( argval, * ) start
        CASE( '-show' )
          show_params = 1
        CASE( '-suffix' )
          add_suffix = 1
        CASE( '-param' )
          CALL GET_COMMAND_ARGUMENT( argnum, argval )
          argnum = argnum + 1
          READ( argval, "( A )" ) param_list
        CASE( '-force' )
          force = 1

!  if an argument is unrecognised, stop unless it is the last argument 
!  and the name of a file

        CASE DEFAULT
          IF ( argnum == narg + 1 ) THEN
            l = LEN( TRIM( argval ) )
            sifname = TRIM( argval )
            IF ( sifname( l - 3 : l ) /= '.SIF' )                              &
              sifname( l + 1 : l + 4 ) = '.SIF'
            INQUIRE( FILE = TRIM( sifname ), EXIST = file_exists )
            IF ( .NOT. file_exists ) THEN
              WRITE( 6, "( ' SIF file ', A, ' does not exist' )" )             &
                TRIM( sifname )
              STOP
            END IF
          ELSE
            WRITE( 6, "( ' Unrecognised command line argument: ', A )" )       &
               TRIM( argval )
            STOP
          END IF
        END SELECT 
      END DO

!  record the problem name

      IF ( sifname( l - 3 : l ) /= '.SIF' ) THEN
        WRITE( 6, "( /, ' Problem name: ', A )" ) sifname( 1 : l )
      ELSE
        WRITE( 6, "( /, ' Problem name: ', A )" ) sifname( 1 : l - 4 )
      END IF

      CALL SIFDECODER_decode( sifname, status, out = out, size = size,         &
                              realpr = realpr, start = start,                  &
                              auto = auto, package = package,                  &
                              output_level = output_level,                     &
                              deriv_check = deriv_check,                       &
                              add_suffix = add_suffix,                         &
                              show_params = show_params,                       &
                              param_list = param_list, force = force )

!  non-executable statements

 2000 FORMAT(                                                                  &
      ' Decode a standard input format (SIF) file. Use:', /,                   &
      '', /,                                                                   &
      '  sifdecoder_standalone [-sp] [-dp] [-qp] [-h] [-o 0|1] [-f] [-b]', /,  &
      '    [-p 1|2|3] [-s 0|1|2|3] [-st 1|2|3] [-show] [-suffix]', /,          &
      '    [-force] [-param name=value[,name=value...]] problem[.SIF]', /,     &
      '', /,                                                                   &
      ' where', /,                                                             &
       '', /,                                                                  &
       '  -sp  Decode the problem for use with the single precision', /,       &
       '       tools. The default is to decode the problem for use with', /,   &
       '       the double precision tools. This will add a suffix _s to', /,   &
       '       names of all generated fortran files.', /,                      &
       '', /,                                                                  &
       '  -dp  Decode the problem for use with the double precision', /,       &
       '       tools. This is the default.', /,                                &
       '', /,                                                                  &
       '  -qp  Decode the problem for use with the quadruple precision', /,    &
       '       tools. The default is to decode the problem for use with', /,   &
       '       the double precision tools. This will add a suffix _q to', /,   &
       '       names of all generated fortran files.', /,                      &
       '', /,                                                                  &
       '  -h, --help   Print this short help message and stop.' )
 2010 FORMAT(                                                                  &
       '  -o 0|1', /,                                                          &
       '       Regulate the output level of sifdecoder. Verbose  mode', /,     &
       '       is -o 1, silent mode is -o 0. Silent mode is the default.', /,  &
       '', /,                                                                  &
       '  -f   Use automatic differentiation with HSL_AD02 in Forward mode.',  &
       /, '', /,                                                               &
       '  -b   Use automatic differentiation with HSL_AD02 in Backward mode.', &
       /, '', /,                                                               &
       '  -p 1|2|3', /,                                                        &
       '       Specifies the package that the decoded problem is ', /,         &
       '       intended for: -p 1 is for LANCELOT, -p 2 is for BARIA and', /,  &
       '       -p 3 is for CUTEst. The default is to decode for CUTEst.' )
!      '  -c   check the derivatves that are provided in ELFUN.f and', /,      &
!      '       GROUP.f by comparing the values against finite-diffrence', /,   &
!      '       approximations; any significant differences will be', /,        &
!      '       reported. N.B. currently not supported as standalone.', /,      &
!      '', /,                                                                  &
  2020 FORMAT(                                                                 &
       '  -s 0|1|2|3', /,                                                      &
       '       Specify the rough size of problem that will be  decoded. ', /,  &
       '       This  is used  for  array  initialization  and', /,             &
       '       although not crucial, it may lead to efficiencies if', /,       &
       '       set correctly. Set -s 0 is for  debugging, -s  1  is ', /,      &
       '       for small problems of up to approximately 100', /,              &
       '       variables and constraints, -s 2 is for medium-sized', /,        &
       '       problems of up to approximately 10000 variables and ', /,       &
       '       constraints, and -s 3 is for larger problems. Setting ', /,     &
       '       -s too large may cause memory  allocation errors on ', /,       &
       '       modest  computers. The default is for large problems.', /,      &
       '', /,                                                                  &
       '  -st 1|2|3', /,                                                       &
       '       Specifies the starting point vector to be used if there ', /,   &
       '       is  more than one.  Any value outside the actual number', /,    &
       '       of starting vectors will be interpreted as 1, and this is', /,  &
       '       is the default.', /,                                            &
       '', /,                                                                  &
       '  -show', /,                                                           &
       '       displays possible parameter settings for problem[.SIF]', /,     &
       '       and stop.  Other options are ignored.', /,                      &
       '', /,                                                                  &
       '  -suffix', /,                                                         &
       '       add _problem as a suffix to all files generated, where', /,     &
       '       problem is the name of the SIF problem.', /,                    &
       '', /,                                                                  &
       '  -param', /,                                                          &
       '       Cast problem[.SIF] against explicit parameter settings. ', /,   &
       '       Several parameter settings may be given as a comma-', /,        &
       '       -separated list following -param. Use', /,                      &
       '          sifdecoder_standalone -show problem', /,                     &
       '       to view possible settings. If a setting is not allowed in', /,  &
       '       the SIF file, no action is taken unless -force is present.', /, &
       '', /,                                                                  &
       '  -force', /,                                                          &
       '       Forces the setting of the parameters named using -param', /,    &
       '       to  the given  values, even if those values are not', /,        &
       '       predefined in the SIF file.', /,                                &
       '', /,                                                                  &
       '  problem[.SIF]', /,                                                   &
       '       problem.SIF is the name of the file containing the SIF ', /,    &
       '       information on the problem to be solved.' )


      END PROGRAM SIFDECODER_standalone_main

# 1 "ompgen.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "ompgen.F90"
MODULE omp_constants

    USE omp_lib

    INTEGER, PARAMETER :: fomp_sched_kind = 4
    INTEGER, PARAMETER :: fomp_lock_kind = 8
    INTEGER, PARAMETER :: fomp_nest_lock_kind = 8
    INTEGER(KIND=4), PARAMETER :: fomp_sched_static = 1
    INTEGER(KIND=4), PARAMETER :: fomp_sched_dynamic = 2
    INTEGER(KIND=4), PARAMETER :: fomp_sched_guided = 3
    INTEGER(KIND=4), PARAMETER :: fomp_sched_auto = 4

# 22 "ompgen.F90"

END MODULE omp_constants


FUNCTION fomp_enabled()

    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL :: fomp_enabled


    fomp_enabled = .TRUE.




END FUNCTION fomp_enabled


SUBROUTINE fomp_set_num_threads(num_threads)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER, INTENT(IN) :: num_threads


    CALL omp_set_num_threads(num_threads)





END SUBROUTINE fomp_set_num_threads


FUNCTION fomp_get_num_threads()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_num_threads


    fomp_get_num_threads = omp_get_num_threads()




END FUNCTION fomp_get_num_threads


FUNCTION fomp_get_max_threads()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_max_threads


    fomp_get_max_threads = omp_get_max_threads()




END FUNCTION fomp_get_max_threads


FUNCTION fomp_get_thread_num()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_thread_num


    fomp_get_thread_num = omp_get_thread_num()




END FUNCTION fomp_get_thread_num


FUNCTION fomp_get_num_procs()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_num_procs


    fomp_get_num_procs = omp_get_num_procs()




END FUNCTION fomp_get_num_procs


FUNCTION fomp_in_parallel()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL :: fomp_in_parallel


    fomp_in_parallel = omp_in_parallel()




END FUNCTION fomp_in_parallel


SUBROUTINE fomp_set_dynamic(dynamic_threads)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL, INTENT(IN) :: dynamic_threads


    CALL omp_set_dynamic(dynamic_threads)




END SUBROUTINE fomp_set_dynamic


FUNCTION fomp_get_dynamic()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL :: fomp_get_dynamic


    fomp_get_dynamic = omp_get_dynamic()




END FUNCTION fomp_get_dynamic


SUBROUTINE fomp_set_nested(nested)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL, INTENT(IN) :: nested


    CALL omp_set_nested(nested)




END SUBROUTINE fomp_set_nested


FUNCTION fomp_get_nested()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL :: fomp_get_nested


    fomp_get_nested = omp_get_nested()




END FUNCTION fomp_get_nested


SUBROUTINE fomp_set_schedule(kind, modifier)

    USE omp_lib

    USE omp_constants, ONLY : fomp_sched_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_sched_kind), INTENT(IN) :: kind
    INTEGER, INTENT(IN) :: modifier


    CALL omp_set_schedule(kind, modifier)




END SUBROUTINE fomp_set_schedule


SUBROUTINE fomp_get_schedule(kind, modifier)

    USE omp_lib

    USE omp_constants, ONLY : fomp_sched_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_sched_kind), INTENT(OUT) :: kind
    INTEGER, INTENT(OUT) :: modifier


    CALL omp_get_schedule(kind, modifier)





END SUBROUTINE fomp_get_schedule


FUNCTION fomp_get_thread_limit()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_thread_limit


    fomp_get_thread_limit = omp_get_thread_limit()




END FUNCTION fomp_get_thread_limit


SUBROUTINE fomp_set_max_active_levels(max_levels)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER, INTENT(IN) :: max_levels


    CALL omp_set_max_active_levels(max_levels)




END SUBROUTINE fomp_set_max_active_levels


FUNCTION fomp_get_max_active_levels()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_max_active_levels


    fomp_get_max_active_levels = omp_get_max_active_levels()




END FUNCTION fomp_get_max_active_levels


FUNCTION fomp_get_level()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_level


    fomp_get_level = omp_get_level()




END FUNCTION fomp_get_level


FUNCTION fomp_get_ancestor_thread_num(level)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER, INTENT(IN) :: level
    INTEGER :: fomp_get_ancestor_thread_num


    fomp_get_ancestor_thread_num = omp_get_ancestor_thread_num(level)





END FUNCTION fomp_get_ancestor_thread_num


FUNCTION fomp_get_team_size(level)

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER, INTENT(IN) :: level
    INTEGER :: fomp_get_team_size


    fomp_get_team_size = omp_get_team_size(level)





END FUNCTION fomp_get_team_size


FUNCTION fomp_get_active_level()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    INTEGER :: fomp_get_active_level


    fomp_get_active_level = omp_get_active_level()




END FUNCTION fomp_get_active_level


FUNCTION fomp_in_final()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    LOGICAL :: fomp_in_final


    fomp_in_final = omp_in_final()




END FUNCTION fomp_in_final


SUBROUTINE fomp_init_lock(svar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_lock_kind), INTENT(OUT) :: svar


    CALL omp_init_lock(svar)




END SUBROUTINE fomp_init_lock


SUBROUTINE fomp_init_nest_lock(nvar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_nest_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_nest_lock_kind), INTENT(OUT) :: nvar


    CALL omp_init_nest_lock(nvar)




END SUBROUTINE fomp_init_nest_lock


SUBROUTINE fomp_destroy_lock(svar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_lock_kind), INTENT(INOUT) :: svar


    CALL omp_destroy_lock(svar)





END SUBROUTINE fomp_destroy_lock


SUBROUTINE fomp_destroy_nest_lock(nvar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_nest_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_nest_lock_kind), INTENT(INOUT) :: nvar


    CALL omp_destroy_nest_lock(nvar)




END SUBROUTINE fomp_destroy_nest_lock


SUBROUTINE fomp_set_lock(svar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_lock_kind), INTENT(INOUT) :: svar


    CALL omp_set_lock(svar)




END SUBROUTINE fomp_set_lock


SUBROUTINE fomp_set_nest_lock(nvar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_nest_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_nest_lock_kind), INTENT(INOUT) :: nvar


    CALL omp_set_nest_lock(nvar)




END SUBROUTINE fomp_set_nest_lock


SUBROUTINE fomp_unset_lock(svar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_lock_kind), INTENT(INOUT) :: svar


    CALL omp_unset_lock(svar)




END SUBROUTINE fomp_unset_lock


SUBROUTINE fomp_unset_nest_lock(nvar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_nest_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_nest_lock_kind), INTENT(INOUT) :: nvar


    CALL omp_unset_nest_lock(nvar)




END SUBROUTINE fomp_unset_nest_lock


FUNCTION fomp_test_lock(svar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_lock_kind), INTENT(INOUT) :: svar
    LOGICAL :: fomp_test_lock


    fomp_test_lock =  omp_test_lock(svar)







END FUNCTION fomp_test_lock


FUNCTION fomp_test_nest_lock(nvar)

    USE omp_lib

    USE omp_constants, ONLY : fomp_nest_lock_kind

    IMPLICIT NONE

    !f2py threadsafe

    INTEGER(KIND=fomp_nest_lock_kind), INTENT(INOUT) :: nvar
    INTEGER :: fomp_test_nest_lock


    fomp_test_nest_lock =  omp_test_nest_lock(nvar)







END FUNCTION fomp_test_nest_lock


FUNCTION fomp_get_wtime()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    REAL (KIND=8) :: fomp_get_wtime


    fomp_get_wtime =  omp_get_wtime()






END FUNCTION fomp_get_wtime


FUNCTION fomp_get_wtick()

    USE omp_lib


    IMPLICIT NONE

    !f2py threadsafe

    REAL (KIND=8) :: fomp_get_wtick


    fomp_get_wtick =  omp_get_wtick()






END FUNCTION fomp_get_wtick



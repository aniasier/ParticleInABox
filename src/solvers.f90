module solvers
    use constants
    implicit none
! interface do dsyev !!!!
    interface
        subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info) bind(C,name="dsyev_")
            import :: dp
            character(len=1), intent(in) :: jobz, uplo
            integer, intent(in) :: n, lda, lwork
            real(dp), intent(inout) :: a(lda,*)
            real(dp), intent(out) :: w(*), work(*)
            integer, intent(out) :: info
        end subroutine
    end interface



contains
    subroutine solve_eigenproblem(H, w, num)
        real(dp), intent(inout) :: H(:,:)
        real(dp), intent(out) :: w(:)
        integer, intent(in) :: num
        real(dp), allocatable :: work(:)
        integer :: lwork, info

        lwork = -1
        allocate(work(1))
        call dsyev('V', 'U', num, H, num, w, work, lwork, info)

        if (info /= 0) then
            print *, "Błąd przy query workspace, info=", info
            stop
        end if

        lwork = int(work(1))
        deallocate(work)
        allocate(work(lwork))

        call dsyev('V', 'U', num, H, num, w, work, lwork, info)

        if ( info /= 0) then
            print *, 'Błąd, info=', info
        end if
    end subroutine
end module solvers
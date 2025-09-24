module hamiltonian
    use constants
    implicit none
    
contains
    subroutine get_hamiltonian(H, m, dx, num)
        real(dp), intent(out) :: H(:,:)
        real(dp), intent(in) :: m, dx
        integer, intent(in) :: num
        
        integer :: i
        real(dp) :: t

        H = 0.0_dp
        t = -1.0_dp / (2.0_dp * m * dx**2)

        do i = 1, num
            H(i,i) = -2.0_dp * t
            if ( i > 1 ) then
                H(i,i-1) = t
                H(i-1,i) = t
            end if
        end do
    end subroutine
end module hamiltonian
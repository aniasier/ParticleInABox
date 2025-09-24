program particle_in_a_box
    use constants
    use hamiltonian
    use solvers
    implicit none

    integer, parameter :: num = 100
    real(dp), allocatable :: H(:,:), w(:)
    real(dp) :: m, width, dx

    allocate(H(num,num),w(num))

    m = 1.17_dp
    width = 10.0 * nm2au
    dx = width / num

    call get_hamiltonian(H, m, dx, num)
    call solve_eigenproblem(H, w, num)

    print *, "Pierwsze 10 energii (eV):"
    print '(10f12.6)', w(1:min(10,num)) / eV2au
    
end program particle_in_a_box
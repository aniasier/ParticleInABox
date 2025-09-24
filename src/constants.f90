module constants
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none

    real(dp), parameter :: eV2au = 0.03674932587122423_dp
    real(dp), parameter :: nm2au = 18.89726133921252_dp
    real(dp), parameter :: T2au = 4.254382E-6_dp

end module constants
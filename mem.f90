module memory
  implicit none

  !GLOBAl

  integer, parameter :: n_max = 1500
  real :: t(5000), ke(5000)


  !for standing wave
  ! real, parameter :: x_min = 0.
  ! real, parameter :: x_max = 1.
  ! real, parameter :: tmin = 0.
  ! real, parameter :: tmax = 5.
  ! integer, parameter :: out_n = 100
  ! real, parameter :: gamma = 1

  !for shock tube
  ! real, parameter :: x_min = -1.5
  ! real, parameter :: x_max = 0.5
  ! real, parameter :: tmin = 0.
  ! real, parameter :: tmax = 0.2
  ! integer, parameter :: out_n = 550
  ! real, parameter :: gamma = 1


  !for sod shock tube
  real, parameter :: x_min = -1.5
  real, parameter :: x_max = 0.5
  real, parameter :: tmin = 0.
  real, parameter :: tmax = 0.2
  integer, parameter :: out_n = 1500
  real, parameter :: gamma = 1.4


end module memory

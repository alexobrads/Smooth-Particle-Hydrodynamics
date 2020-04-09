module memory
  implicit none

  !GLOBAl   !always have these defined
  integer, parameter :: n_max = 1500
  real :: t(5000), ke(5000)



!__________________________________________________________________________________
  ! !for standing wave QUESTIONS 1

  real, parameter :: problem = 0.9!dont change this, picks relivant setup for problem

  !setup parameters
  real, parameter :: x_min = 0.
  real, parameter :: x_max = 1.
  real, parameter :: tmin = 0.
  real, parameter :: tmax = 5.2
  !1 for isothermal case
  real, parameter :: gamma = 1

  !viscosity_yes_or_no equals 1 for viscoity to work, equals 0 for no viscoity
  !please dont use any other value
  real, parameter :: viscosity_yes_or_no = 0.
  real, parameter :: alpha = 1.
  real, parameter :: beta = 2.

  !equals 1 for variable smoothing length to work, equals 0 for no varience
  !please dont use any other value
  real, parameter :: var_smooth_yes_or_no = 0.
!__________________________________________________________________________________





!__________________________________________________________________________________
  !for shock tube
  ! real, parameter :: problem = 2.1!dont change this, picks setup for problem
  ! !setup parameters
  !
  ! real, parameter :: x_min = -1.
  ! real, parameter :: x_max = 1 +9e-3!small correction to range please pretend you didnt see
  ! real, parameter :: tmin = 0.
  ! real, parameter :: tmax = 0.1
  ! !1 for isthermal case
  ! real, parameter :: gamma = 1
  !
  ! !equals 1 for viscoity to work, equals 0 for no viscoity
  ! !please dont use any other value
  ! real, parameter :: viscosity_yes_or_no = 1.
  ! real, parameter :: alpha = 1.
  ! real, parameter :: beta = 2.
  !
  ! !equals 1 for variable smoothing length to work, equals 0 for no varience
  ! !please dont use any other value
  ! real, parameter :: var_smooth_yes_or_no = 1.
!__________________________________________________________________________________




!__________________________________________________________________________________
  ! !for sod shock tube
  ! real, parameter :: problem = 3.!dont change this, picks setup for problem
  ! !setup parameters
  ! real, parameter :: x_min = -1.
  ! real, parameter :: x_max = 1-9.999e-4!small correction to range please pretend you didnt see
  ! real, parameter :: tmin = 0.
  ! real, parameter :: tmax = 0.2
  ! !1.4 for adiabatic case
  ! real, parameter :: gamma = 1.4
  !
  ! !equals 1 for viscoity to work, equals 0 for no viscoity
  ! !please dont use any other value
  ! real, parameter :: viscosity_yes_or_no = 1.
  ! real, parameter :: alpha = 1.
  ! real, parameter :: beta = 2.
  !
  ! !equals 1 for variable smoothing length to work, equals 0 for no varience
  ! !please dont use any other value
  ! real, parameter :: var_smooth_yes_or_no = 1.
!__________________________________________________________________________________



end module memory

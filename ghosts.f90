module ghosts
  use memory
  implicit none

contains

  subroutine set_ghosts(blob, n, ng)

    real, intent(inout) :: blob(:,:)
    integer, intent(inout) :: n, ng
    real, parameter :: radkern = 2.
    real :: range
    integer :: i

    range = x_max - x_min

    ng = 0
    do i = 1,n

      if (blob(i,1) + radkern*blob(i,4)>x_max) then
        ng = ng + 1
        blob(n+ng, :) = blob(i, :)
        blob(n+ng, 1) = blob(i, 1) - range


      else if (blob(i,1) - radkern*blob(i,4)<x_min) then
        ng = ng + 1
        blob(n+ng, :) = blob(i, :)
        blob(n+ng, 1) = blob(i, 1) + range

      endif

    enddo

  end subroutine set_ghosts

end module ghosts

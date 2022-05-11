!>時空間離散化に関する手続を提供する．
!>
!>手続には，離散化する軸に分割幅の情報を渡すための手続が含まれる．
!>
module discretization_op_intervals
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: intervals

contains
    !>分割後の微小空間の幅を受け取り，そのまま返す．
    function intervals(interval) result(new_interval)
        implicit none

        real(real64), intent(in) :: interval
            !! 分割後の微小空間の幅

        real(real64) :: new_interval
            !! 分割後の微小空間の幅

        new_interval = interval
    end function intervals
end module discretization_op_intervals

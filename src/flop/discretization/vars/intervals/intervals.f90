!>時空間離散化に関する派生型や手続を提供する．
!>
!>派生型には，微小区間の幅を表現する派生型が含まれる．
!>
!>手続には，微小区間の幅を表現する派生型のコンストラクタが含まれる．
!>
module discretization_vars_intervals
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Intervals

    !>微小区間の間隔を表現する派生型
    type, public :: intervals_type
        real(real64), allocatable :: intervals(:)
            !! 微小区間の間隔
    end type intervals_type

    !>微小区間の間隔を表現する派生型に対するコンストラクタを
    !>`Intervals`と呼べるようにするためのインタフェース
    interface Intervals
        procedure :: construct_interval
    end interface

contains
    !>分割後の微小空間の幅を受け取り，微小区間の幅を返す．
    function construct_interval(interval) result(new_intervals)
        implicit none

        real(real64), intent(in) :: interval
            !! 分割後の微小空間の幅

        type(intervals_type) :: new_intervals
            !! 分割後の微小空間の幅

        allocate (new_intervals%intervals(1), source=interval)
    end function construct_interval

    !>分割後の微小空間の幅を受け取り，そのまま返す．
    function construct_intervals(intervals) result(new_intervals)
        implicit none

        real(real64), intent(in) :: intervals(:)
            !! 分割後の微小空間の幅

        type(intervals_type) :: new_intervals
            !! 分割後の微小空間の幅

        new_intervals%intervals = intervals
    end function construct_intervals
end module discretization_vars_intervals

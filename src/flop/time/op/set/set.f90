!>時間軸に関する手続を提供する．
!>
!>手続には，開始・終了時間から時間軸を構築する手続が含まれる．
!>
module time_op_set
    use, intrinsic :: iso_fortran_env
    use :: time_vars_axis
    implicit none
    private
    public :: operator(.set.)

    !>ユーザ定義演算子`.set.`を定義するインタフェース
    interface operator(.set.)
        procedure :: set_period
    end interface

contains
    !>開始時間と終了時間を軸に渡し，それらを反映した軸を返す．
    function set_period(axis, period_val) result(new_axis)
        implicit none
        !&<
        type(time_axis_type), intent(in) :: axis
            !! 時間軸
        real(real64)        , intent(in) :: period_val(:)
            !! 開始時間と終了時間
        !&>
        type(time_axis_type) :: new_axis
            !! 構築される時間軸

        call new_axis%construct(period_val)

        return
        if (same_type_as(axis, axis)) continue ! 変数未使用警告の抑制
    end function set_period
end module time_op_set

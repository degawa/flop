!>時間離散化に関する派生型を提供する．
!>
!>派生型には，離散化する時間軸と時間軸を分割する微小区間の幅を
!>まとめて取り扱うための派生型が含まれる．
!>
!>@note
!>モジュールおよび派生型の名前は，演算子指向で書いた時間離散化の処理
!>
!>```Fortran
!>stability_conditions = stability_conditions .set. Courant(grid, U_wall, 0.1d0) &
!>                                            .set. Diffusion(grid, kvisc, 0.5d0)
!>dt = .stabilize.(dt .by. stability_conditions)
!>delta_t = .divide. (t.into.intervals(dt))
!>```
!>
!>に現れる2項演算`t.into.intervals(dt)`に由来．
!>@endnote
!>
module discretization_op_vars_time_intoIntervals
    use :: time_vars_axis
    use :: discretization_vars_intervals
    implicit none
    private

    !>離散化する時間軸と軸方向の微小区間の幅を
    !>まとめて取り扱うための型．<br>
    !>名前は2項演算`t .into. intervals(dt)`に由来．
    type, public :: time_into_intervals_type
        type(time_axis_type) :: time
            !! 離散化する時間軸
        type(intervals_type) :: intervals
            !! 時間軸の微小区間の幅
    end type time_into_intervals_type
end module discretization_op_vars_time_intoIntervals

!>計算の安定化に関係する手続を提供する．
!>
!>手続には，安定化される値と複数の安定条件をまとめる手続が含まれる．
!>
module incompressible_op_by
    use, intrinsic :: iso_fortran_env
    use :: incompressible_vars_stability_conditions
    use :: incompressible_op_vars_stabilizer_stabilizing
    implicit none
    private
    public :: operator(.by.)

    !>ユーザ定義演算子`.by.`を定義するインタフェース
    interface operator(.by.)
        procedure :: set_stability_conditions
    end interface

contains
    !>安定条件を扱う型と安定化される値をまとめてを返す．
    function set_stability_conditions(dt, stability_conditions) result(new_dt_stab)
        implicit none
        !&<
        real(real64)                    , intent(in) :: dt
            !! 安定化される計算時間間隔
        type(stability_conditions_type) , intent(in) :: stability_conditions
            !! 安定条件
        !&>
        type(dt_by_stability_conditions_type) :: new_dt_stab
            !! 安定化されるパラメータと安定条件

        new_dt_stab%dt = dt
        allocate (new_dt_stab%stability_conditions, source=stability_conditions)
    end function set_stability_conditions
end module incompressible_op_by

!>計算の安定化に関係する派生型を提供する．
!>
!>派生型には，安定化される値と安定条件をまとめて取り扱う派生型が含まれる．
!>
module incompressible_op_vars_stabilizer_stabilizing
    use, intrinsic :: iso_fortran_env
    use :: incompressible_vars_stability_conditions
    implicit none
    private

    !>安定化される値と安定条件をまとめて取り扱う派生型．<br>
    !>名前は2項演算`dt .by. stability_conditions`に由来．
    type, public :: dt_by_stability_conditions_type
        real(real64) :: dt
            !! 計算時間間隔
        type(stability_conditions_type), allocatable :: stability_conditions
            !! 安定条件
    end type dt_by_stability_conditions_type
end module incompressible_op_vars_stabilizer_stabilizing

!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型および手続を提供する．
!>
!>派生型には，連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)を
!>求解するCG法の仕様を表す派生型が含まれる．
!>
!>手続には，CG法の仕様のコンストラクタが含まれる．
!>
module grid_uniform_stg_op_cust_linEqs_vars_solver_spec_cg
    use :: grid_uniform_stg_op_cust_linEqs_vars_solver_spec_adt
    implicit none
    private
    public :: CG

    !>ソルバの仕様を表す型．
    type, public, extends(solver_spec_atype) :: CG_spec_type
    end type CG_spec_type

    !>`CG_spec_type`のコンストラクタを`CG()`と呼ぶためのインタフェース．
    interface CG
        procedure :: construct_CG_spec
    end interface
contains
    !>`CG_spec_type`を返す．
    function construct_CG_spec() result(new_cg_spec)
        implicit none
        type(CG_spec_type) :: new_cg_spec
            !! 新しく設定された仕様

        return
        ! 未使用警告の抑制
        if (same_type_as(new_cg_spec, new_cg_spec)) continue
    end function construct_CG_spec
end module grid_uniform_stg_op_cust_linEqs_vars_solver_spec_cg

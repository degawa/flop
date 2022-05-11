!>Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!>に関する派生型を提供する．
!>
!>派生型には，ナブラ演算子\(\nabla\)を模擬した派生型が含まれる．
!>ただし，演算子としての機能は有しておらず，
!>移流項を`u.dot.nabla`と表現するための擬似的な型である．
!>
module grid_uniform_staggered_op_custom_nabla
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_uGrad
    implicit none
    private

    !>ナブラ演算子\(\nabla\)を模擬した派生型．
    !>
    !>演算子としての機能は有さない擬似的な空の型．
    type, public :: nabla_type
    end type nabla_type

    type(nabla_type), public :: nabla
        !! ナブラ演算子\(\nabla\)
end module grid_uniform_staggered_op_custom_nabla

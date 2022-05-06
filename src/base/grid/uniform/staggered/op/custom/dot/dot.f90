!| Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!に関する手続を提供する．
!
!手続には，移流速度\boldsymbol{u}とナブラ演算子\(\nabla\)の内積を
!模擬する手続が含まれる．
!
!また，移流項を`u.dot.nabla`と表現することを実現するための
!ユーザ定義演算子`.dot.`を定義するインタフェースも含まれる．
module grid_uniform_staggered_op_custom_dot
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_vector_2d
    use :: grid_uniform_staggered_op_custom_uGrad
    use :: grid_uniform_staggered_op_custom_nabla, only:nabla_type
    implicit none
    private
    public :: operator(.dot.)

    !| ユーザ定義演算子`.dot.`を定義するインタフェース
    interface operator(.dot.)
        procedure :: dot_u_nabla
    end interface

contains
    !| 移流速度とナブラ演算子の内積を計算し，
    !移流速度を反映した非線形演算子`u_grad_type`を返す．
    function dot_u_nabla(u, nabla) result(u_grad)
        implicit none
        !&<
        class(vector_2d_type)   , intent(in) :: u
            !! 移流速度
        type(nabla_type)        , intent(in) :: nabla
            !! ナブラ演算子
        !&>
        type(u_grad_type) :: u_grad
            !! 非線形演算子\((\boldsymbol{u}\cdot\nabla)\)

        ! 移流速度を複製
        u_grad%u = u

        ! 変数の未使用警告の抑制
        if (same_type_as(nabla, nabla)) continue
    end function dot_u_nabla

end module grid_uniform_staggered_op_custom_dot

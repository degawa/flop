!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，動粘度を取り扱う派生型が含まれる．
!>変数には，動粘度を取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型から動粘度を取り出す演算子に渡すための
!>擬似的な型である．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>動粘度を取り扱う派生型の実体名は`kinetic_viscosity`としたかったが，
!>コンストラクタと重複し，コンパイルはできるが参照できない．
!>また，併用することを前提としている`.value.`演算子についても，
!>名前を`.value_of.`とできなかったため，`of_`を変数名に付けることとした．
!>@endnote
!>
!>手続には，動粘度を取り扱う派生型のコンストラクタが含まれる．
!>
module incompressible_op_vars_kineticViscosity
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: kinetic_viscosity

    !>動粘度を取り扱う派生型．
    type, public :: kinetic_viscosity_type
        real(real64), private :: kinetic_viscosity = 0d0
            !! 動粘度の値
    contains
        procedure, public, pass :: get_kinetic_viscosity
        !* 動粘度を返却
    end type kinetic_viscosity_type

    type(kinetic_viscosity_type), public, parameter :: &
        of_kinetic_viscosity = kinetic_viscosity_type()
        !! 演算子に動粘度を認識させるための変数

    !>コンストラクタを`kinetic_viscosity`と呼ぶためのインタフェース．
    interface kinetic_viscosity
        procedure :: construct_kinetic_viscosity
    end interface

contains
    !>動粘度を取り扱う型のコンストラクタ．
    !>動粘度を取り扱う型を返す．
    function construct_kinetic_viscosity(kinetic_visc) result(new_kvisc)
        implicit none

        real(real64), intent(in) :: kinetic_visc
            !! 動粘度の値
        type(kinetic_viscosity_type) :: new_kvisc
            !! 動粘度

        new_kvisc%kinetic_viscosity = kinetic_visc
    end function construct_kinetic_viscosity

    !>動粘度を返す．
    function get_kinetic_viscosity(this) result(kinetic_viscosity)
        implicit none
        class(kinetic_viscosity_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: kinetic_viscosity
            !! 動粘度

        kinetic_viscosity = this%kinetic_viscosity
    end function get_kinetic_viscosity
end module incompressible_op_vars_kineticViscosity

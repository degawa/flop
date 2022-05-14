!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，動粘度を取り扱う派生型が含まれる．
!>変数には，動粘度を取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型から動粘度を取り出す演算子に渡すための
!>擬似的な型である．
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
module incompressible_op_var_kineticViscosity
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: kinetic_viscosity

    !>動粘度を取り扱う派生型．
    type, public :: kinetic_viscosity_type
        real(real64), public :: kinetic_viscosity
            !! 動粘度の値
    end type kinetic_viscosity_type

    type(kinetic_viscosity_type), public :: of_kinetic_viscosity
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
end module incompressible_op_var_kineticViscosity

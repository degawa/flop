!>非圧縮性流れの数値計算の安定化に関係する定数を提供する．
!>
!>定数には，移流に関する無次元数の上限，拡散に関する無次元数の上限，
!>Penalizationに関する無次元数の上限が含まれる．．
!>
module incompressible_vars_stability_specification
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    real(real64), public, parameter :: Courant_number_stability_limit = 1d0
        !! 移流に関する無次元数（クーラン数）の上限
    real(real64), public, parameter :: Diffusion_number_stability_limit = 0.5d0
        !! 拡散に関する無次元数（拡散数）の上限
    real(real64), public, parameter :: Penalization_number_stability_limit = 1d0
        !! Penalizationに関する無次元数の上限<br>
        !! @note Penalization numberは仮の名前
end module incompressible_vars_stability_specification

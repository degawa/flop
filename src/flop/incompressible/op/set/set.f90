!>非圧縮性流れの特徴量に関係する手続を提供する．
!>
!>手続には，特徴量を管理する派生型の成分を設定する手続と，
!>安定条件と計算時間間隔を管理する派生型の成分を設定する手続が含まれる．
!>
module incompressible_op_set
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: operator(.set.)

    !>ユーザ定義演算子`.set.`を定義するインタフェース
    interface operator(.set.)
        procedure :: set_char_len
        procedure :: set_char_velo
        procedure :: set_kinetic_viscosity
        procedure :: set_Reynolds_number
        procedure :: set_time_interval_stability_condition
    end interface

contains
    !>特徴量に代表長さの値を反映して返す．
    function set_char_len(characteristics, length) result(new_char)
        use :: incompressible_vars_characteristics
        use :: incompressible_op_vars_characteristicLength
        implicit none
        !&<
        class(characteristics_type)         , intent(in) :: characteristics
            !! 特徴量
        class(characteristic_length_type)   , intent(in) :: length
            !! 代表長さ
        !&>
        type(characteristics_type) :: new_char
            !! 特徴量

        new_char = characteristics
        call new_char%set_length(length%get_length())
    end function set_char_len

    !>特徴量に代表速度の値を反映して返す．
    function set_char_velo(characteristics, velocity) result(new_char)
        use :: incompressible_vars_characteristics
        use :: incompressible_op_vars_characteristicVelocity
        implicit none
        !&<
        class(characteristics_type)         , intent(in) :: characteristics
            !! 特徴量
        class(characteristic_velocity_type) , intent(in) :: velocity
            !! 代表速度
        !&>
        type(characteristics_type) :: new_char
            !! 特徴量

        new_char = characteristics
        call new_char%set_velocity(velocity%get_velocity())
    end function set_char_velo

    !>特徴量に動粘度の値を反映して返す．
    function set_kinetic_viscosity(characteristics, kinetic_visc) result(new_char)
        use :: incompressible_vars_characteristics
        use :: incompressible_op_vars_kineticViscosity
        implicit none
        !&<
        class(characteristics_type)     , intent(in) :: characteristics
            !! 特徴量
        class(kinetic_viscosity_type)   , intent(in) :: kinetic_visc
            !! 動粘度
        !&>
        type(characteristics_type) :: new_char
            !! 特徴量

        new_char = characteristics
        call new_char%set_kinetic_viscosity(kinetic_visc%get_kinetic_viscosity())
    end function set_kinetic_viscosity

    !>特徴量にReynolds数の値を反映して返す．
    function set_Reynolds_number(characteristics, Re) result(new_char)
        use :: incompressible_vars_characteristics
        use :: incompressible_op_vars_ReynoldsNumber
        implicit none
        !&<
        class(characteristics_type) , intent(in) :: characteristics
            !! 特徴量
        class(Reynolds_number_type) , intent(in) :: Re
            !! Reynolds数
        !&>
        type(characteristics_type) :: new_char
            !! 特徴量

        new_char = characteristics
        call new_char%set_Reynolds_number(Re%get_Reynolds_number())
    end function set_Reynolds_number

    !>複数の安定条件を設定して返す．
    function set_time_interval_stability_condition &
        (stability_conditions, time_stabilizer) result(new_stab_conds)
        use :: incompressible_vars_stability_conditions
        use :: incompressible_op_vars_stabilizer_time
        use :: incompressible_op_vars_stabilizer_time_advection
        use :: incompressible_op_vars_stabilizer_time_diffusion
        implicit none
        !&<
        type(stability_conditions_type), intent(in) :: stability_conditions
            !! 複数の安定条件
        class(time_stabilizer_atype)   , intent(in) :: time_stabilizer
            !! 個々の安定条件
        !&>
        type(stability_conditions_type), allocatable :: new_stab_conds
            !! 複数の安定条件

        allocate (new_stab_conds, source=stability_conditions)

        select type (time_stabilizer)
        type is (advection_stabilizer_type)
            ! 移流の安定条件
            if (.not. allocated(new_stab_conds%Advection_stabilizer)) &
                allocate (new_stab_conds%Advection_stabilizer, &
                          source=time_stabilizer)

        type is (diffusion_stabilizer_type)
            ! 拡散の安定条件
            if (.not. allocated(new_stab_conds%Diffusion_stabilizer)) &
                allocate (new_stab_conds%Diffusion_stabilizer, &
                          source=time_stabilizer)

        type is (penalization_stabilizer_type)
            ! Penalizationの安定条件
            if (.not. allocated(new_stab_conds%Penalization_stabilizer)) &
                allocate (new_stab_conds%Penalization_stabilizer, &
                          source=time_stabilizer)

        class default
            write (error_unit, *) "error: unsupported stabilize condition"
        end select
    end function set_time_interval_stability_condition
end module incompressible_op_set

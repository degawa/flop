!>非圧縮性流れにおける特徴量に関係する派生型や定数を提供する．
!>
!>派生型には，特徴量の値を設定・管理する派生型が含まれる．
!>
!>定数には，特徴量に関わる変数に付与した定数が含まれる．
!>
module incompressible_vars_characteristics
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    enum, bind(c)
        enumerator :: id_Re = 1
            !! Reynolds数の識別番号
        enumerator :: id_velocity
            !! 代表速度の識別番号
        enumerator :: id_length
            !! 代表長さの識別番号
        enumerator :: id_kinetic_viscosity
            !! 動粘度の識別番号
    end enum

    !>レイノルズ数に関係する特徴量の識別番号をまとめた配列．
    !>**昇順で並べる．**
    integer(int32), private, parameter :: &
        Reynolds_number_related_variable_ids(*) = &
        [id_Re, id_velocity, id_length, id_kinetic_viscosity]

    !>非圧縮性流れの特徴量を設定・管理する派生型．
    type, public :: characteristics_type
        real(real64), private :: Reynolds_number
            !! レイノルズ数
        real(real64), private :: velocity
            !! 代表速度
        real(real64), private :: length
            !! 代表長さ
        real(real64), private :: kinetic_viscosity
            !! 動粘度

        logical, private :: is_Reynolds_number_set = .false.
            !! レイノルズ数が設定されているかの定数
        logical, private :: is_velocity_set = .false.
            !! 代表速度が設定されているかの定数
        logical, private :: is_length_set = .false.
            !! 代表長さが設定されているかの定数
        logical, private :: is_kinetic_viscosity_set = .false.
            !! 動粘度が設定されているかの定数
    contains
        procedure, public, pass :: set_Reynolds_number
        !* レイノルズ数を設定
        procedure, public, pass :: set_velocity
        !* 代表速度を設定
        procedure, public, pass :: set_length
        !* 代表長さを設定
        procedure, public, pass :: set_kinetic_viscosity
        !* 動粘度を設定
        procedure, public, pass :: get_Reynolds_number
        !* レイノルズ数を返却
        procedure, public, pass :: get_velocity
        !* 代表速度を返却
        procedure, public, pass :: get_length
        !* 代表長さを返却
        procedure, public, pass :: get_kinetic_viscosity
        !* 動粘度を返却
        procedure, private, pass :: check_Reynolds_number_consistency
        !* レイノルズ数に関連する特徴量の整合性を確認
    end type characteristics_type

contains
    !>レイノルズ数を設定する．
    subroutine set_Reynolds_number(this, Re)
        implicit none
        !&<
        class(characteristics_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64)                , intent(in)    :: Re
            !! レイノルズ数
        !&>

        this%Reynolds_number = Re
        this%is_Reynolds_number_set = .true.

        call this%check_Reynolds_number_consistency()
    end subroutine set_Reynolds_number

    !>代表速度を設定する．
    subroutine set_velocity(this, velocity)
        implicit none
        !&<
        class(characteristics_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64)                , intent(in)    :: velocity
            !! 代表速度
        !&>

        this%velocity = velocity
        this%is_velocity_set = .true.

        call this%check_Reynolds_number_consistency()
    end subroutine set_velocity

    !>代表長さを設定する．
    subroutine set_length(this, length)
        implicit none
        !&<
        class(characteristics_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64)                , intent(in)    :: length
            !! 代表長さ
        !&>

        this%length = length
        this%is_length_set = .true.

        call this%check_Reynolds_number_consistency()
    end subroutine set_length

    !>動粘度を設定する．
    subroutine set_kinetic_viscosity(this, kinetic_viscosity)
        implicit none
        !&<
        class(characteristics_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64)                , intent(in)    :: kinetic_viscosity
            !! 動粘度
        !&>

        this%kinetic_viscosity = kinetic_viscosity
        this%is_kinetic_viscosity_set = .true.

        call this%check_Reynolds_number_consistency()
    end subroutine set_kinetic_viscosity

    !>レイノルズ数を返す．
    function get_Reynolds_number(this) result(Re)
        implicit none

        class(characteristics_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: Re
            !! レイノルズ数

        Re = this%Reynolds_number
    end function get_Reynolds_number

    !>代表速度を返す．
    function get_velocity(this) result(ch_velo)
        implicit none

        class(characteristics_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: ch_velo
            !! 代表速度

        ch_velo = this%velocity
    end function get_velocity

    !>代表長さを返す．
    function get_length(this) result(ch_len)
        implicit none

        class(characteristics_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: ch_len
            !! 代表長さ

        ch_len = this%length
    end function get_length

    !>動粘度を返す．
    function get_kinetic_viscosity(this) result(kvisc)
        implicit none

        class(characteristics_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: kvisc
            !! 動粘度

        kvisc = this%kinetic_viscosity
    end function get_kinetic_viscosity

    !>レイノルズ数に関係する特徴量の整合性を確認し，
    !>整合性がとれていない場合は動粘度を変化させる．
    subroutine check_Reynolds_number_consistency(this)
        use :: fluid_nonDimensionalNumber_Reynolds
        implicit none

        class(characteristics_type), intent(inout) :: this
            !! 当該実体仮引数

        ! レイノルズ数に関係する変数が設定されているかを配列にまとめる．
        ! 順番は，特徴量の識別番号をまとめた配列で並べた変数の順序と同じとする．
        logical :: exists(size(Reynolds_number_related_variable_ids))
        exists = [this%is_Reynolds_number_set, &
                  this%is_velocity_set, &
                  this%is_length_set, &
                  this%is_kinetic_viscosity_set]

        ! 設定されている変数の数を数えて，数に応じて対応を変える．
        select case (count(exists))
        case (:size(Reynolds_number_related_variable_ids) - 2)
            ! 0,1,2個の場合は，後々設定されることを期待して何もしない．
            ! コンストラクタ内から呼ぶ場合は，ここにエラー処理を追加する．

        case (size(Reynolds_number_related_variable_ids) - 1)
            ! 設定されていない変数を計算する．
            block
                integer(int32), allocatable :: nopass_var_id(:)
                nopass_var_id = pack(Reynolds_number_related_variable_ids, mask=.not. exists)

                select case (nopass_var_id(1))
                case (id_Re)
                    this%Reynolds_number &
                        = Reynolds__compute_Reynolds_number &
                          (this%velocity, this%length, this%kinetic_viscosity)
                case (id_velocity)
                    this%velocity &
                        = Reynolds__compute_velocity &
                          (this%Reynolds_number, this%length, this%kinetic_viscosity)
                case (id_length)
                    this%length &
                        = Reynolds__compute_length &
                          (this%Reynolds_number, this%velocity, this%kinetic_viscosity)
                case (id_kinetic_viscosity)
                    this%kinetic_viscosity &
                        = Reynolds__compute_kinetic_viscosity &
                          (this%Reynolds_number, this%velocity, this%length)
                end select
            end block

        case (size(Reynolds_number_related_variable_ids))
            ! 全ての変数が設定されていても，
            ! Reynolds数，代表速度，代表長さ，動粘度の値に
            ! 不整合がある可能性を排除できないので，
            ! 動粘度の値で不整合を吸収する．
            this%kinetic_viscosity &
                = Reynolds__compute_kinetic_viscosity &
                  (this%Reynolds_number, this%velocity, this%length)
        end select
    end subroutine check_Reynolds_number_consistency
end module incompressible_vars_characteristics

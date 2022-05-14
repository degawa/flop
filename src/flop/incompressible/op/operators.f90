!>非圧縮性流れの条件設定に関するオペレータを一括して`use`するためのモジュール
module incompressible_op
    use :: incompressible_op_var_characteristicLength
    use :: incompressible_op_var_characteristicVelocity
    use :: incompressible_op_var_kineticViscosity
    use :: incompressible_op_var_ReynoldsNumber
    use :: incompressible_op_var_stabilizer_time_advection
    use :: incompressible_op_var_stabilizer_time_diffusion
    use :: incompressible_op_set
    use :: incompressible_op_value
    use :: incompressible_op_by
    use :: incompressible_op_stabilize
    public
end module incompressible_op

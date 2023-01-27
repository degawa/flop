!>非圧縮性流れの条件設定に関するオペレータを一括して`use`するためのモジュール
module incompressible_op
    use :: incompressible_op_vars_characteristicLength
    use :: incompressible_op_vars_characteristicVelocity
    use :: incompressible_op_vars_kineticViscosity
    use :: incompressible_op_vars_ReynoldsNumber
    use :: incompressible_op_vars_stabilizer_time_advection
    use :: incompressible_op_vars_stabilizer_time_diffusion
    use :: incompressible_op_vars_stabilizer_time_penalization
    use :: incompressible_op_vars_stabilizer_characteristics_reluctivity
    use :: incompressible_op_set
    use :: incompressible_op_value
    use :: incompressible_op_by
    use :: incompressible_op_stabilize
    public
end module incompressible_op

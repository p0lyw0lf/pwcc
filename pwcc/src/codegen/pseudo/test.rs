use super::*;

#[test]
fn function_with_arg() {
    let arg0 = tacky::Temporary(0);
    let f = tacky::Function {
        name: Identifier("function_with_arg".to_string()),
        global: false,
        args: vec![arg0],
        body: tacky::Instructions(vec![tacky::Instruction::Return {
            val: tacky::Val::Var(arg0),
        }]),
    };

    let f: Function<State> = f.into();
    assert_eq!(
        Function {
            name: Identifier("function_with_arg".to_string()),
            global: false,
            instructions: Instructions(vec![
                Instruction::Mov {
                    src: Operand::Location(Location(Location::Concrete(hardware::Location::Reg(
                        hardware::Reg::DI
                    )))),
                    dst: Location(Location::Pseudo(0))
                },
                Instruction::Mov {
                    src: Operand::Location(Location(Location::Pseudo(0))),
                    dst: Location(Location::Concrete(hardware::Location::Reg(
                        hardware::Reg::AX
                    )))
                },
                Instruction::Ret
            ]),
        },
        f
    );
}

#[test]
fn function_with_call() {
    let f = tacky::Function {
        name: Identifier("function_with_call".to_string()),
        global: false,
        args: vec![],
        body: tacky::Instructions(vec![
            tacky::Instruction::Call {
                name: Identifier("called_function".to_string()),
                args: vec![tacky::Val::Constant(2)],
                dst: tacky::Temporary(0),
            },
            tacky::Instruction::Return {
                val: tacky::Val::Var(tacky::Temporary(0)),
            },
        ]),
    };

    let f: Function<State> = f.into();
    assert_eq!(
        Function {
            name: Identifier("function_with_call".to_string()),
            global: false,
            instructions: Instructions(vec![
                Instruction::Mov {
                    src: Operand::Imm(2),
                    dst: Location(Location::Concrete(hardware::Location::Reg(
                        hardware::Reg::DI
                    )))
                },
                Instruction::Call(Identifier("called_function".to_string())),
                Instruction::Mov {
                    src: Operand::Location(Location(Location::Concrete(hardware::Location::Reg(
                        hardware::Reg::AX
                    )))),
                    dst: Location(Location::Pseudo(0))
                },
                Instruction::Mov {
                    src: Operand::Location(Location(Location::Pseudo(0))),
                    dst: Location(Location::Concrete(hardware::Location::Reg(
                        hardware::Reg::AX
                    )))
                },
                Instruction::Ret
            ]),
        },
        f
    );
}

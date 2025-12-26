use std::fmt::Display;

use super::*;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in self.declarations.iter() {
            writeln!(f, "{decl}")?;
        }
        Ok(())
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => write!(f, "{function}"),
            Self::StaticVariable(variable) => write!(f, "{variable}"),
        }
    }
}

impl Display for StaticVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            if self.global { "global" } else { "local " },
            self.name,
            self.initial_value
        )
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} (",
            if self.global { "global" } else { "local " },
            self.name.0
        )?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        writeln!(f, "):")?;
        for instruction in self.body.0.iter() {
            writeln!(f, "\t{}", instruction)?;
        }

        Ok(())
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in self.0.iter() {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return { val } => write!(f, "return {val}"),
            Instruction::Unary { op, src, dst } => write!(f, "{dst} = {op} {src}"),
            Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => write!(f, "{dst} = {src1} {op} {src2}"),
            Instruction::Copy { src, dst } => write!(f, "{dst} = {src}"),
            Instruction::Jump { target } => write!(f, "jmp {target}"),
            Instruction::JumpIfZero { condition, target } => write!(f, "jz {target} {condition}"),
            Instruction::JumpIfNotZero { condition, target } => {
                write!(f, "jnz {target} {condition}")
            }
            Instruction::Label(identifier) => write!(f, "{identifier}:"),
            Instruction::Call { name, args, dst } => {
                write!(f, "{dst} = {name} (")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Complement => "~",
                UnaryOp::Negate => "-",
                UnaryOp::Not => "!",
            }
        )
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Subtract => "-",
                BinaryOp::Multiply => "*",
                BinaryOp::Divide => "/",
                BinaryOp::Remainder => "%",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitXor => "^",
                BinaryOp::BitOr => "|",
                BinaryOp::BitLeftShift => "<<",
                BinaryOp::BitRightShift => ">>",
                BinaryOp::Equal => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::LessThan => "<",
                BinaryOp::LessThanEqual => "<=",
                BinaryOp::GreaterThan => ">",
                BinaryOp::GreaterThanEqual => ">=",
            }
        )
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(i) => write!(f, "${i}"),
            Self::Var(t) => write!(f, "{t}"),
            Self::Data(d) => write!(f, "*{d}"),
        }
    }
}

impl Display for Temporary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

use core::fmt::Display;
use core::fmt::Formatter;

use crate::codegen::*;

impl Display for Program<hardware::Pass> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        #[cfg(target_os = "linux")]
        {
            writeln!(f, "\t.section .note.GNU-stack,\"\",@progbits")?;
        }
        Ok(())
    }
}

impl Display for Function<hardware::Pass> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        writeln!(f, "\tpushq\t%rbp")?;
        writeln!(f, "\tmovq\t%rsp, %rbp")?;
        for instruction in &self.instructions.0 {
            write!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl Display for Instruction<hardware::Location> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            Mov { src, dst } => writeln!(f, "\tmovl\t{src}, {dst}"),
            Unary { op, dst } => writeln!(f, "\t{op}\t{dst}"),
            AllocateStack { amount } => writeln!(f, "\tsubq\t${amount}, %rsp"),
            Ret => {
                writeln!(f, "\tmovq\t%rbp, %rsp")?;
                writeln!(f, "\tpopq\t%rbp")?;
                writeln!(f, "\tret")?;
                Ok(())
            }
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use UnaryOp::*;
        match self {
            Neg => write!(f, "negl"),
            Not => write!(f, "notl"),
        }
    }
}

impl Display for Operand<hardware::Location> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Operand::*;
        match self {
            Imm(i) => write!(f, "${i}"),
            Location(l) => write!(f, "{l}"),
        }
    }
}

impl Display for hardware::Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use hardware::Location::*;
        match self {
            Reg(reg) => write!(f, "{reg}"),
            Stack(i) => write!(f, "-{i}(%rbp)"),
        }
    }
}

impl Display for hardware::Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use hardware::Reg::*;
        match self {
            AX => write!(f, "%eax"),
            R10 => write!(f, "%r10d"),
        }
    }
}

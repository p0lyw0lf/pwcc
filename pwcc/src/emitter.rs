use core::fmt::Display;
use core::fmt::Formatter;

use crate::codegen::*;
use crate::tacky::Identifier;

impl Display for Program<hardware::Pass> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function)?;
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

impl Display for Instruction<hardware::Pass> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            Mov { src, dst } => writeln!(f, "\tmovl\t{src}, {dst}"),
            Unary { op, dst } => writeln!(f, "\t{op}\t{dst}"),
            Binary { op, src, dst } => writeln!(f, "\t{op}\t{src}, {dst}"),
            Idiv { denom } => writeln!(f, "\tidivl\t{denom}"),
            Cdq => writeln!(f, "\tcdq"),
            AllocateStack { amount } => writeln!(f, "\tsubq\t${amount}, %rsp"),
            Ret => {
                writeln!(f, "\tmovq\t%rbp, %rsp")?;
                writeln!(f, "\tpopq\t%rbp")?;
                writeln!(f, "\tret")?;
                Ok(())
            }
            Cmp { left, right } => writeln!(f, "\tcmpl\t{right}, {left}"),
            Jmp(label) => writeln!(f, "\tjmp\t.L{label}"),
            JmpCC(cc, label) => writeln!(f, "\tj{cc}\t.L{label}"),
            SetCC(cc, dst) => writeln!(f, "\tset{cc}\t{dst:1}"),
            Label(label) => writeln!(f, ".L{label}:"),
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        match self {
            Add => write!(f, "addl"),
            Sub => write!(f, "subl"),
            Mult => write!(f, "imull"),
            And => write!(f, "andl"),
            Or => write!(f, "orl"),
            Xor => write!(f, "xorl"),
            SAL => write!(f, "sall"),
            SAR => write!(f, "sarl"),
        }
    }
}

impl Display for CondCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use CondCode::*;
        match self {
            E => write!(f, "e"),
            NE => write!(f, "ne"),
            G => write!(f, "g"),
            GE => write!(f, "ge"),
            L => write!(f, "l"),
            LE => write!(f, "le"),
        }
    }
}

impl Display for Operand<hardware::Pass> {
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
        if f.width() == Some(1) {
            match self {
                AX => write!(f, "%al"),
                CX => write!(f, "%cl"),
                DX => write!(f, "%dl"),
                R10 => write!(f, "%r10b"),
                R11 => write!(f, "%r11b"),
            }
        } else {
            match self {
                AX => write!(f, "%eax"),
                CX => write!(f, "%ecx"),
                DX => write!(f, "%edx"),
                R10 => write!(f, "%r10d"),
                R11 => write!(f, "%r11d"),
            }
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

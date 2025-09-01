use core::fmt::Display;
use core::fmt::Formatter;

use crate::codegen::*;
use crate::tacky::Identifier;

impl Display for Program<hardware::Pass> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
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
            DeallocateStack { amount } => todo!(),
            Push(operand) => todo!(),
            Call(identifier) => todo!(),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use UnaryOp::*;
        f.write_str(match self {
            Neg => "negl",
            Not => "notl",
        })
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        f.write_str(match self {
            Add => "addl",
            Sub => "subl",
            Mult => "imull",
            And => "andl",
            Or => "orl",
            Xor => "xorl",
            SAL => "sall",
            SAR => "sarl",
        })
    }
}

impl Display for CondCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use CondCode::*;
        f.write_str(match self {
            E => "e",
            NE => "ne",
            G => "g",
            GE => "ge",
            L => "l",
            LE => "le",
        })
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
        f.write_str(if f.width() == Some(1) {
            match self {
                AX => "%al",
                CX => "%cl",
                DX => "%dl",
                DI => "%dil",
                SI => "%sil",
                R8 => "%r8b",
                R9 => "%r9b",
                R10 => "%r10b",
                R11 => "%r11b",
            }
        } else {
            match self {
                AX => "%eax",
                CX => "%ecx",
                DX => "%edx",
                DI => "%edi",
                SI => "%esi",
                R8 => "%r8d",
                R9 => "%r9d",
                R10 => "%r10d",
                R11 => "%r11d",
            }
        })
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

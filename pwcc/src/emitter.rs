use core::fmt::Display;
use core::fmt::Formatter;

use crate::codegen::*;
use crate::semantic::SymbolTable;
use crate::tacky::Identifier;

pub fn emit(
    mut f: impl std::io::Write,
    program: &Program<hardware::Pass>,
    symbol_table: &SymbolTable,
) -> std::io::Result<()> {
    write!(
        f,
        "{}",
        WithSymbolTable {
            symbol_table,
            value: program
        }
    )
}

struct WithSymbolTable<'a, T> {
    symbol_table: &'a SymbolTable,
    value: &'a T,
}

impl<'a, T> WithSymbolTable<'a, T> {
    fn wrap<'b, O>(&'b self, value: &'b O) -> WithSymbolTable<'b, O> {
        WithSymbolTable {
            symbol_table: self.symbol_table,
            value,
        }
    }
}

impl Display for WithSymbolTable<'_, Program<hardware::Pass>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for function in &self.value.functions {
            write!(f, "{}", self.wrap(function))?;
        }
        #[cfg(target_os = "linux")]
        {
            writeln!(f, "\t.section .note.GNU-stack,\"\",@progbits")?;
        }
        Ok(())
    }
}

impl Display for WithSymbolTable<'_, Function<hardware::Pass>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let value = self.value;
        writeln!(f, "\t.globl {}", value.name)?;
        writeln!(f, "{}:", value.name)?;
        writeln!(f, "\tpushq\t%rbp")?;
        writeln!(f, "\tmovq\t%rsp, %rbp")?;
        for instruction in &value.instructions.0 {
            write!(f, "{}", self.wrap(instruction))?;
        }
        Ok(())
    }
}

impl Display for WithSymbolTable<'_, Instruction<hardware::Pass>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match &self.value {
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
            DeallocateStack { amount } => writeln!(f, "\taddq\t${amount}, %rsp"),
            Push(operand) => writeln!(f, "\tpushq\t{operand:8}"),
            Call(identifier) => {
                #[cfg(target_os = "macos")]
                {
                    writeln!(f, "\tcall\t_{identifier}")
                }
                #[cfg(target_os = "linux")]
                if self.symbol_table.is_internal_symbol(&identifier.0) {
                    writeln!(f, "\tcall\t{identifier}")
                } else {
                    writeln!(f, "\tcall\t{identifier}@PLT")
                }
                #[cfg(not(any(target_os = "macos", target_os = "linux")))]
                {
                    panic!("function calls not supported on this OS")
                }
            }
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
        let w = f.width().unwrap_or(4);
        match self {
            Imm(i) => write!(f, "${i}"),
            Location(l) => write!(f, "{:w$}", l),
        }
    }
}

impl Display for hardware::Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use hardware::Location::*;
        let w = f.width().unwrap_or(4);
        match self {
            Reg(reg) => write!(f, "{:w$}", reg),
            Stack(i) => write!(f, "{i}(%rbp)"),
        }
    }
}

impl Display for hardware::Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use hardware::Reg::*;
        f.write_str(
            match (
                self,
                f.width().expect("register display must include width"),
            ) {
                (AX, 1) => "%al",
                (CX, 1) => "%cl",
                (DX, 1) => "%dl",
                (DI, 1) => "%dil",
                (SI, 1) => "%sil",
                (R8, 1) => "%r8b",
                (R9, 1) => "%r9b",
                (R10, 1) => "%r10b",
                (R11, 1) => "%r11b",
                (AX, 4) => "%eax",
                (CX, 4) => "%ecx",
                (DX, 4) => "%edx",
                (DI, 4) => "%edi",
                (SI, 4) => "%esi",
                (R8, 4) => "%r8d",
                (R9, 4) => "%r9d",
                (R10, 4) => "%r10d",
                (R11, 4) => "%r11d",
                (AX, 8) => "%rax",
                (CX, 8) => "%rcx",
                (DX, 8) => "%rdx",
                (DI, 8) => "%rdi",
                (SI, 8) => "%rsi",
                (R8, 8) => "%r8",
                (R9, 8) => "%r9",
                (R10, 8) => "%r10",
                (R11, 8) => "%r11",
                (r, w) => panic!("register {r:?} had invalid width {w}"),
            },
        )
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

use core::fmt::Display;
use core::fmt::Formatter;

use crate::codegen::*;

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        #[cfg(target_os = "linux")]
        {
            writeln!(f, "\t.section .note.GNU-stack,\"\",@progbits")?;
        }
        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\t.globl {}", self.name.ident)?;
        writeln!(f, "{}:", self.name.ident)?;
        for instruction in &self.instructions.0 {
            write!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            Mov { src, dst } => writeln!(f, "\tmovl\t{src}, {dst}"),
            Ret => writeln!(f, "\tret"),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Operand::*;
        match self {
            Register => write!(f, "%eax"),
            Imm(i) => write!(f, "${i}"),
        }
    }
}

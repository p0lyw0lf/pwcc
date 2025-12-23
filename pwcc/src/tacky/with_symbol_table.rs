use crate::semantic::type_check::SymbolTable;

/// Helper struct for annotating values with a symbol table, for the purposes of deriving traits on
/// the combination of the two.
pub struct WithSymbolTable<'a, T> {
    pub symbol_table: &'a SymbolTable,
    pub value: T,
}

impl<'a, T> WithSymbolTable<'a, T> {
    pub fn wrap<'b, O>(&'b self, value: O) -> WithSymbolTable<'b, O> {
        WithSymbolTable {
            symbol_table: self.symbol_table,
            value,
        }
    }
}

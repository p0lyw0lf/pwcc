use super::*;
use functional::functor;

// Declaration
// TODO: I really want to generate all possible functor trees automatically...
// I know it's possible (but only with proc macros (or with reflection/introspection)) but those
// are beyond me atm...
functor!(struct Program try for Declaration | { function, .., });
functor!(struct Function try for Declaration | { body, .., name, });
functor!(struct Body try for Declaration | (+0,));
functor!(enum BlockItem try for Declaration | {
    Statement(-stmt,),
    Declaration(+decl,),
});
functor!(type Declaration);

// Exp
functor!(struct Program try for Exp | { function, .., });
functor!(struct Function try for Exp | { body, .., name, });
functor!(struct Body try for Exp | (+0,));
functor!(enum BlockItem try for Exp | {
    Statement(+stmt,),
    Declaration(+decl,),
});
functor!(enum Statement try for Exp | {
    ExpressionStmt(+s,),
    ReturnStmt(+s,),
    NullStmt(-s,),
});
functor!(struct ExpressionStmt try for Exp | { exp, .., });
functor!(struct ReturnStmt try for Exp | { exp, .., });
functor!(struct Declaration try for Exp | { init, .., name, });
functor!(enum Initializer try for Exp | {
    NoInit(-i,),
    ExpressionInit(+i,),
});
functor!(struct ExpressionInit try for Exp | { exp, .., });
functor!(type Exp);

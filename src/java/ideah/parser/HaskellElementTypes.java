package ideah.parser;

import ideah.lexer.HaskellTokenType;

public interface HaskellElementTypes {

    HaskellTokenType MODULE = new HaskellTokenType("module");
    HaskellTokenType INFIX_PREFIX_IDENT = new HaskellTokenType("infix ident or prefix operator");
}

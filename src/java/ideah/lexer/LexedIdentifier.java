package ideah.lexer;

import org.jetbrains.annotations.NotNull;

public final class LexedIdentifier {

    public final boolean isSymbol;
    /**
     * null for no module
     */
    public final String module;
    @NotNull
    public final String text;

    LexedIdentifier(boolean symbol, String module, String text) {
        isSymbol = symbol;
        this.module = module;
        this.text = text;
    }

    public static LexedIdentifier parse(String str) {
        return HaskellLexerImpl.parseIdent(str);
    }

    public static void main(String[] args) {
        LexedIdentifier id = parse("A.B.C");
        System.out.println(id.text);
    }
}

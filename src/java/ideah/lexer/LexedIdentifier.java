package ideah.lexer;

import org.jetbrains.annotations.NotNull;

public final class LexedIdentifier {

    public final HaskellTokenType type;
    /**
     * null for no module
     */
    public final String module;
    @NotNull
    public final String text;

    LexedIdentifier(HaskellTokenType type, String module, @NotNull String text) {
        this.type = type;
        this.module = module;
        this.text = text;
    }

    public static LexedIdentifier parse(String str) {
        return HaskellLexerImpl.parseIdent(str);
    }

    public static void main(String[] args) {
        LexedIdentifier id = parse("A.B.where");
        System.out.println(id.text);
    }
}

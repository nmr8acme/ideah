package ideah.lexer;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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

    @Nullable
    public static LexedIdentifier parse(String str) {
        return HaskellLexerImpl.parseIdent(str);
    }

    @Nullable
    public static LexedIdentifier parseMaybeInfixPrefix(String str) {
        return parse(removeInfixPrefixForm(str));
    }

    public static String removeInfixPrefixForm(String s) {
        int size = s.length() - 1;
        char first = s.charAt(0);
        char last = s.charAt(size);
        if (first == '(' && last == ')' || first == '`' && last == '`') {
            return s.substring(1, size);
        }
        return s;
    }

    public static void main(String[] args) {
        LexedIdentifier id = parse("length");
        System.out.println(id.text);
    }
}

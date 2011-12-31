package ideah.lexer;

public final class Unescaper implements Escaping {

    private final StringBuilder buf = new StringBuilder();
    private boolean error = false;

    private static char unescape(char c) {
        int i = SINGLE_ESCAPES.indexOf(c);
        return i >= 0 ? SINGLE_UNESCAPES.charAt(i) : 0;
    }

    public static char escape(char c) {
        int i = SINGLE_UNESCAPES.indexOf(c);
        return i >= 0 ? SINGLE_ESCAPES.charAt(i) : 0;
    }

    void singleChar(char c) {
        if (c != '&') {
            append(unescape(c));
        }
    }

    private void append(int code) {
        buf.append((char) code);
    }

    private void appendCode(String value, int radix) {
        try {
            int i = Integer.parseInt(value, radix);
            append(i);
        } catch (Exception ex) {
            error = true;
        }
    }

    void octal(String value) {
        appendCode(value, 8);
    }

    void hex(String value) {
        appendCode(value, 16);
    }

    void decimal(String value) {
        appendCode(value, 10);
    }

    void control(char c) {
        int i = (int) c - (int) 'A' + 1;
        append(i);
    }

    void namedControl(int index) {
        append(index);
    }

    void normalChar(char c) {
        buf.append(c);
    }

    public static String unescape(String string) {
        String toParse = "\"" + string + "\"";
        HaskellLexerImpl lexer = new HaskellLexerImpl();
        lexer.init(toParse, 0, toParse.length());
        Unescaper unescaper = new Unescaper();
        HaskellToken token = lexer.string(0, unescaper);
        boolean hasNext = lexer.nextToken() != null;
        if (token == null || token.type != HaskellTokenTypes.STRING || unescaper.error || hasNext) {
            return string;
        } else {
            return unescaper.buf.toString();
        }
    }
}

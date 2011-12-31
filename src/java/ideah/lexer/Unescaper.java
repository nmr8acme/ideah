package ideah.lexer;

public final class Unescaper {

    private final StringBuilder buf = new StringBuilder();
    private boolean error = false;

    private static char unescape(char c) {
        switch (c) {
        case 'a': return 7;
        case 'b': return '\b';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return 11;
        }
        return c;
    }

    public static char escape(char c) {
        switch (c) {
        case 7: return 'a';
        case '\b': return 'b';
        case '\f': return 'f';
        case '\n': return 'n';
        case '\r': return 'r';
        case '\t': return 't';
        case 11: return 'v';
        }
        return 0;
    }

    void singleChar(char c) {
        append(unescape(c));
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
        if (index > ' ') {
            append(127);
        } else {
            append(index);
        }
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

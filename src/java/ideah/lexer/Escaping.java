package ideah.lexer;

import java.util.Arrays;
import java.util.List;

interface Escaping {

    String SINGLE_ESCAPES = "abfnrtv\\\"'";
    String SINGLE_UNESCAPES = "\u0007\b\f\n\r\t\u000B\\\"'";
    List<String> ESCAPES = Arrays.asList(
        "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", // 0-7
        "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",  // 8-15
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", // 16-23
        "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",  // 24-31
        "SP",  "DEL"                                            // 32, 127
    );
}

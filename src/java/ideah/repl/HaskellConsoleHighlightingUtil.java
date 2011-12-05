package ideah.repl;

import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.util.Key;

import java.util.regex.Pattern;

final class HaskellConsoleHighlightingUtil {

    private static final String MODULES = "Prelude"; // todo: other modules!!!
    private static final String PROMPT_ARROW = ">";
    static final String LINE_WITH_PROMPT = MODULES + PROMPT_ARROW + ".*";

    static final Pattern GHCI_PATTERN = Pattern.compile(MODULES + PROMPT_ARROW);

    static void processOutput(LanguageConsoleImpl console, String text, Key attributes) {
        ConsoleViewContentType outputType = ConsoleViewContentType.NORMAL_OUTPUT;
        // todo implement multiple cases for error etc.
        LanguageConsoleImpl.printToConsole(console, text, outputType, null);
    }
}

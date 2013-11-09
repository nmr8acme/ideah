package ideah.compiler;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import ideah.util.LineColRange;

public final class GHCMessage {

    private final LineColRange range;
    private final String errorMessage;
    private final CompilerMessageCategory category;
    private final String fileName;

    GHCMessage(String ghcError) {
        String newLine = LaunchGHC.EOLN;
        int newLineIndex = ghcError.indexOf(newLine);
        fileName = ghcError.substring(0, newLineIndex);

        int nextNewLineIndex = ghcError.indexOf(newLine, newLineIndex + newLine.length());
        String posString = ghcError.substring(newLineIndex + newLine.length(), nextNewLineIndex);
        CompilerMessageCategory cmc;
        if (posString.startsWith("W")) {
            cmc = CompilerMessageCategory.WARNING;
            posString = posString.substring(1);
        } else if (posString.startsWith("E")) {
            cmc = CompilerMessageCategory.ERROR;
            posString = posString.substring(1);
        } else {
            cmc = CompilerMessageCategory.ERROR;
        }
        range = new LineColRange(posString);

        String err = ghcError.substring(nextNewLineIndex + newLine.length());

        // strange bug workaround: error message is shown twice
        if (err.length() % 2 == 0) {
            int half = err.length() / 2;
            String halfS = err.substring(0, half);
            if (err.endsWith(halfS))
                err = halfS;
        }
        errorMessage = err;

        category = cmc;
    }

    GHCMessage(String errorMessage, String fileName) {
        this.range = LineColRange.getFake();
        this.errorMessage = errorMessage;
        this.category = CompilerMessageCategory.ERROR;
        this.fileName = fileName;
    }

    public LineColRange getRange() {
        return range;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public CompilerMessageCategory getCategory() {
        return category;
    }

    public String getFileName() {
        return fileName;
    }
}

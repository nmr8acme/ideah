package ideah.util;

import com.intellij.openapi.editor.LazyRangeMarkerFactory;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiFile;

public final class LineColRange {

    /**
     * 1-based line number
     */
    public final int startLine;
    /**
     * 1-based linme number
     */
    public final int endLine;
    /**
     * 1-based column number
     */
    public final int startColumn;
    /**
     * 1-based column number
     */
    public final int endColumn;

    public LineColRange(int startLine, int endLine, int startColumn, int endColumn) {
        this.startLine = startLine;
        this.endLine = endLine;
        this.startColumn = startColumn;
        this.endColumn = endColumn;
    }

    public LineColRange(String str) {
        String[] lineColumnStrings = str.split("[-:]");
        startLine = parseInt(lineColumnStrings[0]);
        startColumn = parseInt(lineColumnStrings[1]);
        endLine = parseInt(lineColumnStrings[2]);
        endColumn = parseInt(lineColumnStrings[3]);
    }

    private static int parseInt(String str) {
        str = str.trim();
        if ("?".equals(str))
            return 0;
        return Integer.parseInt(str);
    }

    public TextRange getRange(PsiFile file) {
        return new TextRange(getOffset(file, startLine, startColumn), getOffset(file, endLine, endColumn));
    }

    private static int getOffset(PsiFile file, int line, int col) {
        LazyRangeMarkerFactory factory = LazyRangeMarkerFactory.getInstance(file.getProject());
        RangeMarker rangeMarker = factory.createRangeMarker(
            file.getVirtualFile(), Math.max(0, line - 1), Math.max(0, col - 1), false
        );
        return rangeMarker.getStartOffset();
    }

    public String toString() {
        return startLine + ":" + startColumn + " - " + endLine + ":" + endColumn;
    }
}

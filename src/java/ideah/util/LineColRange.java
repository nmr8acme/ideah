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
        int p = str.indexOf('-');
        String start = str.substring(0, p);
        String end = str.substring(p + 1);
        int[] starts = parsePair(start);
        int[] ends = parsePair(end);
        startLine = starts[0];
        startColumn = starts[1];
        endLine = ends[0];
        endColumn = ends[1];
    }

    private static int[] parsePair(String str) {
        str = str.trim();
        if ("?".equals(str))
            return new int[] {0, 0};
        int p = str.indexOf(':');
        String from = str.substring(0, p);
        String to = str.substring(p + 1);
        return new int[] {parseInt(from), parseInt(to)};
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

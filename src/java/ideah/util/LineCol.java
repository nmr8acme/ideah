package ideah.util;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.LazyRangeMarkerFactory;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;

public final class LineCol implements Comparable<LineCol> {

    /**
     * 1-based line number
     */
    public final int line;
    /**
     * 1-based column number
     */
    public final int column;

    public LineCol(int line, int column) {
        this.line = line;
        this.column = column;
    }

    public int getOffset(PsiFile file) {
        return getOffset(file, line, column);
    }

    public static int getOffset(PsiFile psiFile, int line, int col) {
        LazyRangeMarkerFactory factory = LazyRangeMarkerFactory.getInstance(psiFile.getProject());
        RangeMarker rangeMarker = factory.createRangeMarker(
            psiFile.getVirtualFile(), Math.max(0, line - 1), Math.max(0, col - 1), false
        );
        return rangeMarker.getStartOffset();
    }

    public static LineCol fromOffset(PsiFile psiFile, int offset) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        FileDocumentManager fdm = FileDocumentManager.getInstance();
        Document doc = fdm.getCachedDocument(file);
        if (doc == null)
            return null;
        int line = doc.getLineNumber(offset);
        int col = offset - doc.getLineStartOffset(line);
        return new LineCol(line + 1, col + 1);
    }

    @Override
    public String toString() {
        return line + ":" + column;
    }

    public int compareTo(LineCol that) {
        if (this.line < that.line) {
            return -1;
        } else if (this.line > that.line) {
            return 1;
        } else {
            if (this.column < that.column) {
                return -1;
            } else if (this.column > that.column) {
                return 1;
            } else {
                return 0;
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LineCol) {
            LineCol that = (LineCol) obj;
            return this.line == that.line && this.column == that.column;
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return 31 * line + column;
    }
}

package ideah.formatter;

import com.intellij.openapi.util.TextRange;
import ideah.tree.ILocation;
import ideah.tree.IRange;

final class MyRange implements IRange {

    private final TextRange range;

    MyRange(TextRange range) {
        this.range = range;
    }

    public TextRange getRange() {
        return range;
    }

    public ILocation getStart() {
        return new MyLocation(range.getStartOffset());
    }

    public ILocation getEnd() {
        return new MyLocation(range.getEndOffset());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MyRange) {
            MyRange that = (MyRange) obj;
            return this.range.equals(that.range);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return range.hashCode();
    }

    @Override
    public String toString() {
        return range.toString();
    }
}

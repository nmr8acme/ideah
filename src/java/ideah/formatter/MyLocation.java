package ideah.formatter;

import com.intellij.openapi.util.TextRange;
import ideah.tree.ILocation;

final class MyLocation implements ILocation {

    private final int offset;

    MyLocation(int offset) {
        this.offset = offset;
    }

    static MyRange create(MyLocation start, MyLocation end) {
        return new MyRange(new TextRange(start.offset, end.offset));
    }

    public int compareTo(ILocation o) {
        MyLocation that = (MyLocation) o;
        if (this.offset < that.offset) {
            return -1;
        } else if (this.offset > that.offset) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MyLocation) {
            MyLocation that = (MyLocation) obj;
            return this.offset == that.offset;
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return offset;
    }

    @Override
    public String toString() {
        return String.valueOf(offset);
    }
}

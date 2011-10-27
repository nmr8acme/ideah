package ideah.formatter;

import ideah.util.ILocation;

final class MyLocation implements ILocation {

    private final int offset;

    MyLocation(int offset) {
        this.offset = offset;
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
